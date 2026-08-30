open Ppxlib
open Ast_builder.Default

type level = Trace | Debug | Info | Warn | Error

let level_to_int = function Trace -> 0 | Debug -> 1 | Info -> 2 | Warn -> 3 | Error -> 4

let level_name = function
  | Trace -> "Trace"
  | Debug -> "Debug"
  | Info -> "Info"
  | Warn -> "Warn"
  | Error -> "Error"

let level_of_string ~loc value =
  match String.lowercase_ascii value with
  | "trace" -> Trace
  | "debug" -> Debug
  | "info" -> Info
  | "warn" | "warning" -> Warn
  | "error" -> Error
  | _ -> Location.raise_errorf ~loc "delator: unknown level %S" value

let initial_static_level () =
  match Sys.getenv_opt "DELATOR_STATIC_LEVEL" with
  | None | Some "" -> Trace
  | Some value -> level_of_string ~loc:Location.none value

let static_level = ref (initial_static_level ())

let set_static_level value =
  static_level := level_of_string ~loc:Location.none value

let () =
  Driver.add_arg "--static-level" (Arg.String set_static_level)
    ~doc:"LEVEL statically remove Delator calls below LEVEL"

let survives level = level_to_int level >= level_to_int !static_level

let instrument_attr =
  Attribute.declare_flag "delator.instrument" Attribute.Context.value_binding

let skip_all_attr =
  Attribute.declare_flag "delator.skip_all" Attribute.Context.value_binding

let no_exn_attr =
  Attribute.declare_flag "delator.no_exn_log" Attribute.Context.value_binding

let level_attr =
  Attribute.declare "delator.level" Attribute.Context.value_binding
    Ast_pattern.(single_expr_payload __) Fun.id

let field_attr =
  Attribute.declare "delator.field" Attribute.Context.pattern
    Ast_pattern.(single_expr_payload __) Fun.id

let field_pp_attr =
  Attribute.declare "delator.field.pp" Attribute.Context.pattern
    Ast_pattern.(single_expr_payload __) Fun.id

let skip_attr =
  Attribute.declare_flag "delator.skip" Attribute.Context.pattern

let delator_attribute_name name =
  name = "delator.instrument" || name = "delator.level"
  || name = "delator.skip_all" || name = "delator.no_exn_log"
  || name = "delator.field" || name = "delator.field.pp"
  || name = "delator.skip"

let strip_attributes attributes =
  List.filter (fun attribute -> not (delator_attribute_name attribute.attr_name.txt)) attributes

class strip_pattern_attributes = object
  inherit Ast_traverse.map as super
  method! pattern pattern =
    let pattern = super#pattern pattern in
    { pattern with ppat_attributes = strip_attributes pattern.ppat_attributes }
end

let strip_pattern = (new strip_pattern_attributes)#pattern

let longident ~loc name =
  let txt =
    match String.split_on_char '.' name with
    | [] | [ "" ] -> Location.raise_errorf ~loc "delator: invalid generated path %S" name
    | first :: rest ->
        List.fold_left (fun path component -> Longident.Ldot (path, component))
          (Longident.Lident first) rest
  in
  pexp_ident ~loc { txt; loc }

let runtime_level ~loc level =
  let name = "Delator.Level." ^ level_name level in
  let path =
    match String.split_on_char '.' name with
    | first :: rest ->
        List.fold_left (fun path component -> Longident.Ldot (path, component))
          (Longident.Lident first) rest
    | [] -> assert false
  in
  pexp_construct ~loc { txt = path; loc } None
let module_target ~loc = longident ~loc "__MODULE__"

let field_pair ~loc name value =
  pexp_tuple ~loc [ estring ~loc name; value ]

let parse_log_payload ~loc payload =
  let message, arguments =
    match payload.pexp_desc with
    | Pexp_apply (message, arguments) -> (message, arguments)
    | _ -> (payload, [])
  in
  let message_text =
    match message.pexp_desc with
    | Pexp_constant (Pconst_string (value, _, _)) -> value
    | _ -> Location.raise_errorf ~loc:message.pexp_loc "delator log message must be a string literal"
  in
  let fields =
    List.map
      (fun (label, expression) ->
        match label with
        | Asttypes.Labelled name ->
            let value =
              match expression.pexp_desc with
              | Pexp_ident { txt = Longident.Lident pun; _ } when String.equal pun name ->
                  eapply ~loc (longident ~loc "Delator.Field.string") [ expression ]
              | _ -> expression
            in
            field_pair ~loc name value
        | Asttypes.Nolabel | Asttypes.Optional _ ->
            Location.raise_errorf ~loc:expression.pexp_loc
              "delator log fields must use ~name or ~name:value")
      arguments
  in
  (estring ~loc message_text, elist ~loc fields)

let expand_log ~loc level payload =
  if not (survives level) then eunit ~loc
  else
    let message, fields = parse_log_payload ~loc payload in
    let target = module_target ~loc in
    let enabled =
      pexp_apply ~loc (longident ~loc "Delator.Runtime.is_enabled")
        [ (Labelled "level", runtime_level ~loc level); (Labelled "target", target) ]
    in
    let event =
      pexp_apply ~loc (longident ~loc "Delator.Runtime.event")
        [ (Labelled "target", module_target ~loc);
          (Labelled "level", runtime_level ~loc level);
          (Labelled "msg", message); (Labelled "fields", fields) ]
    in
    pexp_ifthenelse ~loc enabled event (Some (eunit ~loc))

let log_level_of_extension name =
  let prefixes = [ "log."; "delator." ] in
  List.find_map
    (fun prefix ->
      if String.length name > String.length prefix
         && String.sub name 0 (String.length prefix) = prefix
      then Some (String.sub name (String.length prefix) (String.length name - String.length prefix))
      else None)
    prefixes

let payload_expression ~loc = function
  | PStr [ { pstr_desc = Pstr_eval (expression, []); _ } ] -> expression
  | _ -> Location.raise_errorf ~loc "delator log extension expects one expression payload"

let variable_name pattern =
  let found = ref None in
  let iterator =
    object
      inherit Ast_traverse.iter as super
      method! pattern pattern =
        (match (!found, pattern.ppat_desc) with
        | None, Ppat_var name -> found := Some name.txt
        | _ -> ());
        super#pattern pattern
    end
  in
  iterator#pattern pattern;
  !found

let binding_name binding = variable_name binding.pvb_pat

let level_from_attribute expression =
  match expression.pexp_desc with
  | Pexp_ident { txt = Longident.Lident name; _ } -> level_of_string ~loc:expression.pexp_loc name
  | _ -> Location.raise_errorf ~loc:expression.pexp_loc "[@@delator.level] expects a level name"

type parameter_field = { pattern : pattern; field : expression option }

let parameter_field pattern =
  let converter = Attribute.get field_attr pattern in
  let printer = Attribute.get field_pp_attr pattern in
  let skip = Attribute.has_flag skip_attr pattern in
  let pattern = strip_pattern pattern in
  let field =
    match variable_name pattern with
    | None -> None
    | Some _ when skip -> None
    | Some name ->
        let loc = pattern.ppat_loc in
        let value = longident ~loc name in
        let rendered =
          match (converter, printer) with
          | Some converter, None ->
              eapply ~loc (longident ~loc "Delator.Field.string")
                [ eapply ~loc converter [ value ] ]
          | None, Some printer ->
              eapply ~loc (longident ~loc "Delator.Field.pp") [ printer; value ]
          | None, None ->
              eapply ~loc (longident ~loc "Delator.Field.string")
                [ estring ~loc "<opaque>" ]
          | Some _, Some _ ->
              Location.raise_errorf ~loc
                "a parameter cannot have both [@delator.field] and [@delator.field.pp]"
        in
        Some (field_pair ~loc name rendered)
  in
  { pattern; field }

let in_span ~loc ~name ~level ~fields ~log_exn body =
  let fields_thunk = pexp_fun ~loc Nolabel None (punit ~loc) (elist ~loc fields) in
  let body_thunk = pexp_fun ~loc Nolabel None (punit ~loc) body in
  pexp_apply ~loc (longident ~loc "Delator.in_span")
    [ (Labelled "level", runtime_level ~loc level);
      (Labelled "target", module_target ~loc);
      (Labelled "name", estring ~loc name);
      (Labelled "fields", fields_thunk);
      (Labelled "log_exn", ebool ~loc log_exn);
      (Nolabel, body_thunk) ]

let instrument_expression ~name ~level ~skip_all ~log_exn expression =
  let loc = expression.pexp_loc in
  match expression.pexp_desc with
  | Pexp_function (parameters, constraint_, Pfunction_body body) ->
      let name_only =
        List.exists
          (fun parameter ->
            match parameter.pparam_desc with
            | Pparam_newtype _ -> true
            | Pparam_val _ -> false)
          parameters
      in
      let parameters, fields =
        List.fold_right
          (fun parameter (parameters, fields) ->
            match parameter.pparam_desc with
            | Pparam_newtype _ -> (parameter :: parameters, fields)
            | Pparam_val (label, default, pattern) ->
                let converted = parameter_field pattern in
                let parameter =
                  { parameter with
                    pparam_desc = Pparam_val (label, default, converted.pattern) }
                in
                let fields =
                  match converted.field with Some field -> field :: fields | None -> fields
                in
                (parameter :: parameters, fields))
          parameters ([], [])
      in
      let fields = if skip_all || name_only then [] else fields in
      { expression with
        pexp_desc =
          Pexp_function
            (parameters, constraint_, Pfunction_body (in_span ~loc ~name ~level ~fields ~log_exn body)) }
  | Pexp_function (parameters, constraint_, Pfunction_cases (cases, cases_loc, attributes)) ->
      let parameters =
        List.map
          (fun parameter ->
            match parameter.pparam_desc with
            | Pparam_newtype _ -> parameter
            | Pparam_val (label, default, pattern) ->
                { parameter with pparam_desc = Pparam_val (label, default, strip_pattern pattern) })
          parameters
      in
      let cases =
        List.map
          (fun case ->
            { case with pc_rhs = in_span ~loc:case.pc_rhs.pexp_loc ~name ~level ~fields:[] ~log_exn case.pc_rhs })
          cases
      in
      { expression with
        pexp_desc = Pexp_function (parameters, constraint_, Pfunction_cases (cases, cases_loc, attributes)) }
  | _ ->
      in_span ~loc ~name ~level ~fields:[] ~log_exn expression

let consume_binding_attributes binding =
  let instrument = Attribute.has_flag instrument_attr binding in
  let skip_all = Attribute.has_flag skip_all_attr binding in
  let no_exn = Attribute.has_flag no_exn_attr binding in
  let level = Option.map level_from_attribute (Attribute.get level_attr binding) in
  let binding = { binding with pvb_attributes = strip_attributes binding.pvb_attributes } in
  (binding, instrument, skip_all, no_exn, level)

class mapper = object
  inherit Ast_traverse.map as super

  method! expression expression =
    match expression.pexp_desc with
    | Pexp_extension ({ txt = name; loc }, payload) -> (
        match log_level_of_extension name with
        | None -> super#expression expression
        | Some raw_level ->
            let level = level_of_string ~loc raw_level in
            expand_log ~loc:expression.pexp_loc level (payload_expression ~loc payload))
    | _ -> super#expression expression

  method! value_binding binding =
    let binding, instrument, skip_all, no_exn, configured_level =
      consume_binding_attributes binding
    in
    let binding = super#value_binding binding in
    if not instrument then binding
    else
      let level = Option.value ~default:Debug configured_level in
      if not (survives level) then
        { binding with pvb_expr = binding.pvb_expr }
      else
        match binding_name binding with
        | None ->
            Location.raise_errorf ~loc:binding.pvb_loc
              "[@@delator.instrument] requires a named value binding"
        | Some name ->
            { binding with
              pvb_expr =
                instrument_expression ~name ~level ~skip_all ~log_exn:(not no_exn)
                  binding.pvb_expr }
end

let impl structure = (new mapper)#structure structure
