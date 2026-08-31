open Ppxlib
open Ast_builder.Default

type level = Trace | Debug | Info | Warn | Error

let level_to_int = function
  | Trace -> 0
  | Debug -> 1
  | Info -> 2
  | Warn -> 3
  | Error -> 4

let level_name = function
  | Trace -> "trace"
  | Debug -> "debug"
  | Info -> "info"
  | Warn -> "warn"
  | Error -> "error"

let level_of_name = function
  | "trace" -> Some Trace
  | "debug" -> Some Debug
  | "info" -> Some Info
  | "warn" | "warning" -> Some Warn
  | "error" -> Some Error
  | _ -> None

let level_attribute attribute =
  let prefix = "log_value." in
  let name = attribute.attr_name.txt in
  if String.length name <= String.length prefix
     || String.sub name 0 (String.length prefix) <> prefix
  then None
  else
    let suffix =
      String.sub name (String.length prefix)
        (String.length name - String.length prefix)
    in
    match level_of_name suffix with
    | Some level ->
        (match attribute.attr_payload with
        | PStr [] -> ()
        | _ ->
            Location.raise_errorf ~loc:attribute.attr_loc
              "[@%s] does not accept a payload" name);
        Some level
    | None ->
        Location.raise_errorf ~loc:attribute.attr_loc
          "unknown log-value level in [@%s]" name

let split_level_attributes attributes =
  let levels, remaining =
    List.fold_right
      (fun attribute (levels, remaining) ->
        match level_attribute attribute with
        | Some level -> ((level, attribute.attr_loc) :: levels, remaining)
        | None -> (levels, attribute :: remaining))
      attributes ([], [])
  in
  match levels with
  | [] -> (None, remaining)
  | [ (level, _) ] -> (Some level, remaining)
  | (_, loc) :: _ ->
      Location.raise_errorf ~loc
        "exactly one [@log_value.LEVEL] annotation is allowed at a site"

let level_on_attributes attributes = fst (split_level_attributes attributes)

let level_on_expression expression =
  level_on_attributes expression.pexp_attributes

let level_on_pattern pattern = level_on_attributes pattern.ppat_attributes
let level_on_type typ = level_on_attributes typ.ptyp_attributes

let level_on_binding binding =
  level_on_attributes binding.pvb_attributes

let level_on_label label = level_on_attributes label.pld_attributes

let survives ~static_level level = level_to_int level >= static_level

let compatible ~context value =
  level_to_int context <= level_to_int value

let level_article = function Info | Error -> "an" | Trace | Debug | Warn -> "a"

let variable_name pattern =
  match pattern.ppat_desc with
  | Ppat_var name -> Some name.txt
  | Ppat_constraint (nested, _) -> (
      match nested.ppat_desc with Ppat_var name -> Some name.txt | _ -> None)
  | _ -> None

let formal_level pattern =
  let pattern_level = level_on_pattern pattern in
  let type_level =
    match pattern.ppat_desc with
    | Ppat_constraint (_, typ) -> level_on_type typ
    | _ -> None
  in
  match (pattern_level, type_level) with
  | None, level | level, None -> level
  | Some _, Some _ ->
      Location.raise_errorf ~loc:pattern.ppat_loc
        "a formal cannot carry two log-value annotations"

let strip_pattern_formal_level pattern =
  let _, attributes = split_level_attributes pattern.ppat_attributes in
  let pattern = { pattern with ppat_attributes = attributes } in
  match pattern.ppat_desc with
  | Ppat_constraint (nested, typ) ->
      let _, attributes = split_level_attributes typ.ptyp_attributes in
      { pattern with
        ppat_desc =
          Ppat_constraint
            (nested, { typ with ptyp_attributes = attributes }) }
  | _ -> pattern

let names_with_level level pattern =
  let names = ref [] in
  let iterator =
    object
      inherit Ast_traverse.iter as super
      method! pattern pattern =
        (match pattern.ppat_desc with
        | Ppat_var name -> names := (name.txt, level) :: !names
        | _ -> ());
        super#pattern pattern
    end
  in
  iterator#pattern pattern;
  !names

let longident_name = function
  | Longident.Lident name -> Some name
  | Ldot _ | Lapply _ -> None

let final_longident_name path =
  let rec loop = function
    | Longident.Lident name -> name
    | Ldot (_, name) -> name
    | Lapply (_, right) -> loop right
  in
  loop path

let log_level_of_extension name =
  let prefixes = [ "log."; "delator." ] in
  List.find_map
    (fun prefix ->
      if String.length name > String.length prefix
         && String.sub name 0 (String.length prefix) = prefix
      then
        level_of_name
          (String.sub name (String.length prefix)
             (String.length name - String.length prefix))
      else None)
    prefixes

let payload_expression ~loc = function
  | PStr [ { pstr_desc = Pstr_eval (expression, []); _ } ] -> expression
  | _ -> Location.raise_errorf ~loc "delator log extension expects one expression payload"

let has_attribute name attributes =
  List.exists (fun attribute -> String.equal attribute.attr_name.txt name) attributes

let instrumentation_level binding =
  if not (has_attribute "delator.instrument" binding.pvb_attributes) then None
  else
    let configured =
      List.find_map
        (fun attribute ->
          if not (String.equal attribute.attr_name.txt "delator.level") then None
          else
            match attribute.attr_payload with
            | PStr
                [ { pstr_desc =
                      Pstr_eval
                        ({ pexp_desc = Pexp_ident { txt = Lident name; _ }; _ }, []);
                    _ } ] ->
                (match level_of_name (String.lowercase_ascii name) with
                | Some level -> Some level
                | None ->
                    Location.raise_errorf ~loc:attribute.attr_loc
                      "unknown instrumentation level %s" name)
            | _ ->
                Location.raise_errorf ~loc:attribute.attr_loc
                  "[@@delator.level] expects a level name")
        binding.pvb_attributes
    in
    Some (Option.value ~default:Debug configured)

let skipped_pattern pattern =
  has_attribute "delator.skip" pattern.ppat_attributes

let validate_instrumentation_formals binding =
  match instrumentation_level binding with
  | None -> ()
  | Some _ when has_attribute "delator.skip_all" binding.pvb_attributes -> ()
  | Some instrument_level -> (
      match binding.pvb_expr.pexp_desc with
      | Pexp_function (parameters, _, _) ->
          List.iter
            (fun parameter ->
              match parameter.pparam_desc with
              | Pparam_newtype _ -> ()
              | Pparam_val (_, _, pattern) ->
                  (match formal_level pattern with
                  | Some value_level
                    when not (skipped_pattern pattern)
                         && not
                              (compatible ~context:instrument_level value_level)
                    ->
                      Location.raise_errorf ~loc:pattern.ppat_loc
                        "%s log-value parameter would be captured by %s instrumentation; add [@delator.skip] or use compatible instrumentation"
                        (String.capitalize_ascii (level_name value_level))
                        (String.capitalize_ascii (level_name instrument_level))
                  | Some _ | None -> ()))
            parameters
      | _ -> ())

let function_signature expression =
  match expression.pexp_desc with
  | Pexp_function (parameters, _, _) ->
      Some
        (List.filter_map
           (fun parameter ->
             match parameter.pparam_desc with
             | Pparam_newtype _ -> None
             | Pparam_val (label, _, pattern) ->
                 Some (label, formal_level pattern))
           parameters)
  | _ -> None

let with_ref reference value action =
  let saved = !reference in
  reference := value;
  Fun.protect ~finally:(fun () -> reference := saved) action

class validator =
  object (self)
    inherit Ast_traverse.iter as super

    val context : level option ref = ref None
    val bindings : (string * level) list ref = ref []
    val callables : (string * (arg_label * level option) list) list ref = ref []
    val fields : (string * level option) list ref = ref []
    val structural_components = ref true

    method private with_context level action = with_ref context (Some level) action

    method private with_bindings additions action =
      with_ref bindings (additions @ !bindings) action

    method private with_callables additions action =
      with_ref callables (additions @ !callables) action

    method private field_level label =
      let name = final_longident_name label in
      match List.filter_map
              (fun (candidate, level) ->
                if String.equal candidate name then Some level else None)
              !fields
      with
      | [] -> None
      | first :: rest when List.for_all (( = ) first) rest -> Some first
      | _ -> None

    method private validate_field_slot ~loc label annotation =
      let name = final_longident_name label in
      match (self#field_level label, annotation) with
      | Some (Some expected), Some actual when expected = actual -> ()
      | Some (Some expected), Some actual ->
          Location.raise_errorf ~loc
            "field %s expects [@log_value.%s], not [@log_value.%s]"
            name (level_name expected) (level_name actual)
      | Some (Some expected), None ->
          Location.raise_errorf ~loc
            "field %s requires [@log_value.%s] at this site"
            name (level_name expected)
      | Some None, Some actual ->
          Location.raise_errorf ~loc
            "field %s is not level-gated, but this site uses [@log_value.%s]"
            name (level_name actual)
      | Some None, None | None, _ -> ()

    method private validate_field_use ~loc label annotation =
      let name = final_longident_name label in
      match (self#field_level label, annotation) with
      | Some (Some declared), Some used
        when compatible ~context:used declared ->
          ()
      | Some (Some declared), Some used ->
          Location.raise_errorf ~loc
            "field %s is available at %s and cannot be used at %s"
            name (level_name declared) (level_name used)
      | Some (Some declared), None ->
          Location.raise_errorf ~loc
            "field %s requires a compatible [@log_value.LEVEL] use annotation (declared %s)"
            name (level_name declared)
      | Some None, Some used ->
          Location.raise_errorf ~loc
            "field %s is not level-gated, but this use has [@log_value.%s]"
            name (level_name used)
      | Some None, None | None, _ -> ()

    method private validate_call callee arguments =
      match callee.pexp_desc with
      | Pexp_ident { txt = Lident name; _ } -> (
          match List.assoc_opt name !callables with
          | Some formals when List.length formals = List.length arguments ->
              List.iter2
                (fun (formal_label, expected) (actual_label, actual) ->
                  if formal_label = actual_label then
                    match (expected, level_on_expression actual) with
                    | Some expected, Some actual_level when expected = actual_level -> ()
                    | Some expected, Some actual_level ->
                        Location.raise_errorf ~loc:actual.pexp_loc
                          "%s expects [@log_value.%s], not [@log_value.%s]"
                          name (level_name expected) (level_name actual_level)
                    | Some expected, None ->
                        Location.raise_errorf ~loc:actual.pexp_loc
                          "%s requires [@log_value.%s] on this actual"
                          name (level_name expected)
                    | None, Some actual_level ->
                        Location.raise_errorf ~loc:actual.pexp_loc
                          "%s has no level-gated formal for [@log_value.%s]"
                          name (level_name actual_level)
                    | None, None -> ())
                formals arguments
          | Some _ | None -> ())
      | _ -> ()

    method private validate_level_use ~loc level =
      match !context with
      | Some current when compatible ~context:current level -> ()
      | Some current ->
          Location.raise_errorf ~loc
            "%s log-value used in %s %s context"
            (String.capitalize_ascii (level_name level))
            (level_article current)
            (String.capitalize_ascii (level_name current))
      | None ->
          Location.raise_errorf ~loc
            "%s log-value used outside a level-gated context"
            (String.capitalize_ascii (level_name level))

    method private validate_identifier expression annotation =
      match expression.pexp_desc with
      | Pexp_ident { txt; _ } -> (
          match longident_name txt with
          | None -> ()
          | Some name -> (
              match List.assoc_opt name !bindings with
              | None -> ()
              | Some declared ->
                  (match annotation with
                  | Some used when compatible ~context:used declared -> ()
                  | Some used ->
                      Location.raise_errorf ~loc:expression.pexp_loc
                        "%s is available at %s and cannot be used at %s"
                        name (level_name declared) (level_name used)
                  | None ->
                      Location.raise_errorf ~loc:expression.pexp_loc
                        "%s requires [@log_value.%s] at each use"
                        name (level_name declared))))
      | _ -> ()

    method private gated_expression expression level =
      self#with_context level (fun () -> self#expression expression)

    method private expression_component expression =
      match level_on_expression expression with
      | Some level -> self#gated_expression expression level
      | None -> self#expression expression

    method private pattern_bindings pattern =
      let inherited = level_on_pattern pattern in
      let additions = ref [] in
      let iterator =
        object
          inherit Ast_traverse.iter as pattern_super
          val active = ref inherited
          method! pattern pattern =
            let level =
              match level_on_pattern pattern with
              | Some level -> Some level
              | None -> !active
            in
            with_ref active level (fun () ->
                (match (level, pattern.ppat_desc) with
                | Some level, Ppat_var name ->
                    additions := (name.txt, level) :: !additions
                | _ -> ());
                pattern_super#pattern pattern)
        end
      in
      iterator#pattern pattern;
      !additions

    method! structure structure =
      let rec loop = function
        | [] -> ()
        | item :: rest ->
            self#structure_item item;
            let additions =
              match item.pstr_desc with
              | Pstr_value (Nonrecursive, [ binding ]) -> (
                  match (level_on_binding binding, variable_name binding.pvb_pat) with
                  | Some level, Some name -> [ (name, level) ]
                  | _ -> [])
              | _ -> []
            in
            bindings := additions @ !bindings;
            (match item.pstr_desc with
            | Pstr_value (Nonrecursive, bindings_here) ->
                List.iter
                  (fun binding ->
                    match (variable_name binding.pvb_pat, function_signature binding.pvb_expr) with
                    | Some name, Some signature ->
                        callables := (name, signature) :: !callables
                    | _ -> ())
                  bindings_here
            | _ -> ());
            (match item.pstr_desc with
            | Pstr_type (_, declarations) ->
                List.iter
                  (fun declaration ->
                    match declaration.ptype_kind with
                    | Ptype_record labels ->
                        List.iter
                          (fun label ->
                            fields :=
                              (label.pld_name.txt, level_on_label label) :: !fields)
                          labels
                    | _ -> ())
                  declarations
            | _ -> ());
            loop rest
      in
      loop structure

    method! value_binding binding =
      validate_instrumentation_formals binding;
      match level_on_binding binding with
      | None -> super#value_binding binding
      | Some level ->
          if variable_name binding.pvb_pat = None then
            Location.raise_errorf ~loc:binding.pvb_pat.ppat_loc
              "a level-gated binding requires a variable pattern";
          self#pattern binding.pvb_pat;
          self#with_context level (fun () -> self#expression binding.pvb_expr)

    method! expression expression =
      let annotation = level_on_expression expression in
      self#validate_identifier expression annotation;
      (match expression.pexp_desc with
      | Pexp_field (_, { txt; _ }) ->
          self#validate_field_use ~loc:expression.pexp_loc txt annotation
      | _ -> ());
      match expression.pexp_desc with
      | Pexp_extension ({ txt = name; loc }, payload) -> (
          match log_level_of_extension name with
          | None ->
              Option.iter
                (fun level -> self#validate_level_use ~loc:expression.pexp_loc level)
                annotation;
              super#expression expression
          | Some level ->
              let payload = payload_expression ~loc payload in
              self#with_context level (fun () ->
                  with_ref structural_components false (fun () ->
                      self#expression payload)))
      | Pexp_let (rec_flag, bindings_here, body) ->
          let marked =
            List.filter (fun binding -> level_on_binding binding <> None) bindings_here
          in
          if marked <> [] && (rec_flag = Recursive || List.length bindings_here <> 1)
          then
            Location.raise_errorf ~loc:expression.pexp_loc
              "a level-gated let must be one nonrecursive binding";
          List.iter self#value_binding bindings_here;
          let additions =
            List.concat_map
              (fun binding ->
                match level_on_binding binding with
                | None -> []
                | Some level -> names_with_level level binding.pvb_pat)
              bindings_here
          in
          let callable_additions =
            List.filter_map
              (fun binding ->
                match (variable_name binding.pvb_pat, function_signature binding.pvb_expr) with
                | Some name, Some signature -> Some (name, signature)
                | _ -> None)
              bindings_here
          in
          self#with_bindings additions (fun () ->
              self#with_callables callable_additions (fun () ->
                  self#expression body))
      | Pexp_function (parameters, constraint_, body) ->
          let additions =
            List.concat_map
              (fun parameter ->
                match parameter.pparam_desc with
                | Pparam_newtype _ -> []
                | Pparam_val (_, default, pattern) ->
                    Option.iter self#expression default;
                    self#pattern pattern;
                    (match formal_level pattern with
                    | None -> self#pattern_bindings pattern
                    | Some level ->
                        if variable_name pattern = None then
                          Location.raise_errorf ~loc:pattern.ppat_loc
                            "a level-gated formal requires a variable pattern";
                        names_with_level level pattern))
              parameters
          in
          Option.iter
            (function
              | Pconstraint typ -> self#core_type typ
              | Pcoerce (source, target) ->
                  Option.iter self#core_type source;
                  self#core_type target)
            constraint_;
          self#with_bindings additions (fun () ->
              match body with
              | Pfunction_body body -> self#expression body
              | Pfunction_cases (cases, _, attributes) ->
                  self#attributes attributes;
                  List.iter self#case cases)
      | Pexp_apply (callee, arguments) when !structural_components ->
          self#validate_call callee arguments;
          self#expression callee;
          List.iter (fun (_, argument) -> self#expression_component argument) arguments
      | Pexp_record (record_fields, inherited) when !structural_components ->
          List.iter
            (fun ({ txt; _ }, value) ->
              self#validate_field_slot ~loc:value.pexp_loc txt
                (level_on_expression value);
              self#expression_component value)
            record_fields;
          Option.iter self#expression inherited
      | Pexp_setfield (record, { txt; _ }, value) when !structural_components ->
          self#validate_field_slot ~loc:value.pexp_loc txt (level_on_expression value);
          self#expression record;
          self#expression_component value
      | Pexp_construct (_, Some argument) when !structural_components -> (
          match argument.pexp_desc with
          | Pexp_tuple components ->
              List.iter self#expression_component components
          | _ -> self#expression argument)
      | Pexp_tuple components when !structural_components ->
          List.iter self#expression_component components
      | _ ->
          Option.iter
            (fun level -> self#validate_level_use ~loc:expression.pexp_loc level)
            annotation;
          super#expression expression

    method! pattern pattern =
      (match pattern.ppat_desc with
      | Ppat_record (record_fields, _) ->
          List.iter
            (fun ({ txt; _ }, component) ->
              self#validate_field_slot ~loc:component.ppat_loc txt
                (level_on_pattern component))
            record_fields
      | _ -> ());
      super#pattern pattern

    method! case case =
      self#pattern case.pc_lhs;
      let additions = self#pattern_bindings case.pc_lhs in
      self#with_bindings additions (fun () ->
          Option.iter self#expression case.pc_guard;
          self#expression case.pc_rhs)
  end

class rewriter ~static_level =
  object (self)
    inherit Ast_traverse.map as super

    method private keep level = survives ~static_level level

    method private expression_component expression =
      match level_on_expression expression with
      | Some level when not (self#keep level) -> None
      | Some _ | None -> Some (self#expression expression)

    method private pattern_component pattern =
      match level_on_pattern pattern with
      | Some level when not (self#keep level) -> None
      | Some _ | None -> Some (self#pattern pattern)

    method! structure structure =
      List.filter_map
        (fun item ->
          match item.pstr_desc with
          | Pstr_value (rec_flag, bindings) ->
              let marked =
                List.filter (fun binding -> level_on_binding binding <> None) bindings
              in
              if marked <> [] && (rec_flag = Recursive || List.length bindings <> 1)
              then
                Location.raise_errorf ~loc:item.pstr_loc
                  "a top-level level-gated declaration must be one nonrecursive binding";
              (match bindings with
              | [ binding ] -> (
                  match level_on_binding binding with
                  | Some level when not (self#keep level) -> None
                  | Some _ | None -> Some (super#structure_item item))
              | _ -> Some (super#structure_item item))
          | _ -> Some (super#structure_item item))
        structure

    method! signature signature =
      List.filter_map
        (fun item ->
          match item.psig_desc with
          | Psig_value value -> (
              match level_on_attributes value.pval_attributes with
              | Some level when not (self#keep level) -> None
              | Some _ | None -> Some (super#signature_item item))
          | _ -> Some (super#signature_item item))
        signature

    method! value_binding binding =
      let _, attributes = split_level_attributes binding.pvb_attributes in
      super#value_binding { binding with pvb_attributes = attributes }

    method! value_description value =
      let _, attributes = split_level_attributes value.pval_attributes in
      super#value_description { value with pval_attributes = attributes }

    method! label_declaration label =
      let _, attributes = split_level_attributes label.pld_attributes in
      super#label_declaration { label with pld_attributes = attributes }

    method! type_declaration declaration =
      let rewrite_label label =
        match level_on_label label with
        | Some level when not (self#keep level) -> None
        | Some _ | None -> Some (self#label_declaration label)
      in
      let rewrite_constructor constructor =
        let args =
          match constructor.pcd_args with
          | Pcstr_record labels -> Pcstr_record (List.filter_map rewrite_label labels)
          | Pcstr_tuple arguments ->
              Pcstr_tuple
                (List.filter_map
                   (fun argument ->
                     match level_on_type argument with
                     | Some level when not (self#keep level) -> None
                     | Some _ | None -> Some (self#core_type argument))
                   arguments)
        in
        { (super#constructor_declaration constructor) with pcd_args = args }
      in
      let kind =
        match declaration.ptype_kind with
        | Ptype_record labels ->
            let labels = List.filter_map rewrite_label labels in
            if labels = [] then
              Location.raise_errorf ~loc:declaration.ptype_loc
                "a record cannot contain only level-gated fields";
            Ptype_record labels
        | Ptype_variant constructors ->
            Ptype_variant (List.map rewrite_constructor constructors)
        | kind -> kind
      in
      { (super#type_declaration declaration) with ptype_kind = kind }

    method! core_type typ =
      let _, attributes = split_level_attributes typ.ptyp_attributes in
      let typ = { typ with ptyp_attributes = attributes } in
      match typ.ptyp_desc with
      | Ptyp_arrow (label, argument, result) ->
          let level = level_on_type argument in
          let result = self#core_type result in
          (match level with
          | Some level when not (self#keep level) -> result
          | Some _ | None ->
              { typ with
                ptyp_desc =
                  Ptyp_arrow (label, self#core_type argument, result) })
      | _ -> super#core_type typ

    method! expression expression =
      let level, attributes = split_level_attributes expression.pexp_attributes in
      match level with
      | Some level when not (self#keep level) -> eunit ~loc:expression.pexp_loc
      | Some _ -> super#expression { expression with pexp_attributes = attributes }
      | None -> (
          match expression.pexp_desc with
          | Pexp_let (Nonrecursive, [ binding ], body) -> (
              match level_on_binding binding with
              | Some level when not (self#keep level) -> self#expression body
              | Some _ | None -> super#expression expression)
          | Pexp_function (parameters, constraint_, body) ->
              let parameters =
                List.filter_map
                  (fun parameter ->
                    match parameter.pparam_desc with
                    | Pparam_newtype _ -> Some parameter
                    | Pparam_val (label, default, pattern) ->
                        let level = formal_level pattern in
                        if
                          match level with
                          | Some level -> not (self#keep level)
                          | None -> false
                        then None
                        else
                          Some
                            { parameter with
                              pparam_desc =
                                Pparam_val
                                  ( label,
                                    Option.map self#expression default,
                                    self#pattern
                                      (strip_pattern_formal_level pattern) ) })
                  parameters
              in
              let body =
                match body with
                | Pfunction_body body -> Pfunction_body (self#expression body)
                | Pfunction_cases (cases, loc, attributes) ->
                    Pfunction_cases
                      (List.map self#case cases, loc, self#attributes attributes)
              in
              (match (parameters, body) with
              | [], Pfunction_body body -> body
              | _ ->
                  { expression with
                    pexp_desc = Pexp_function (parameters, constraint_, body) })
          | Pexp_apply (callee, arguments) ->
              let arguments =
                List.filter_map
                  (fun (label, argument) ->
                    Option.map
                      (fun argument -> (label, argument))
                      (self#expression_component argument))
                  arguments
              in
              if arguments = [] then self#expression callee
              else
                { expression with
                  pexp_desc = Pexp_apply (self#expression callee, arguments) }
          | Pexp_record (fields, inherited) ->
              let fields =
                List.filter_map
                  (fun (label, value) ->
                    Option.map
                      (fun value -> (label, value))
                      (self#expression_component value))
                  fields
              in
              (match (fields, inherited) with
              | [], Some inherited -> self#expression inherited
              | [], None -> eunit ~loc:expression.pexp_loc
              | _ ->
                  { expression with
                    pexp_desc =
                      Pexp_record (fields, Option.map self#expression inherited) })
          | Pexp_setfield (record, field, value) -> (
              match self#expression_component value with
              | None -> eunit ~loc:expression.pexp_loc
              | Some value ->
                  { expression with
                    pexp_desc =
                      Pexp_setfield (self#expression record, field, value) })
          | Pexp_construct (constructor, Some argument) -> (
              match argument.pexp_desc with
              | Pexp_tuple components ->
                  let components =
                    List.filter_map self#expression_component components
                  in
                  let argument =
                    match components with
                    | [] -> None
                    | [ component ] -> Some component
                    | components ->
                        Some { argument with pexp_desc = Pexp_tuple components }
                  in
                  { expression with
                    pexp_desc = Pexp_construct (constructor, argument) }
              | _ -> super#expression expression)
          | Pexp_tuple components ->
              let components =
                List.filter_map self#expression_component components
              in
              (match components with
              | [] -> eunit ~loc:expression.pexp_loc
              | [ component ] -> component
              | components ->
                  { expression with pexp_desc = Pexp_tuple components })
          | _ -> super#expression expression)

    method! pattern pattern =
      let level, attributes = split_level_attributes pattern.ppat_attributes in
      match level with
      | Some level when not (self#keep level) -> ppat_any ~loc:pattern.ppat_loc
      | Some _ -> super#pattern { pattern with ppat_attributes = attributes }
      | None -> (
          match pattern.ppat_desc with
          | Ppat_record (fields, closed) ->
              let fields =
                List.filter_map
                  (fun (label, component) ->
                    Option.map
                      (fun component -> (label, component))
                      (self#pattern_component component))
                  fields
              in
              if fields = [] then ppat_any ~loc:pattern.ppat_loc
              else { pattern with ppat_desc = Ppat_record (fields, closed) }
          | Ppat_construct (constructor, Some (vars, argument)) -> (
              match argument.ppat_desc with
              | Ppat_tuple components ->
                  let components =
                    List.filter_map self#pattern_component components
                  in
                  let argument =
                    match components with
                    | [] -> None
                    | [ component ] -> Some (vars, component)
                    | components ->
                        Some
                          ( vars,
                            { argument with ppat_desc = Ppat_tuple components } )
                  in
                  { pattern with ppat_desc = Ppat_construct (constructor, argument) }
              | _ -> super#pattern pattern)
          | Ppat_tuple components ->
              let components =
                List.filter_map self#pattern_component components
              in
              (match components with
              | [] -> ppat_any ~loc:pattern.ppat_loc
              | [ component ] -> component
              | components ->
                  { pattern with ppat_desc = Ppat_tuple components })
          | _ -> super#pattern pattern)
  end

let rewrite ~static_level structure =
  (new validator)#structure structure;
  (new rewriter ~static_level)#structure structure

let rewrite_signature ~static_level signature =
  (new rewriter ~static_level)#signature signature
