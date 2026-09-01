open Ppxlib
open Ast_builder.Default

module Contract = Delator_log_value_contract

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

let pattern_names pattern =
  let names = ref [] in
  let iterator =
    object
      inherit Ast_traverse.iter as super
      method! pattern pattern =
        (match pattern.ppat_desc with
        | Ppat_var name -> names := name.txt :: !names
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
  | Pexp_function (parameters, _, body) ->
      let formals =
        List.filter_map
          (fun parameter ->
            match parameter.pparam_desc with
            | Pparam_newtype _ -> None
            | Pparam_val (label, _, pattern) ->
                Some (label, formal_level pattern))
          parameters
      in
      Some
        (match body with
        | Pfunction_cases _ -> formals @ [ (Nolabel, None) ]
        | Pfunction_body _ -> formals)
  | _ -> None

let reject_fully_gated_callable ~loc = function
  | Some (_ :: _ as formals)
    when List.for_all (fun (_, level) -> Option.is_some level) formals ->
      Location.raise_errorf ~loc
        "a level-gated function must retain at least one ordinary formal"
  | Some _ | None -> ()

let rec signature_formals typ =
  match typ.ptyp_desc with
  | Ptyp_arrow (label, argument, result) ->
      (label, level_on_type argument) :: signature_formals result
  | Ptyp_poly (_, nested) -> signature_formals nested
  | _ -> []

let slot_contracts formals =
  let positional = ref 0 in
  List.map
    (fun (label, level) ->
      let key =
        match label with
        | Nolabel ->
            incr positional;
            Contract.Positional !positional
        | Labelled name | Optional name -> Contract.Named name
      in
      (key, level))
    formals

let formal_slots formals =
  let positional = ref 0 in
  List.map
    (fun (label, level) ->
      let key =
        match label with
        | Nolabel ->
            incr positional;
            Contract.Positional !positional
        | Labelled name | Optional name -> Contract.Named name
      in
      (key, label, level))
    formals

let argument_slots arguments =
  let positional = ref 0 in
  List.map
    (fun (label, expression) ->
      let key =
        match label with
        | Nolabel ->
            incr positional;
            Contract.Positional !positional
        | Labelled name | Optional name -> Contract.Named name
      in
      (key, expression))
    arguments

let longident_parts path =
  let rec loop suffix = function
    | Longident.Lident name -> Some (name :: suffix)
    | Ldot (prefix, name) -> loop (name :: suffix) prefix
    | Lapply _ -> None
  in
  loop [] path

let path_name path = String.concat "." path

let split_qualified path =
  match longident_parts path with
  | Some (_ :: _ :: _ as parts) ->
      let reversed = List.rev parts in
      let name = List.hd reversed in
      Some (List.rev (List.tl reversed), name)
  | Some _ | None -> None

let longident_of_parts = function
  | [] -> invalid_arg "longident_of_parts"
  | first :: rest ->
      List.fold_left (fun path name -> Longident.Ldot (path, name))
        (Longident.Lident first) rest

let qualified_marker_path modules marker =
  longident_of_parts (modules @ [ marker ])

type 'a lookup = Unknown | Known of 'a | Ambiguous

let lookup name entries =
  match
    List.filter_map
      (fun (candidate, contract) ->
        if String.equal candidate name then Some contract else None)
      entries
  with
  | [] -> Unknown
  | first :: rest when List.for_all (( = ) first) rest -> Known first
  | _ -> Ambiguous

type 'a catalog_entry = {
  catalog_name : string;
  catalog_loc : Location.t;
  catalog_contract : 'a;
}

let lookup_catalog ~loc name entries =
  let before_use entry =
    entry.catalog_loc.loc_start.pos_cnum <= loc.Location.loc_start.pos_cnum
  in
  let named =
    List.filter (fun entry -> String.equal entry.catalog_name name) entries
  in
  match List.filter_map
          (fun entry ->
            if before_use entry then Some entry.catalog_contract else None)
          named
  with
  | [] when named = [] -> Unknown
  | [] -> Ambiguous
  | first :: rest when List.for_all (( = ) first) rest -> Known first
  | _ -> Ambiguous

let rec lookup_local ~loc module_path parts entries =
  match lookup_catalog ~loc (path_name (module_path @ parts)) entries with
  | (Known _ | Ambiguous) as result -> result
  | Unknown -> (
      match List.rev module_path with
      | [] -> Unknown
      | _ :: parent -> lookup_local ~loc (List.rev parent) parts entries)

type catalog = {
  callables : (arg_label * level option) list catalog_entry list;
  values : level option catalog_entry list;
  fields : level option catalog_entry list;
  constructors : level option list catalog_entry list;
}

let constructor_levels constructor =
  match constructor.pcd_args with
  | Pcstr_tuple arguments -> Some (List.map level_on_type arguments)
  | Pcstr_record _ -> None

let constructor_contract_owner name arity =
  Printf.sprintf "%s/%d" name arity

let tuple_levels typ =
  match typ.ptyp_desc with
  | Ptyp_tuple components -> Some (List.map level_on_type components)
  | _ -> None

let tuple_type_name_on_pattern pattern =
  match pattern.ppat_desc with
  | Ppat_constraint
      (_, { ptyp_desc = Ptyp_constr ({ txt = Lident name; _ }, []); _ }) ->
      Some name
  | _ -> None

let tuple_type_name_on_binding binding =
  match binding.pvb_constraint with
  | Some
      (Pvc_constraint
        { typ =
            { ptyp_desc = Ptyp_constr ({ txt = Lident name; _ }, []); _ };
          _ }) ->
      Some name
  | Some (Pvc_constraint _ | Pvc_coercion _) | None ->
      tuple_type_name_on_pattern binding.pvb_pat

let rec removable_pattern_is_irrefutable pattern =
  match pattern.ppat_desc with
  | Ppat_any | Ppat_var _ -> true
  | Ppat_alias (nested, _) | Ppat_constraint (nested, _) ->
      removable_pattern_is_irrefutable nested
  | _ -> false

let collect_catalog structure =
  let callables = ref [] in
  let values = ref [] in
  let fields = ref [] in
  let constructors = ref [] in
  let replace_local entry entries =
    let name = fst entry in
    entry :: List.filter (fun (candidate, _) -> not (String.equal name candidate)) entries
  in
  let rec scan path structure =
    let local_callables = ref [] in
    List.iter
      (fun item ->
        match item.pstr_desc with
        | Pstr_value (rec_flag, bindings) ->
            let inherited_callables = !local_callables in
            List.iter
              (fun binding ->
                match variable_name binding.pvb_pat with
                | None -> ()
                | Some name ->
                    let qualified = path_name (path @ [ name ]) in
                    values :=
                      { catalog_name = qualified;
                        catalog_loc = binding.pvb_loc;
                        catalog_contract = level_on_binding binding }
                      :: !values;
                    let signature =
                      match function_signature binding.pvb_expr with
                      | Some signature -> Some signature
                      | None when rec_flag = Nonrecursive -> (
                          match binding.pvb_expr.pexp_desc with
                          | Pexp_ident { txt = Lident target; _ } ->
                              List.assoc_opt target inherited_callables
                          | _ -> None)
                      | None -> None
                    in
                    Option.iter
                      (fun signature ->
                        local_callables :=
                          replace_local (name, signature) !local_callables;
                        callables :=
                          { catalog_name = qualified;
                            catalog_loc = binding.pvb_loc;
                            catalog_contract = signature }
                          :: !callables)
                      signature)
              bindings
        | Pstr_primitive value ->
            let name = value.pval_name.txt in
            let qualified = path_name (path @ [ name ]) in
            values :=
              { catalog_name = qualified;
                catalog_loc = value.pval_loc;
                catalog_contract = level_on_attributes value.pval_attributes }
              :: !values;
            let signature = signature_formals value.pval_type in
            if signature <> [] then (
              local_callables :=
                replace_local (name, signature) !local_callables;
              callables :=
                { catalog_name = qualified;
                  catalog_loc = value.pval_loc;
                  catalog_contract = signature }
                :: !callables)
        | Pstr_type (_, declarations) ->
            List.iter
              (fun declaration ->
                match declaration.ptype_kind with
                | Ptype_record labels ->
                    List.iter
                      (fun label ->
                        fields :=
                          { catalog_name =
                              path_name (path @ [ label.pld_name.txt ]);
                            catalog_loc = label.pld_loc;
                            catalog_contract = level_on_label label }
                          :: !fields)
                      labels
                | Ptype_variant declarations ->
                    List.iter
                      (fun constructor ->
                        match constructor.pcd_args with
                        | Pcstr_tuple _ ->
                            Option.iter
                              (fun levels ->
                                constructors :=
                                  { catalog_name =
                                      path_name
                                        (path @ [ constructor.pcd_name.txt ]);
                                    catalog_loc = constructor.pcd_loc;
                                    catalog_contract = levels }
                                  :: !constructors)
                              (constructor_levels constructor)
                        | Pcstr_record labels ->
                            List.iter
                              (fun label ->
                                fields :=
                                  { catalog_name =
                                      path_name
                                        (path @ [ label.pld_name.txt ]);
                                    catalog_loc = label.pld_loc;
                                    catalog_contract = level_on_label label }
                                  :: !fields)
                              labels)
                      declarations
                | Ptype_abstract | Ptype_open -> ())
              declarations
        | Pstr_module binding -> (
            match (binding.pmb_name.txt, binding.pmb_expr.pmod_desc) with
            | Some name, Pmod_structure nested -> scan (path @ [ name ]) nested
            | _ -> ())
        | _ -> ())
      structure
  in
  scan [] structure;
  { callables = !callables;
    values = !values;
    fields = !fields;
    constructors = !constructors }

let with_ref reference value action =
  let saved = !reference in
  reference := value;
  Fun.protect ~finally:(fun () -> reference := saved) action

class validator ~catalog =
  object (self)
    inherit Ast_traverse.iter as super

    val context : level option ref = ref None
    val bindings : (string * level) list ref = ref []
    val callables : (string * (arg_label * level option) list) list ref = ref []
    val fields : (string * level option) list ref = ref []
    val constructors : (string * level option list) list ref = ref []
    val tuple_types : (string * level option list) list ref = ref []
    val tuple_values : (string * (level option list * bool)) list ref = ref []
    val tuple_site_contract : level option list option ref = ref None
    val tuple_identifier_allowed = ref false
    val witnesses : Contract.witness list ref = ref []
    val structural_components = ref true
    val module_path : string list ref = ref []
    val local_modules : string list ref = ref []
    val binding_only_pattern_components : Location.t list ref = ref []

    method witnesses = List.rev !witnesses
    method binding_only_pattern_components = !binding_only_pattern_components

    method private add_witness ?(compatible = false) ~loc ~modules ~marker level =
      if self#is_local_module_path modules then
        Location.raise_errorf ~loc
          "cannot authenticate a log-value contract through a local module";
      let expected =
        match level with None -> "ordinary" | Some level -> level_name level
      in
      let witness =
        { Contract.contract_path = qualified_marker_path modules marker;
          expected;
          relation =
            (if compatible then Contract.Compatible else Contract.Exact);
          loc }
      in
      if
        not
          (List.exists
             (fun existing ->
               existing.Contract.contract_path = witness.Contract.contract_path
               && String.equal existing.expected witness.expected
               && existing.relation = witness.relation)
             !witnesses)
      then witnesses := witness :: !witnesses

    method private with_context level action = with_ref context (Some level) action

    method private is_local_module_path = function
      | first :: _ -> List.mem first !local_modules
      | [] -> false

    method private with_bindings ~shadowed additions action =
      let outer =
        List.filter
          (fun (name, _) -> not (List.mem name shadowed))
          !bindings
      in
      with_ref bindings (additions @ outer) action

    method private with_callables ~shadowed additions action =
      let outer =
        List.filter
          (fun (name, _) -> not (List.mem name shadowed))
          !callables
      in
      with_ref callables (additions @ outer) action

    method private field_level ~loc label =
      match longident_parts label with
      | Some [ name ] -> lookup name !fields
      | Some parts -> lookup_local ~loc !module_path parts catalog.fields
      | None -> Unknown

    method private cannot_authenticate ~loc ~site level =
      Location.raise_errorf ~loc
        "cannot authenticate [@log_value.%s] actual for %s"
        (level_name level) site

    method private validate_field_slot ~loc label annotation =
      let name = final_longident_name label in
      match (self#field_level ~loc label, annotation) with
      | Ambiguous, _ ->
          Location.raise_errorf ~loc
            "record field %s has ambiguous log-value declarations" name
      | Known (Some expected), Some actual when expected = actual -> ()
      | Known (Some expected), Some actual ->
          Location.raise_errorf ~loc
            "field %s expects [@log_value.%s], not [@log_value.%s]"
            name (level_name expected) (level_name actual)
      | Known (Some expected), None ->
          Location.raise_errorf ~loc
            "field %s requires [@log_value.%s] at this site"
            name (level_name expected)
      | Known None, Some actual ->
          Location.raise_errorf ~loc
            "field %s is not level-gated, but this site uses [@log_value.%s]"
            name (level_name actual)
      | Known None, None | Unknown, None -> ()
      | Unknown, Some actual -> (
          match split_qualified label with
          | Some (modules, field) ->
              self#add_witness ~loc ~modules
                ~marker:(Contract.marker_name ~kind:"field" ~owner:field ())
                (Some actual)
          | None -> self#cannot_authenticate ~loc ~site:name actual)

    method private validate_field_use ~loc label annotation =
      let name = final_longident_name label in
      match (self#field_level ~loc label, annotation) with
      | Ambiguous, _ ->
          Location.raise_errorf ~loc
            "record field %s has ambiguous log-value declarations" name
      | Known (Some declared), Some used
        when compatible ~context:used declared ->
          ()
      | Known (Some declared), Some used ->
          Location.raise_errorf ~loc
            "field %s is available at %s and cannot be used at %s"
            name (level_name declared) (level_name used)
      | Known (Some declared), None ->
          Location.raise_errorf ~loc
            "field %s requires a compatible [@log_value.LEVEL] use annotation (declared %s)"
            name (level_name declared)
      | Known None, Some used ->
          Location.raise_errorf ~loc
            "field %s is not level-gated, but this use has [@log_value.%s]"
            name (level_name used)
      | Known None, None | Unknown, None -> ()
      | Unknown, Some used -> (
          match split_qualified label with
          | Some (modules, field) ->
              self#add_witness ~compatible:true ~loc ~modules
                ~marker:(Contract.marker_name ~kind:"field" ~owner:field ())
                (Some used)
          | None -> self#cannot_authenticate ~loc ~site:name used)

    method private validate_actual ~callee_name ~loc expected actual =
      match (expected, actual) with
      | Some expected, Some actual when expected = actual -> ()
      | Some expected, Some actual ->
          Location.raise_errorf ~loc
            "%s expects [@log_value.%s], not [@log_value.%s]"
            callee_name (level_name expected) (level_name actual)
      | Some expected, None ->
          Location.raise_errorf ~loc
            "%s requires [@log_value.%s] on this actual"
            callee_name (level_name expected)
      | None, Some actual -> self#cannot_authenticate ~loc ~site:callee_name actual
      | None, None -> ()

    method private validate_known_call name formals arguments =
      let indexed_formals = formal_slots formals in
      let contracts = slot_contracts formals in
      List.iter
        (fun (key, actual) ->
          match List.assoc_opt key contracts with
          | Some expected ->
              self#validate_actual ~callee_name:name ~loc:actual.pexp_loc expected
                (level_on_expression actual)
          | None ->
              Option.iter
                (fun level ->
                  self#cannot_authenticate ~loc:actual.pexp_loc ~site:name level)
                (level_on_expression actual))
        (argument_slots arguments);
      let supplied = List.map fst (argument_slots arguments) in
      let missing_required =
        List.filter
          (fun (key, label, _) ->
            match label with
            | Optional _ -> false
            | Nolabel | Labelled _ -> not (List.mem key supplied))
          indexed_formals
      in
      if
        missing_required <> []
        && List.for_all
             (fun (_, _, level) -> Option.is_some level)
             missing_required
      then
        Location.raise_errorf ~loc:(List.hd arguments |> snd).pexp_loc
          "partial application of %s leaves only level-gated required formals; erasure could execute the function body"
          name

    method private external_call modules name arguments =
      List.iter
        (fun (key, argument) ->
          match level_on_expression argument with
          | Some level
            when not
                   (Option.value ~default:false
                      (Option.map
                         (fun current -> compatible ~context:current level)
                         !context)) ->
              self#add_witness ~loc:argument.pexp_loc ~modules
                ~marker:
                  (Contract.marker_name ~kind:"function" ~owner:name ~slot:key
                     ())
                (Some level)
          | Some _ | None -> ())
        (argument_slots arguments)

    method private validate_call callee arguments =
      let reject_unknown site =
        List.iter
          (fun (_, argument) ->
            Option.iter
              (fun level ->
                self#cannot_authenticate ~loc:argument.pexp_loc ~site level)
              (level_on_expression argument))
          arguments
      in
      match callee.pexp_desc with
      | Pexp_ident { txt = Lident name; _ } -> (
          match List.assoc_opt name !callables with
          | Some formals -> self#validate_known_call name formals arguments
          | None -> reject_unknown name)
      | Pexp_ident { txt; _ } -> (
          match split_qualified txt with
          | Some (modules, _) when self#is_local_module_path modules ->
              reject_unknown "local module callee"
          | Some (modules, name) -> (
              match
                lookup_local ~loc:callee.pexp_loc !module_path
                  (modules @ [ name ]) catalog.callables
              with
              | Known formals -> self#validate_known_call name formals arguments
              | Ambiguous -> reject_unknown (path_name (modules @ [ name ]))
              | Unknown -> self#external_call modules name arguments)
          | None -> reject_unknown "qualified callee")
      | _ -> reject_unknown "expression callee"

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
      (match expression.pexp_desc with
      | Pexp_ident { txt = Lident name; _ }
        when Option.value ~default:false
               (Option.map snd (self#tuple_value_provenance name))
             && not !tuple_identifier_allowed ->
          Location.raise_errorf ~loc:expression.pexp_loc
            "tuple value %s has level-gated lexical provenance and may only be directly aliased or destructured by a matching pattern"
            name
      | _ -> ());
      match expression.pexp_desc with
      | Pexp_ident { txt; _ } -> (
          match longident_name txt with
          | None -> (
              match (split_qualified txt, annotation) with
              | Some (modules, name), Some used -> (
                  match
                    lookup_local ~loc:expression.pexp_loc !module_path
                      (modules @ [ name ]) catalog.values
                  with
                  | Known (Some declared) when compatible ~context:used declared -> ()
                  | Known (Some declared) ->
                      Location.raise_errorf ~loc:expression.pexp_loc
                        "%s is available at %s and cannot be used at %s"
                        name (level_name declared) (level_name used)
                  | Known None | Ambiguous ->
                      self#cannot_authenticate ~loc:expression.pexp_loc
                        ~site:(path_name (modules @ [ name ])) used
                  | Unknown ->
                      self#add_witness ~compatible:true
                        ~loc:expression.pexp_loc ~modules
                        ~marker:
                          (Contract.marker_name ~kind:"value" ~owner:name ())
                        (Some used))
              | _ -> ())
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

    method private validate_component ~loc ~site ~position expected actual =
      match (expected, actual) with
      | Some expected, Some actual when expected = actual -> ()
      | Some expected, Some actual ->
          Location.raise_errorf ~loc
            "%s[%d] expects [@log_value.%s], not [@log_value.%s]" site
            position (level_name expected) (level_name actual)
      | Some expected, None ->
          Location.raise_errorf ~loc
            "%s component %d requires [@log_value.%s]"
            site position (level_name expected)
      | None, Some _ ->
          Location.raise_errorf ~loc "%s component %d is not level-gated" site
            position
      | None, None -> ()

    method private validate_components ~site expected components =
      List.iteri
        (fun index (loc, actual) ->
          match List.nth_opt expected index with
          | Some expected ->
              self#validate_component ~loc ~site ~position:(index + 1) expected
                actual
          | None ->
              Option.iter
                (fun _ ->
                  Location.raise_errorf ~loc
                    "%s component %d is not level-gated" site (index + 1))
                actual)
        components

    method private constructor_level ~loc ~arity path =
      match longident_parts path with
      | Some [ name ] ->
          lookup name
            (List.filter
               (fun (_, levels) -> List.length levels = arity)
               !constructors)
      | Some parts ->
          lookup_local ~loc !module_path parts
            (List.filter
               (fun entry -> List.length entry.catalog_contract = arity)
               catalog.constructors)
      | None -> Unknown

    method private external_constructor modules name components =
      let owner = constructor_contract_owner name (List.length components) in
      List.iteri
        (fun index (loc, level) ->
          match level with
          | Some level ->
            self#add_witness ~loc ~modules
              ~marker:
                (Contract.marker_name ~kind:"constructor" ~owner
                   ~slot:(Contract.Positional (index + 1)) ())
              (Some level)
          | None -> ())
        components

    method private validate_constructor path components =
      let name = final_longident_name path in
      match
        self#constructor_level ~loc:(fst (List.hd components))
          ~arity:(List.length components) path
      with
      | Known expected ->
          self#validate_components ~site:("constructor " ^ name) expected
            components
      | Ambiguous ->
          Location.raise_errorf ~loc:(fst (List.hd components))
            "constructor %s has ambiguous log-value declarations" name
      | Unknown -> (
          match split_qualified path with
          | Some (modules, name) ->
              self#external_constructor modules name components
          | None ->
              List.iter
                (fun (loc, level) ->
                  Option.iter
                    (fun level ->
                      self#cannot_authenticate ~loc
                        ~site:("constructor " ^ name) level)
                    level)
                components)

    method private validate_constructor_pattern path patterns =
      let components =
        List.map
          (fun pattern -> (pattern.ppat_loc, level_on_pattern pattern))
          patterns
      in
      let name = final_longident_name path in
      match
        self#constructor_level ~loc:(fst (List.hd components))
          ~arity:(List.length components) path
      with
      | Known expected ->
          self#validate_components ~site:("constructor " ^ name) expected
            components
      | Ambiguous ->
          Location.raise_errorf ~loc:(fst (List.hd components))
            "constructor %s has ambiguous log-value declarations" name
      | Unknown ->
          let structural_components =
            List.map2
              (fun pattern (loc, level) ->
                match level with
                | Some _ when pattern_names pattern <> [] ->
                    binding_only_pattern_components :=
                      loc :: !binding_only_pattern_components;
                    (loc, None)
                | Some _ | None -> (loc, level))
              patterns components
          in
          (match split_qualified path with
          | Some (modules, name) ->
              self#external_constructor modules name structural_components
          | None ->
              List.iter
                (fun (loc, level) ->
                  Option.iter
                    (fun level ->
                      self#cannot_authenticate ~loc
                        ~site:("constructor " ^ name) level)
                    level)
                structural_components)

    method private tuple_contract_named name = lookup name !tuple_types

    method private tuple_value_provenance name = List.assoc_opt name !tuple_values

    method private tuple_value_contract name =
      Option.map fst (self#tuple_value_provenance name)

    method private known_tuple_type name =
      match self#tuple_contract_named name with
      | Known levels -> Some levels
      | Unknown | Ambiguous -> None

    method private tuple_expression_provenance expression =
      match expression.pexp_desc with
      | Pexp_tuple components ->
          let levels = List.map level_on_expression components in
          if List.exists Option.is_some levels then Some (levels, true) else None
      | Pexp_constraint
          ( { pexp_desc = Pexp_tuple _; _ },
            { ptyp_desc = Ptyp_constr ({ txt = Lident name; _ }, []); _ } ) ->
          Option.map (fun levels -> (levels, false))
            (self#known_tuple_type name)
      | Pexp_ident { txt = Lident name; _ } ->
          self#tuple_value_provenance name
      | _ -> None

    method private validate_tuple ?type_name components =
      let contextual = !tuple_site_contract in
      tuple_site_contract := None;
      match type_name with
      | Some name -> (
          match self#tuple_contract_named name with
          | Known expected ->
              self#validate_components ~site:"tuple" expected components
          | Ambiguous ->
              Location.raise_errorf ~loc:(fst (List.hd components))
                "tuple type %s has ambiguous log-value declarations" name
          | Unknown ->
              if
                List.exists
                  (fun (_, level) -> Option.is_some level)
                  components
              then
                Location.raise_errorf ~loc:(fst (List.hd components))
                  "cannot authenticate log-value tuple type %s" name)
      | None -> (
          match contextual with
          | Some expected ->
              self#validate_components ~site:"tuple" expected components
          | None ->
              if
                List.exists (fun (_, level) -> Option.is_some level) components
              then
                Location.raise_errorf ~loc:(fst (List.hd components))
                  "a gated tuple site requires an explicit named contract or direct lexical tuple provenance")

    method private validate_tuple_type path components =
      match path with
      | Longident.Lident name -> self#validate_tuple ~type_name:name components
      | _ when List.exists (fun (_, level) -> Option.is_some level) components ->
          let name =
            Option.value ~default:"qualified tuple"
              (Option.map path_name (longident_parts path))
          in
          Location.raise_errorf ~loc:(fst (List.hd components))
            "cannot authenticate log-value tuple type %s" name
      | _ -> ()

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

    method! module_binding binding =
      match binding.pmb_name.txt with
      | Some name ->
          with_ref module_path (!module_path @ [ name ]) (fun () ->
              super#module_binding binding)
      | None -> super#module_binding binding

    method! structure structure =
      let rec loop = function
        | [] -> ()
        | item :: rest ->
            let traverse_item () = self#structure_item item in
            (match item.pstr_desc with
            | Pstr_module _ | Pstr_recmodule _ ->
                let saved_bindings = !bindings in
                let saved_callables = !callables in
                let saved_fields = !fields in
                let saved_constructors = !constructors in
                let saved_tuple_types = !tuple_types in
                let saved_tuple_values = !tuple_values in
                Fun.protect
                  ~finally:(fun () ->
                    bindings := saved_bindings;
                    callables := saved_callables;
                    fields := saved_fields;
                    constructors := saved_constructors;
                    tuple_types := saved_tuple_types;
                    tuple_values := saved_tuple_values)
                  traverse_item
            | Pstr_value (Recursive, bindings_here) ->
                let shadowed =
                  List.concat_map
                    (fun binding -> pattern_names binding.pvb_pat)
                    bindings_here
                in
                let recursive_callables =
                  List.filter_map
                    (fun binding ->
                      match
                        ( variable_name binding.pvb_pat,
                          function_signature binding.pvb_expr )
                      with
                      | Some name, Some signature -> Some (name, signature)
                      | _ -> None)
                    bindings_here
                in
                self#with_callables ~shadowed recursive_callables traverse_item
            | _ -> traverse_item ());
            let additions =
              match item.pstr_desc with
              | Pstr_value (Nonrecursive, [ binding ]) -> (
                  match (level_on_binding binding, variable_name binding.pvb_pat) with
                  | Some level, Some name -> [ (name, level) ]
                  | _ -> [])
              | _ -> []
            in
            let shadowed, callable_additions =
              match item.pstr_desc with
              | Pstr_value (rec_flag, bindings_here) ->
                  ( List.concat_map
                      (fun binding -> pattern_names binding.pvb_pat)
                      bindings_here,
                    List.filter_map
                      (fun binding ->
                        match variable_name binding.pvb_pat with
                        | None -> None
                        | Some name ->
                            let signature =
                              match function_signature binding.pvb_expr with
                              | Some signature -> Some signature
                              | None when rec_flag = Nonrecursive -> (
                                  match binding.pvb_expr.pexp_desc with
                                  | Pexp_ident { txt = Lident target; _ } ->
                                      List.assoc_opt target !callables
                                  | _ -> None)
                              | None -> None
                            in
                            Option.map (fun signature -> (name, signature)) signature)
                      bindings_here )
              | Pstr_primitive value ->
                  let formals = signature_formals value.pval_type in
                  ( [ value.pval_name.txt ],
                    if formals = [] then []
                    else [ (value.pval_name.txt, formals) ] )
              | _ -> ([], [])
            in
            bindings :=
              additions
              @ List.filter
                  (fun (name, _) -> not (List.mem name shadowed))
                  !bindings;
            callables :=
              callable_additions
              @ List.filter
                  (fun (name, _) -> not (List.mem name shadowed))
                  !callables;
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
                    | Ptype_variant declarations ->
                        List.iter
                          (fun constructor ->
                            match constructor.pcd_args with
                            | Pcstr_tuple arguments ->
                                constructors :=
                                  ( constructor.pcd_name.txt,
                                    List.map level_on_type arguments )
                                  :: !constructors
                            | Pcstr_record labels ->
                                List.iter
                                  (fun label ->
                                    fields :=
                                      (label.pld_name.txt, level_on_label label)
                                      :: !fields)
                                  labels)
                          declarations
                    | Ptype_abstract ->
                        Option.iter
                          (fun levels ->
                            tuple_types :=
                              (declaration.ptype_name.txt, levels) :: !tuple_types)
                          (Option.bind declaration.ptype_manifest tuple_levels)
                    | Ptype_open -> ())
                  declarations
            | _ -> ());
            loop rest
      in
      loop structure

    method! value_binding binding =
      validate_instrumentation_formals binding;
      reject_fully_gated_callable ~loc:binding.pvb_loc
        (function_signature binding.pvb_expr);
      Option.iter self#value_constraint binding.pvb_constraint;
      let declared_provenance =
        Option.map (fun levels -> (levels, false))
          (Option.bind (tuple_type_name_on_binding binding)
             self#known_tuple_type)
      in
      let provenance =
        match declared_provenance with
        | Some _ as provenance -> provenance
        | None -> self#tuple_expression_provenance binding.pvb_expr
      in
      let contract = Option.map fst provenance in
      let expression_contract =
        match binding.pvb_expr.pexp_desc with
        | Pexp_tuple _ -> contract
        | _ -> None
      in
      let allow_identifier =
        match binding.pvb_expr.pexp_desc with
        | Pexp_ident { txt = Lident name; _ } ->
            Option.is_some (self#tuple_value_contract name)
        | _ -> false
      in
      with_ref tuple_site_contract contract (fun () ->
          self#pattern binding.pvb_pat);
      let validate_expression () =
        with_ref tuple_site_contract expression_contract (fun () ->
            with_ref tuple_identifier_allowed allow_identifier (fun () ->
                self#expression binding.pvb_expr))
      in
      (match level_on_binding binding with
      | None -> validate_expression ()
      | Some level ->
          if variable_name binding.pvb_pat = None then
            Location.raise_errorf ~loc:binding.pvb_pat.ppat_loc
              "a level-gated binding requires a variable pattern";
          self#with_context level validate_expression);
      let shadowed = pattern_names binding.pvb_pat in
      tuple_values :=
        List.filter
          (fun (name, _) -> not (List.mem name shadowed))
          !tuple_values;
      match (variable_name binding.pvb_pat, provenance) with
      | Some name, Some (levels, _ as provenance)
        when List.exists Option.is_some levels ->
          tuple_values := (name, provenance) :: !tuple_values
      | _ -> ()

    method! expression expression =
      let annotation = level_on_expression expression in
      self#validate_identifier expression annotation;
      (match expression.pexp_desc with
      | Pexp_field (_, { txt; _ }) ->
          self#validate_field_use ~loc:expression.pexp_loc txt annotation
      | _ -> ());
      match expression.pexp_desc with
      | Pexp_letmodule (name, module_expression, body) ->
          self#module_expr module_expression;
          (match name.txt with
          | Some name ->
              with_ref local_modules (name :: !local_modules) (fun () ->
                  self#expression body)
          | None -> self#expression body)
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
          let outer_tuple_values = !tuple_values in
          let recursive_callables =
            if rec_flag = Recursive then
              List.filter_map
                (fun binding ->
                  match
                    ( variable_name binding.pvb_pat,
                      function_signature binding.pvb_expr )
                  with
                  | Some name, Some signature -> Some (name, signature)
                  | _ -> None)
                bindings_here
            else []
          in
          let recursive_shadowed =
            List.concat_map
              (fun binding -> pattern_names binding.pvb_pat)
              bindings_here
          in
          let tuple_additions = ref [] in
          let validate_binding binding =
            tuple_values := outer_tuple_values;
            self#value_binding binding;
            let names = pattern_names binding.pvb_pat in
            tuple_additions :=
              List.filter
                (fun (name, _) -> List.mem name names)
                !tuple_values
              @ !tuple_additions
          in
          self#with_callables ~shadowed:recursive_shadowed recursive_callables
            (fun () -> List.iter validate_binding bindings_here);
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
                match variable_name binding.pvb_pat with
                | None -> None
                | Some name ->
                    let signature =
                      match function_signature binding.pvb_expr with
                      | Some signature -> Some signature
                      | None -> (
                          match binding.pvb_expr.pexp_desc with
                          | Pexp_ident { txt = Lident target; _ } ->
                              List.assoc_opt target !callables
                          | _ -> None)
                    in
                    Option.map (fun signature -> (name, signature)) signature)
              bindings_here
          in
          let shadowed = recursive_shadowed in
          let scoped_tuple_values =
            !tuple_additions
            @ List.filter
                (fun (name, _) -> not (List.mem name shadowed))
                outer_tuple_values
          in
          tuple_values := outer_tuple_values;
          with_ref tuple_values scoped_tuple_values (fun () ->
              self#with_bindings ~shadowed additions (fun () ->
                  self#with_callables ~shadowed callable_additions (fun () ->
                      self#expression body)))
      | Pexp_function (parameters, constraint_, body) ->
          reject_fully_gated_callable ~loc:expression.pexp_loc
            (function_signature expression);
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
          let shadowed =
            List.concat_map
              (fun parameter ->
                match parameter.pparam_desc with
                | Pparam_newtype _ -> []
                | Pparam_val (_, _, pattern) -> pattern_names pattern)
              parameters
          in
          self#with_bindings ~shadowed additions (fun () ->
              self#with_callables ~shadowed [] (fun () ->
                  match body with
                  | Pfunction_body body -> self#expression body
                  | Pfunction_cases (cases, _, attributes) ->
                      self#attributes attributes;
                      List.iter self#case cases))
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
      | Pexp_constraint
          ({ pexp_desc = Pexp_tuple components; _ },
           { ptyp_desc = Ptyp_constr ({ txt = type_path; _ }, []); _ })
        when !structural_components ->
          self#validate_tuple_type type_path
            (List.map
               (fun component ->
                 (component.pexp_loc, level_on_expression component))
               components);
          List.iter self#expression_component components
      | Pexp_construct ({ txt = constructor; _ }, Some argument)
        when !structural_components -> (
          match argument.pexp_desc with
          | Pexp_tuple components ->
              self#validate_constructor constructor
                (List.map
                   (fun component ->
                     (component.pexp_loc, level_on_expression component))
                   components);
              List.iter self#expression_component components
          | _ ->
              self#validate_constructor constructor
                [ (argument.pexp_loc, level_on_expression argument) ];
              self#expression_component argument)
      | Pexp_tuple components when !structural_components ->
          self#validate_tuple
            (List.map
               (fun component ->
                 (component.pexp_loc, level_on_expression component))
               components);
          List.iter self#expression_component components
      | _ ->
          Option.iter
            (fun level -> self#validate_level_use ~loc:expression.pexp_loc level)
            annotation;
          super#expression expression

    method! pattern pattern =
      (match level_on_pattern pattern with
      | Some _ when not (removable_pattern_is_irrefutable pattern) ->
          Location.raise_errorf ~loc:pattern.ppat_loc
            "a level-gated pattern component must be irrefutable"
      | Some _ | None -> ());
      match pattern.ppat_desc with
      | Ppat_record (record_fields, _) ->
          List.iter
            (fun ({ txt; _ }, component) ->
              self#validate_field_slot ~loc:component.ppat_loc txt
                (level_on_pattern component))
            record_fields;
          super#pattern pattern
      | Ppat_constraint
          ({ ppat_desc = Ppat_tuple components; _ },
           ({ ptyp_desc = Ptyp_constr ({ txt = type_path; _ }, []); _ } as typ)) ->
          self#validate_tuple_type type_path
            (List.map
               (fun component ->
                 (component.ppat_loc, level_on_pattern component))
               components);
          self#core_type typ;
          List.iter self#pattern components
      | Ppat_construct ({ txt = constructor; _ }, Some (_, argument)) -> (
          match argument.ppat_desc with
          | Ppat_tuple components ->
              self#validate_constructor_pattern constructor components;
              List.iter self#pattern components
          | _ ->
              self#validate_constructor_pattern constructor [ argument ];
              self#pattern argument)
      | Ppat_tuple components ->
          self#validate_tuple
            (List.map
               (fun component ->
                 (component.ppat_loc, level_on_pattern component))
               components);
          List.iter self#pattern components
      | _ -> super#pattern pattern

    method! case case =
      self#pattern case.pc_lhs;
      let additions = self#pattern_bindings case.pc_lhs in
      let shadowed = pattern_names case.pc_lhs in
      self#with_bindings ~shadowed additions (fun () ->
          self#with_callables ~shadowed [] (fun () ->
              Option.iter self#expression case.pc_guard;
              self#expression case.pc_rhs))
  end

type contract_spec = {
  marker : string;
  contract : string;
  loc : Location.t;
}

let contract_name = function
  | None -> "ordinary"
  | Some level -> level_name level

let label_abi = function
  | Nolabel -> ("nolabel", "")
  | Labelled name -> ("labelled", name)
  | Optional name -> ("optional", name)

let formal_abi (label, level) =
  let kind, name = label_abi label in
  Contract.abi_component "formal" [ kind; name; contract_name level ]

let function_abi value_level formals =
  Contract.abi_contract "function"
    (contract_name value_level :: List.map formal_abi formals)

let field_abi label =
  Contract.abi_component "field"
    [ label.pld_name.txt;
      (match label.pld_mutable with Mutable -> "mutable" | Immutable -> "immutable");
      contract_name (level_on_label label) ]

let type_abi declaration =
  let descriptor =
    match declaration.ptype_kind with
    | Ptype_record labels ->
        Some (Contract.abi_contract "record" (List.map field_abi labels))
    | Ptype_variant constructors ->
        let constructor_abi constructor =
          let result = if Option.is_some constructor.pcd_res then "gadt" else "regular" in
          match constructor.pcd_args with
          | Pcstr_tuple arguments ->
              Contract.abi_component "constructor-tuple"
                (constructor.pcd_name.txt :: result
                :: List.map
                     (fun argument -> contract_name (level_on_type argument))
                     arguments)
          | Pcstr_record labels ->
              Contract.abi_component "constructor-record"
                (constructor.pcd_name.txt :: result :: List.map field_abi labels)
        in
        Some
          (Contract.abi_contract "variant" (List.map constructor_abi constructors))
    | Ptype_abstract -> (
        match Option.bind declaration.ptype_manifest tuple_levels with
        | Some levels ->
            Some
              (Contract.abi_contract "tuple" (List.map contract_name levels))
        | None -> None)
    | Ptype_open -> Some (Contract.abi_contract "open" [])
  in
  Option.map
    (fun contract ->
      { marker =
          Contract.marker_name ~kind:"type_abi"
            ~owner:declaration.ptype_name.txt ();
        contract;
        loc = declaration.ptype_loc })
    descriptor

let replace_contract spec contracts =
  spec
  :: List.filter
       (fun candidate -> not (String.equal candidate.marker spec.marker))
       contracts

let add_strict_contract ~collision spec contracts =
  match List.find_opt (fun candidate -> String.equal candidate.marker spec.marker) contracts with
  | None -> spec :: contracts
  | Some existing when String.equal existing.contract spec.contract -> contracts
  | Some _ -> Location.raise_errorf ~loc:spec.loc "%s" collision

let starts_with text prefix =
  String.length text >= String.length prefix
  && String.sub text 0 (String.length prefix) = prefix

let function_abi_marker name =
  Contract.marker_name ~kind:"function_abi" ~owner:name ()

let remove_function_contracts name contracts =
  let prefix = Contract.marker_name ~kind:"function" ~owner:name () ^ "_" in
  let abi_marker = function_abi_marker name in
  List.filter
    (fun spec ->
      not
        (starts_with spec.marker prefix || String.equal spec.marker abi_marker))
    contracts

let add_value_contract contracts ~loc ~name level =
  let marker = Contract.marker_name ~kind:"value" ~owner:name () in
  match level with
  | None ->
      List.filter
        (fun candidate -> not (String.equal candidate.marker marker))
        contracts
  | Some _ ->
      replace_contract { marker; contract = contract_name level; loc } contracts

let add_function_contracts contracts ~loc ~name ~value_level formals =
  if
    Option.is_none value_level
    && not (List.exists (fun (_, level) -> Option.is_some level) formals)
  then contracts
  else
    let contracts = remove_function_contracts name contracts in
    let contracts =
      { marker = function_abi_marker name;
        contract = function_abi value_level formals;
        loc }
      :: contracts
    in
    List.fold_left
      (fun contracts (slot, level) ->
        { marker =
            Contract.marker_name ~kind:"function" ~owner:name ~slot ();
          contract = contract_name level;
          loc }
        :: contracts)
      contracts (slot_contracts formals)

let type_has_log_value declaration =
  let label_has_log_value label = Option.is_some (level_on_label label) in
  match declaration.ptype_kind with
  | Ptype_record labels -> List.exists label_has_log_value labels
  | Ptype_variant constructors ->
      List.exists
        (fun constructor ->
          match constructor.pcd_args with
          | Pcstr_tuple arguments ->
              List.exists
                (fun argument -> Option.is_some (level_on_type argument))
                arguments
          | Pcstr_record labels -> List.exists label_has_log_value labels)
        constructors
  | Ptype_abstract -> (
      match Option.bind declaration.ptype_manifest tuple_levels with
      | Some levels -> List.exists Option.is_some levels
      | None -> false)
  | Ptype_open -> false

let add_type_contracts contracts declaration =
  if not (type_has_log_value declaration) then contracts
  else
  let contracts =
    match type_abi declaration with
    | None -> contracts
    | Some contract -> replace_contract contract contracts
  in
  match declaration.ptype_kind with
  | Ptype_record labels ->
      List.fold_left
        (fun contracts label ->
          let name = label.pld_name.txt in
          let spec =
            { marker = Contract.marker_name ~kind:"field" ~owner:name ();
              contract = contract_name (level_on_label label);
              loc = label.pld_loc }
          in
          add_strict_contract
            ~collision:
              (Printf.sprintf
                 "record field %s has ambiguous log-value declarations" name)
            spec contracts)
        contracts labels
  | Ptype_variant constructors ->
      List.fold_left
        (fun contracts constructor ->
          match constructor_levels constructor with
          | None -> (
              match constructor.pcd_args with
              | Pcstr_tuple _ -> contracts
              | Pcstr_record labels ->
                  List.fold_left
                    (fun contracts label ->
                      let name = label.pld_name.txt in
                      let spec =
                        { marker =
                            Contract.marker_name ~kind:"field" ~owner:name ();
                          contract = contract_name (level_on_label label);
                          loc = label.pld_loc }
                      in
                      add_strict_contract
                        ~collision:
                          (Printf.sprintf
                             "record field %s has ambiguous log-value declarations"
                             name)
                        spec contracts)
                    contracts labels)
          | Some levels ->
              List.fold_left
                (fun contracts (slot, level) ->
                  let name = constructor.pcd_name.txt in
                  let owner = constructor_contract_owner name (List.length levels) in
                  let spec =
                    { marker =
                        Contract.marker_name ~kind:"constructor" ~owner
                          ~slot ();
                      contract = contract_name level;
                      loc = constructor.pcd_loc }
                  in
                  add_strict_contract
                    ~collision:
                      (Printf.sprintf
                         "constructor %s has ambiguous log-value declarations"
                         name)
                    spec contracts)
                contracts
                (List.mapi
                   (fun index level ->
                     (Contract.Positional (index + 1), level))
                   levels))
        contracts constructors
  | Ptype_abstract -> (
      match Option.bind declaration.ptype_manifest tuple_levels with
      | None -> contracts
      | Some levels ->
          List.fold_left
            (fun contracts (slot, level) ->
              { marker =
                  Contract.marker_name ~kind:"tuple"
                    ~owner:declaration.ptype_name.txt ~slot ();
                contract = contract_name level;
                loc = declaration.ptype_loc }
              :: contracts)
            contracts
            (List.mapi
               (fun index level ->
                 (Contract.Positional (index + 1), level))
               levels))
  | Ptype_open -> contracts

let structure_contracts structure =
  let contracts = ref [] in
  let callables = ref [] in
  List.iter
    (fun item ->
      match item.pstr_desc with
      | Pstr_value (rec_flag, bindings) ->
          let inherited_callables = !callables in
          List.iter
            (fun binding ->
              match variable_name binding.pvb_pat with
              | None -> ()
              | Some name ->
                  let value_level = level_on_binding binding in
                  contracts :=
                    add_value_contract !contracts ~loc:binding.pvb_loc ~name
                      value_level;
                  let signature =
                    match function_signature binding.pvb_expr with
                    | Some signature -> Some signature
                    | None when rec_flag = Nonrecursive -> (
                        match binding.pvb_expr.pexp_desc with
                        | Pexp_ident { txt = Lident target; _ } ->
                            List.assoc_opt target inherited_callables
                        | _ -> None)
                    | None -> None
                  in
                  contracts := remove_function_contracts name !contracts;
                  callables :=
                    List.filter
                      (fun (candidate, _) -> not (String.equal candidate name))
                      !callables;
                  Option.iter
                    (fun signature ->
                      contracts :=
                        add_function_contracts !contracts ~loc:binding.pvb_loc
                          ~name ~value_level signature;
                      callables := (name, signature) :: !callables)
                    signature)
            bindings
      | Pstr_primitive value ->
          let name = value.pval_name.txt in
          let value_level = level_on_attributes value.pval_attributes in
          contracts :=
            add_value_contract !contracts ~loc:value.pval_loc ~name value_level;
          contracts := remove_function_contracts name !contracts;
          let formals = signature_formals value.pval_type in
          if formals <> [] then
            contracts :=
              add_function_contracts !contracts ~loc:value.pval_loc ~name
                ~value_level formals
      | Pstr_type (_, declarations) ->
          List.iter
            (fun declaration ->
              if
                not
                  (Contract.is_internal_name declaration.ptype_name.txt)
              then contracts := add_type_contracts !contracts declaration)
            declarations
      | _ -> ())
    structure;
  List.rev !contracts

let signature_contracts signature =
  let contracts = ref [] in
  List.iter
    (fun item ->
      match item.psig_desc with
      | Psig_value value ->
          let name = value.pval_name.txt in
          let value_level = level_on_attributes value.pval_attributes in
          contracts :=
            add_value_contract !contracts ~loc:value.pval_loc ~name value_level;
          contracts := remove_function_contracts name !contracts;
          let formals = signature_formals value.pval_type in
          if formals <> [] then
            contracts :=
              add_function_contracts !contracts ~loc:value.pval_loc ~name
                ~value_level formals
      | Psig_type (_, declarations) ->
          List.iter
            (fun declaration ->
              if
                not
                  (Contract.is_internal_name declaration.ptype_name.txt)
              then contracts := add_type_contracts !contracts declaration)
            declarations
      | _ -> ())
    signature;
  List.rev !contracts

let with_module_abi ~loc contracts =
  let entries =
    List.sort
      (fun left right ->
        match String.compare left.marker right.marker with
        | 0 -> String.compare left.contract right.contract
        | order -> order)
      contracts
    |> List.map (fun contract ->
           Contract.abi_component "contract"
             [ contract.marker; contract.contract ])
  in
  { marker =
      Contract.marker_name ~kind:"module_abi"
        ~owner:(Contract.compilation_unit_owner loc) ();
    contract = Contract.abi_contract "module" entries;
    loc }
  :: contracts

let structure_type_declarations structure =
  List.concat_map
    (fun item ->
      match item.pstr_desc with
      | Pstr_type (_, declarations) -> declarations
      | _ -> [])
    structure

let signature_type_declarations signature =
  List.concat_map
    (fun item ->
      match item.psig_desc with
      | Psig_type (_, declarations) -> declarations
      | _ -> [])
    signature

class rewriter ~static_level ~binding_only_pattern_components =
  object (self)
    inherit Ast_traverse.map as super

    val contract_root = ref true
    val signature_root = ref true

    method private mapped_structure_item item =
      with_ref contract_root false (fun () ->
          with_ref signature_root false (fun () -> super#structure_item item))

    method private mapped_signature_item item =
      with_ref contract_root false (fun () ->
          with_ref signature_root false (fun () -> super#signature_item item))

    method private keep level = survives ~static_level level

    method private expression_component expression =
      match level_on_expression expression with
      | Some level when not (self#keep level) -> None
      | Some _ | None -> Some (self#expression expression)

    method private pattern_component pattern =
      match level_on_pattern pattern with
      | Some level when not (self#keep level) ->
          if List.mem pattern.ppat_loc binding_only_pattern_components then
            Some (ppat_any ~loc:pattern.ppat_loc)
          else None
      | Some _ | None -> Some (self#pattern pattern)

    method! structure structure =
      let declarations = structure_type_declarations structure in
      List.iter Contract.reject_reserved_source_declaration declarations;
      let loc =
        match structure with item :: _ -> item.pstr_loc | [] -> Location.none
      in
      let raw_contracts = structure_contracts structure in
      let raw_contracts =
        if !contract_root || raw_contracts <> [] then
          with_module_abi ~loc raw_contracts
        else raw_contracts
      in
      let contracts =
        List.filter
          (fun contract ->
            Contract.should_emit_contract declarations ~name:contract.marker
              ~loc:contract.loc)
          raw_contracts
      in
      let structure =
        List.filter_map
          (fun item ->
            match item.pstr_desc with
            | Pstr_value (rec_flag, bindings) ->
                let marked =
                  List.filter
                    (fun binding -> level_on_binding binding <> None)
                    bindings
                in
                if
                  marked <> []
                  && (rec_flag = Recursive || List.length bindings <> 1)
                then
                  Location.raise_errorf ~loc:item.pstr_loc
                    "a top-level level-gated declaration must be one nonrecursive binding";
                (match bindings with
                | [ binding ] -> (
                    match level_on_binding binding with
                    | Some level when not (self#keep level) -> None
                    | Some _ | None -> Some (self#mapped_structure_item item))
                | _ -> Some (self#mapped_structure_item item))
            | _ -> Some (self#mapped_structure_item item))
          structure
      in
      structure
      @ List.map
          (fun contract ->
            Contract.structure_contract ~loc:contract.loc ~name:contract.marker
              ~contract:contract.contract)
          contracts

    method! signature signature =
      let declarations = signature_type_declarations signature in
      List.iter Contract.reject_reserved_source_declaration declarations;
      let loc =
        match signature with item :: _ -> item.psig_loc | [] -> Location.none
      in
      let raw_contracts = signature_contracts signature in
      let raw_contracts =
        if !signature_root || raw_contracts <> [] then
          with_module_abi ~loc raw_contracts
        else raw_contracts
      in
      let contracts =
        List.filter
          (fun contract ->
            Contract.should_emit_contract declarations ~name:contract.marker
              ~loc:contract.loc)
          raw_contracts
      in
      let signature =
        List.filter_map
          (fun item ->
            match item.psig_desc with
            | Psig_value value -> (
                match level_on_attributes value.pval_attributes with
                | Some level when not (self#keep level) -> None
                | Some _ | None -> Some (self#mapped_signature_item item))
            | _ -> Some (self#mapped_signature_item item))
          signature
      in
      signature
      @ List.map
          (fun contract ->
            Contract.signature_contract ~loc:contract.loc ~name:contract.marker
              ~contract:contract.contract)
          contracts

    method! value_binding binding =
      let _, attributes = split_level_attributes binding.pvb_attributes in
      super#value_binding { binding with pvb_attributes = attributes }

    method! value_description value =
      let formals = signature_formals value.pval_type in
      reject_fully_gated_callable ~loc:value.pval_loc (Some formals);
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
                     | Some _ | None ->
                         Some (self#core_type argument))
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
      | Ptyp_tuple components ->
          let components =
            List.filter_map
              (fun component ->
                match level_on_type component with
                | Some level when not (self#keep level) -> None
                | Some _ | None -> Some (self#core_type component))
              components
          in
          (match components with
          | [] -> ptyp_constr ~loc:typ.ptyp_loc { txt = Lident "unit"; loc = typ.ptyp_loc } []
          | [ component ] -> component
          | components -> { typ with ptyp_desc = Ptyp_tuple components })
      | Ptyp_arrow (label, argument, result) ->
          let level = level_on_type argument in
          let result = self#core_type result in
          (match level with
          | Some level when not (self#keep level) -> result
          | Some _ | None ->
              { typ with
                ptyp_desc =
                  Ptyp_arrow
                    (label, self#core_type argument, result) })
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
                    List.filter_map
                      (fun component -> self#expression_component component)
                      components
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
              | _ ->
                  { expression with
                    pexp_desc =
                      Pexp_construct
                        (constructor, self#expression_component argument) })
          | Pexp_tuple components ->
              let components =
                List.filter_map
                  (fun component -> self#expression_component component)
                  components
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
                    List.filter_map
                      (fun component -> self#pattern_component component)
                      components
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
              | _ ->
                  let argument =
                    Option.map
                      (fun argument -> (vars, argument))
                      (self#pattern_component argument)
                  in
                  { pattern with
                    ppat_desc = Ppat_construct (constructor, argument) })
          | Ppat_tuple components ->
              let components =
                List.filter_map
                  (fun component -> self#pattern_component component)
                  components
              in
              (match components with
              | [] -> ppat_any ~loc:pattern.ppat_loc
              | [ component ] -> component
              | components ->
                  { pattern with ppat_desc = Ppat_tuple components })
          | _ -> super#pattern pattern)
  end

let rewrite ~static_level structure =
  let validator = new validator ~catalog:(collect_catalog structure) in
  validator#structure structure;
  (new rewriter ~static_level
     ~binding_only_pattern_components:validator#binding_only_pattern_components)
    #structure structure
  @ Contract.witness_structure validator#witnesses

let rewrite_signature ~static_level signature =
  (new rewriter ~static_level ~binding_only_pattern_components:[])#signature
    signature
