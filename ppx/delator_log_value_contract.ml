open Ppxlib
open Ast_builder.Default

type slot_key = Positional of int | Named of string

type relation = Exact | Compatible

type witness = {
  contract_path : Longident.t;
  expected : string;
  relation : relation;
  loc : Location.t;
}

let encode_name name =
  let buffer = Buffer.create (2 * String.length name) in
  String.iter
    (fun character -> Buffer.add_string buffer (Printf.sprintf "%02x" (Char.code character)))
    name;
  Buffer.contents buffer

let slot_suffix = function
  | Positional position -> Printf.sprintf "position_%d" position
  | Named name -> "label_" ^ encode_name name

let marker_name ~kind ~owner ?slot () =
  String.concat "_"
    ([ "delator"; "internal"; "log"; "value"; kind; encode_name owner ]
    @
    match slot with None -> [] | Some slot -> [ slot_suffix slot ])

let frame value = string_of_int (String.length value) ^ ":" ^ value

let abi_component kind components =
  frame kind ^ String.concat "" (List.map frame components)

let abi_contract kind components =
  "abi_" ^ encode_name (abi_component kind components)

let is_internal_name name =
  let prefix = "delator_internal_log_value_" in
  String.length name >= String.length prefix
  && String.sub name 0 (String.length prefix) = prefix

let tag ~loc contract =
  rtag ~loc { txt = "Delator_log_value_" ^ contract; loc } true []

let availability = function
  | "trace" -> [ "trace" ]
  | "debug" -> [ "trace"; "debug" ]
  | "info" -> [ "trace"; "debug"; "info" ]
  | "warn" -> [ "trace"; "debug"; "info"; "warn" ]
  | "error" -> [ "trace"; "debug"; "info"; "warn"; "error" ]
  | contract -> [ contract ]

let contract_type ~loc contract =
  ptyp_variant ~loc (List.map (tag ~loc) (availability contract)) Closed None

let compatible_type ~loc contract =
  ptyp_variant ~loc [ tag ~loc contract ] Open None

let declaration ~loc ~name ~params ~cstrs manifest =
  let declaration =
    type_declaration ~loc ~name:{ txt = name; loc } ~params ~cstrs
      ~kind:Ptype_abstract ~private_:Public ~manifest:(Some manifest)
  in
  let warning =
    attribute ~loc ~name:{ txt = "warning"; loc }
      ~payload:(PStr [ pstr_eval ~loc (estring ~loc "-34") [] ])
  in
  { declaration with ptype_attributes = warning :: declaration.ptype_attributes }

let structure_contract ~loc ~name ~contract =
  pstr_type ~loc Nonrecursive
    [ declaration ~loc ~name ~params:[] ~cstrs:[] (contract_type ~loc contract) ]

let signature_contract ~loc ~name ~contract =
  psig_type ~loc Nonrecursive
    [ declaration ~loc ~name ~params:[] ~cstrs:[] (contract_type ~loc contract) ]

let is_generated_contract declaration =
  let name_prefix = "delator_internal_log_value_" in
  let tag_prefix = "Delator_log_value_" in
  String.length declaration.ptype_name.txt >= String.length name_prefix
  && String.sub declaration.ptype_name.txt 0 (String.length name_prefix)
     = name_prefix
  && List.exists
       (fun attribute -> String.equal attribute.attr_name.txt "warning")
       declaration.ptype_attributes
  &&
  match declaration.ptype_manifest with
  | Some { ptyp_desc = Ptyp_variant (rows, Closed, None); _ } ->
      rows <> []
      && List.for_all
           (fun row ->
             match row.prf_desc with
             | Rtag ({ txt = tag; _ }, true, []) ->
                 String.length tag >= String.length tag_prefix
                 && String.sub tag 0 (String.length tag_prefix) = tag_prefix
             | _ -> false)
           rows
  | _ -> false

let should_emit_contract declarations ~name ~loc =
  match
    List.find_opt
      (fun declaration -> String.equal declaration.ptype_name.txt name)
      declarations
  with
  | None -> true
  | Some declaration when is_generated_contract declaration -> false
  | Some _ ->
      Location.raise_errorf ~loc
        "type name %s is reserved for Delator log-value contracts" name

let witness_structure witnesses =
  let rec loop index = function
    | [] -> []
    | witness :: rest ->
        let loc = witness.loc in
        let witness_name =
          Printf.sprintf "delator_internal_log_value_witness_%d" index
        in
        let check_name =
          Printf.sprintf "delator_internal_log_value_check_%d" index
        in
        let parameter = ptyp_var ~loc "delator_contract" in
        let contract =
          ptyp_constr ~loc { txt = witness.contract_path; loc } []
        in
        let witness_declaration =
          declaration ~loc ~name:witness_name
            ~params:[ (parameter, (NoVariance, NoInjectivity)) ]
            ~cstrs:[ (parameter, contract, loc) ]
            (ptyp_constr ~loc { txt = Longident.Lident "unit"; loc } [])
        in
        let check_declaration =
          let expected =
            match witness.relation with
            | Exact -> contract_type ~loc witness.expected
            | Compatible -> compatible_type ~loc witness.expected
          in
          declaration ~loc ~name:check_name ~params:[] ~cstrs:[]
            (ptyp_constr ~loc { txt = Longident.Lident witness_name; loc }
               [ expected ])
        in
        pstr_type ~loc Nonrecursive [ witness_declaration ]
        :: pstr_type ~loc Nonrecursive [ check_declaration ]
        :: loop (index + 1) rest
  in
  loop 0 witnesses
