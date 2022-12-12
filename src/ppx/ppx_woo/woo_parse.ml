module P = Ppxlib
module A = P.Ast_builder.Default
module W = Woo_types

let get_default_value_opt : P.attribute -> P.expression option =
 fun attr ->
  let P.{ attr_payload = pl; attr_name = name; _ } = attr in
  let ( >>? ) v f = Option.bind v ~f in
  (if String.equal name.txt "default" then Some () else None)
  >>? fun () ->
  (match pl with
  | P.PStr items -> Some items
  | _ -> None)
  >>? fun items ->
  (match items with
  | [ default ] -> Some default
  | _ -> None)
  >>? fun default ->
  match default.pstr_desc with
  | Pstr_eval (expr, _) -> Some expr
  | _ -> None


let labelled_record_field index : P.label_declaration -> W.labelled_record_field =
 fun ld ->
  let label = ld.pld_name.txt in
  let ty = ld.pld_type in
  let attributes = ld.pld_attributes in
  let default_value = Base.List.find_map ~f:get_default_value_opt attributes in
  label, W.record_field ?default_value index (W.T_core ty)


let record : P.label_declaration list -> W.type_expression =
 fun lds ->
  let record_fields = List.mapi ~f:labelled_record_field lds in
  T_record (W.r_decls record_fields)


let type_declaration ?non_recursive : P.type_declaration -> W.type_declaration =
 fun td ->
  let label = td.ptype_name.txt in
  let body =
    match td.ptype_kind with
    | P.Ptype_variant cds ->
      let aux : P.constructor_declaration -> string * W.type_expression list =
       fun cd ->
        let constructor = cd.pcd_name.txt in
        let args =
          match cd.pcd_args with
          | P.Pcstr_tuple tys -> List.map ~f:(fun ty -> W.T_core ty) tys
          | P.Pcstr_record lds -> [ record lds ]
        in
        constructor, args
      in
      let polymorphic = false in
      let constructor_declarations = W.c_decls @@ List.map ~f:aux cds in
      W.T_variant { polymorphic; constructor_declarations }
    | P.Ptype_record lds -> record lds
    | P.Ptype_abstract ->
      (match td.ptype_manifest with
      | Some ct -> W.T_core ct
      | None ->
        failwith "parse_type_declaration: unknown case PType_abstract and no manifest")
    | P.Ptype_open -> failwith "parse_type_declaration: unknown case PType_open"
  in
  W.type_declaration ?non_recursive (label, body)


let type_declarations ?non_recursive : P.type_declaration list -> W.type_declarations =
 fun tds -> List.map ~f:(type_declaration ?non_recursive) tds


let non_recursive : P.rec_flag -> W.non_recursive =
 fun rf ->
  match rf with
  | P.Nonrecursive -> true
  | P.Recursive -> false
