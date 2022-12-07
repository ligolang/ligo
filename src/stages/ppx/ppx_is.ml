open Ppxlib
module List = ListLabels
open Ast_builder.Default

let ppx_name = "is"

let constructor_impl ~tag (cd : constructor_declaration) =
  let loc = cd.pcd_loc in
  let { txt; loc = cloc } = cd.pcd_name in
  let rhs =
    if List.mem
         ~set:(List.map ~f:(fun { attr_name; _ } -> attr_name.txt) cd.pcd_attributes)
         tag
    then [%expr true]
    else [%expr false]
  in
  let pat =
    match cd.pcd_args with
    | Pcstr_tuple [] | Pcstr_record [] -> None
    | _ -> Some (ppat_any ~loc)
  in
  case ~lhs:(ppat_construct ~loc { loc = cloc; txt = Lident txt } pat) ~guard:None ~rhs


let func_of_cases ~loc ~prefix ~tag (cs : cases) =
  let func_name = prefix ^ "is_" ^ tag in
  pstr_value
    ~loc
    Nonrecursive
    [ { pvb_pat = ppat_var ~loc { loc; txt = func_name }
      ; pvb_expr =
          pexp_fun
            ~loc
            Nolabel
            None
            (ppat_var ~loc { loc; txt = "x" })
            (pexp_match ~loc (pexp_ident ~loc { loc; txt = lident "x" }) cs)
      ; pvb_attributes = []
      ; pvb_loc = loc
      }
    ]


let generate_impl
    ~ctxt
    (_rec_flag, type_declarations)
    (params : expression list option)
    (name : expression option)
  =
  let to_prefix s = if String.equal s "t" then "" else s ^ "_" in
  let params = Option.value ~default:[] params in
  let f = function
    | { pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ } -> s
    | _ -> Location.raise_errorf "Need strings in tags"
  in
  let tags = List.map ~f params in
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let f tag =
    List.map type_declarations ~f:(fun (td : type_declaration) ->
        match td with
        | { ptype_kind = Ptype_abstract | Ptype_record _ | Ptype_open; _ } ->
          Location.raise_errorf ~loc "Cannot derive accessors for non variant types"
        | { ptype_kind = Ptype_variant constructors; ptype_name; _ } ->
          let prefix =
            match name with
            | None -> to_prefix ptype_name.txt
            | Some { pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ } -> s ^ "_"
            | _ -> Location.raise_errorf "Need string in name"
          in
          prefix, List.map constructors ~f:(constructor_impl ~tag))
    |> List.map ~f:(fun (prefix, cases) -> func_of_cases ~prefix ~loc ~tag cases)
  in
  List.map ~f tags |> List.concat


let impl_generator =
  let prefix = Deriving.Args.(empty +> arg "tags" (elist __) +> arg "name" __) in
  Deriving.Generator.V2.make prefix generate_impl


let my_deriver = Deriving.add ppx_name ~str_type_decl:impl_generator
