open Ppxlib
module List = ListLabels
open Ast_builder.Default
open Core

let ppx_name = "read_constant"
let func_name = "read_constant'"

let rec get_payload tag = function
  | [] -> None
  | { attr_name; attr_payload; _ } :: attributes ->
    if String.equal attr_name.txt tag
    then (
      match attr_payload with
      | PStr
          [ { pstr_desc =
                Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ }, _)
            ; _
            }
          ] -> Some s
      | _ -> None)
    else get_payload tag attributes


let constructor_impl (cd : constructor_declaration) =
  let loc = cd.pcd_loc in
  let { txt; loc = cloc } = cd.pcd_name in
  let s =
    match get_payload "read" cd.pcd_attributes with
    | Some s -> s
    | None -> if String.is_prefix ~prefix:"C_" txt then String.drop_prefix txt 2 else txt
  in
  let _s = estring ~loc s in
  let rhs = econstruct cd None in
  let pat = ppat_constant ~loc:cloc (Pconst_string (s, cloc, None)) in
  case ~lhs:pat ~guard:None ~rhs:[%expr Some [%e rhs]]


let func_of_cases ~loc (cs : cases) =
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


let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun (td : type_declaration) ->
      match td with
      | { ptype_kind = Ptype_abstract | Ptype_record _ | Ptype_open; _ } ->
        Location.raise_errorf ~loc "Cannot derive accessors for non variant types"
      | { ptype_kind = Ptype_variant constructors; _ } ->
        List.map constructors ~f:constructor_impl)
  |> List.map ~f:(fun xs ->
         List.append xs [ case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:[%expr None] ])
  |> List.map ~f:(func_of_cases ~loc)


let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let my_deriver = Deriving.add ppx_name ~str_type_decl:impl_generator
