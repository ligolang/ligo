module P = Ppxlib
module A = P.Ast_builder.Default
module W = Woo_types

module type PARAMS = sig
  val location : P.location
end

module Generate (Params : PARAMS) = struct
  let loc = Params.location
  let failwith str = P.Location.raise_errorf ~loc str
  let lident label = A.Located.lident ~loc label
  let e_true = P.([%expr true])
  let e_false = P.([%expr false])
  let e_bool b = if b then e_true else e_false
  let e_unit = P.([%expr ()])
  let e_var x = A.evar ~loc x

  let var_names prefix n =
    List.map ~f:(fun i -> prefix ^ string_of_int i) @@ Base.List.range 0 n


  let e_vars prefix n = List.map ~f:e_var @@ var_names prefix n
  let e_string str = A.estring ~loc str
  let e_var_n n = e_var (Int.to_string n)

  let e_tuple lst =
    match lst with
    | [] -> e_unit
    | [ single ] -> single
    | lst -> A.pexp_tuple ~loc lst


  let p_var x = A.pvar ~loc x

  let p_record_pun lst =
    A.ppat_record ~loc (List.map ~f:(fun (x : string) -> lident x, p_var x) lst) Closed


  let e_record lst =
    A.pexp_record ~loc (List.map ~f:(fun (var, expr) -> lident var, expr) lst) None


  let e_with record property content =
    A.pexp_record ~loc [ lident property, content ] (Some (e_var record))


  let e_fun ?(label = P.Nolabel) ?default var body =
    A.pexp_fun ~loc label default (p_var var) body


  let e_funs vars body = List.fold_right ~f:e_fun vars ~init:body

  let e_fun_pat ?(label = P.Nolabel) ?default pat body =
    A.pexp_fun ~loc label default pat body


  let e_named_fun var body =
    let label = P.Labelled var in
    e_fun ~label var body


  let e_named_funs vars body = List.fold_right ~f:e_named_fun vars ~init:body

  let e_option_fun var ~default body =
    let label = P.Optional var in
    e_fun ~label ~default var body


  let t_unit = P.([%type: unit])
  let t_tuple lst = A.ptyp_tuple ~loc lst
  let e_property ~record ~(label : string) = A.pexp_field ~loc record (lident label)

  let label_to_variable : W.label -> string =
   fun str ->
    let s = String.lowercase str in
    if P.Keyword.is_keyword s then s ^ "_" else s


  let d_value x expr =
    let pat = p_var @@ label_to_variable x in
    let declaration = A.value_binding ~loc ~expr ~pat in
    A.pstr_value ~loc Nonrecursive [ declaration ]


  (* Not fit for variant declarations *)
  let abstract_type_declaration ?(params = []) ?(private_ = P.Public) name body =
    let manifest = Some body in
    let name = Location.mkloc name loc in
    let declaration =
      A.type_declaration
        ~loc
        ~params
        ~name
        ~cstrs:[]
        ~kind:P.Ptype_abstract
        ~private_
        ~manifest
    in
    let declarations = A.pstr_type ~loc Nonrecursive [ declaration ] in
    declarations


  let declaration ~name ~body =
    let pat = p_var name in
    let expr = body in
    let declaration = A.value_binding ~loc ~expr ~pat in
    let declarations = A.pstr_value ~loc Nonrecursive [ declaration ] in
    declarations


  let p_constructor ~polymorphic ?pattern label =
    if polymorphic
    then A.ppat_variant ~loc label pattern
    else A.ppat_construct ~loc (A.Located.lident ~loc label) pattern


  let e_constructor ~polymorphic ?body label =
    if polymorphic
    then A.pexp_variant ~loc label body
    else A.pexp_construct ~loc (A.Located.lident ~loc label) body


  let e_apply f x = A.pexp_apply ~loc f [ Nolabel, x ]

  let e_constructors ?(unit = false) ~polymorphic label lst =
    let apply body =
      if polymorphic
      then A.pexp_variant ~loc label body
      else A.pexp_construct ~loc (A.Located.lident ~loc label) body
    in
    match List.length lst = 0, unit with
    | false, _ -> apply @@ Some (e_tuple lst)
    | true, true -> apply @@ Some e_unit
    | true, false -> apply None


  let e_applies ?(unit = false) f lst =
    match List.length lst = 0, unit with
    | false, _ -> e_apply f @@ e_tuple lst
    | true, true -> e_apply f @@ e_unit
    | true, false -> f


  let e_match
      :  polymorphic:bool -> P.expression -> (string * string list * P.expression) list
      -> P.expression
    =
   fun ~polymorphic matchee lst ->
    let case : string * string list * P.expression -> P.case =
     fun (constructor, params, rhs) ->
      let lhs =
        if List.length params = 0
        then p_constructor ~polymorphic constructor
        else (
          let pattern =
            if polymorphic
            then p_var @@ List.hd_exn params
            else (
              let patterns = List.map ~f:(fun name -> A.pvar ~loc name) params in
              A.ppat_tuple ~loc patterns)
          in
          p_constructor ~polymorphic ~pattern constructor)
      in
      A.case ~lhs ~rhs ~guard:None
    in
    A.pexp_match ~loc matchee @@ List.map ~f:case lst


  let extract_core_type = function
    | W.T_variant _ -> failwith "ez doesn't support inline variants"
    | W.T_record _ -> failwith "ez doesn't support inline records"
    | W.T_core ct -> ct


  let is_tauto_type_declaration : P.structure_item -> bool =
   fun si ->
    match si.pstr_desc with
    | Pstr_type (_, [ single ]) ->
      (match single.ptype_params, single.ptype_cstrs, single.ptype_manifest with
      | [], [], Some ty ->
        let name = single.ptype_name.txt in
        (match ty.ptyp_desc with
        | Ptyp_constr (ident, _) ->
          (match ident.txt with
          | Lident var -> String.equal name var
          | _ -> false)
        | _ -> false)
      | _ -> false)
    | _ -> false
end
