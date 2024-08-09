open Ocaml_common
open Asttypes
open Types
open Typedtree
open Ligo_prim
open Ast_core
open Caml_core

(* TODO: check all assert and failwith *)

let error_unimplemented () = failwith "unimplemented"
let error_unsupported () = failwith "unsupported"
let error_unreachable () = failwith "unreachable"

(* TODO: ideally this would not be needed  *)
let split_arrow ~exp_env ~label type_ =
  (* TODO: is this guaranteed to not fail? *)
  Ctype.filter_arrow exp_env type_ label


let pat_wrap type_ desc = { pat_desc = desc; pat_type = type_ }
let var_pat_wrap type_ desc = { var_pat_desc = desc; var_pat_type = type_ }
let expr_wrap type_ desc = { expr_desc = desc; expr_type = type_ }
let decl_wrap desc = { decl_desc = desc }

let rec compile_type type_ =
  let open Ocaml_common.Types in
  (* TODO: rectypes *)
  match get_desc type_ with
  (* polymorphism *)
  | Tvar name | Tunivar name ->
    assert (get_level type_ = Btype.generic_level);
    T_var (name, get_id type_)
  | Tpoly (type_, bound) -> error_unreachable ()
  (* type constructors *)
  | Tconstr (path, args, _abbrev) ->
    let args = List.map ~f:(fun arg -> compile_type arg) args in
    T_constr (path, args)
  (* arrow *)
  | Tarrow (Nolabel, type1, type2, _comm) ->
    let type1 = compile_type type1 in
    let type2 = compile_type type2 in
    (* TODO: what about param_names? *)
    T_arrow (type1, type2)
  | Tarrow (Labelled _, _type1, _type2, _comm) -> error_unimplemented ()
  | Tarrow (Optional _, _type1, _type2, _comm) -> error_unimplemented ()
  (* tuple *)
  | Ttuple fields ->
    let fields = List.map fields ~f:(fun field -> compile_type field) in
    T_tuple fields
  (* variants *)
  | Tvariant _ -> error_unsupported ()
  (* first-class modules *)
  | Tpackage _ -> error_unsupported ()
  (* objects *)
  | Tobject _ -> error_unsupported ()
  | Tfield _ -> error_unsupported ()
  | Tnil -> error_unsupported ()
  (* machinery *)
  | Tlink _ -> error_unreachable ()
  | Tsubst (_, _) -> error_unreachable ()


let compile_type_declaration decl =
  let { type_params
      ; type_arity
      ; type_kind
      ; type_private
      ; type_manifest
      ; type_variance
      ; type_separability
      ; type_is_newtype (* TODO: use this? *)
      ; type_expansion_scope = _
      ; type_loc
      ; type_attributes
      ; type_immediate = _
      ; type_unboxed_default
      ; type_uid = _
      }
    =
    decl
  in
  assert (List.is_empty type_params);
  assert (type_arity = 0);
  assert (
    match type_private with
    | Private -> false
    | Public -> true);
  assert (List.is_empty type_variance);
  assert (List.is_empty type_separability);
  (* TODO: support new type *)
  assert (not type_is_newtype);
  assert (List.is_empty type_attributes);
  assert (not type_unboxed_default);
  match type_kind, type_manifest with
  | Type_abstract, Some manifest ->
    let manifest = compile_type manifest in
    T_alias manifest
  | Type_abstract, None ->
    (* TODO: useful for aliasing *)
    error_unimplemented ()
  | Type_record (labels, Record_regular), None ->
    (* TODO: high priority *)
    let labels =
      List.map labels ~f:(fun label ->
          let { ld_id; ld_mutable; ld_type; ld_loc; ld_attributes; ld_uid = _ } = label in
          assert (
            match ld_mutable with
            | Immutable -> true
            | Mutable -> false);
          assert (List.is_empty ld_attributes);
          let type_ = compile_type ld_type in
          ld_id, type_)
    in
    T_record labels
  | Type_record (_, Record_regular), Some _ -> error_unimplemented ()
  | ( Type_record
        (_, (Record_float | Record_unboxed _ | Record_inlined _ | Record_extension _))
    , (Some _ | None) ) -> error_unimplemented ()
  | Type_variant (_, _), (Some _ | None) ->
    (* TODO: high priority *)
    error_unimplemented ()
  | Type_open, (Some _ | None) -> error_unimplemented ()


let compile_literal constant =
  let open Literal_value in
  match constant with
  | Const_int n ->
    let n = Z.of_int n in
    Literal_int n
  | Const_char _ -> error_unsupported ()
  | Const_string (content, _loc, None) ->
    (* TODO: use string loc? *)
    (* TODO: standard vs verbatin *)
    Literal_string (Standard content)
  | Const_string (_content, _loc, Some _tag) -> error_unsupported ()
  | Const_float _ -> error_unsupported ()
  | Const_int32 _ -> error_unsupported ()
  | Const_int64 _ -> error_unsupported ()
  | Const_nativeint _ -> error_unsupported ()


let compile_var_pat : type a. a general_pattern -> var_pat =
 fun pat ->
  let { pat_desc; pat_loc; pat_extra; pat_type; pat_env; pat_attributes } = pat in
  assert (List.is_empty pat_extra);
  assert (List.is_empty pat_attributes);
  let pat_type = compile_type pat_type in
  match pat_desc with
  | Tpat_any -> error_unsupported ()
  | Tpat_var (ident, _label) -> var_pat_wrap pat_type @@ ident
  (* TODO: this one seems relatively easy *)
  | Tpat_alias _ -> error_unsupported ()
  | Tpat_constant _ -> error_unsupported ()
  | Tpat_tuple _ -> error_unsupported ()
  | Tpat_construct _ -> error_unsupported ()
  | Tpat_variant _ -> error_unsupported ()
  | Tpat_record _ -> error_unsupported ()
  | Tpat_array _ -> error_unsupported ()
  | Tpat_lazy _ -> error_unsupported ()
  | Tpat_or _ -> error_unsupported ()
  | Tpat_value _ -> error_unsupported ()
  | Tpat_exception _ -> error_unsupported ()


let rec compile_pat : type a. a general_pattern -> pat =
 fun pat ->
  let { pat_desc; pat_loc; pat_extra; pat_type; pat_env; pat_attributes } = pat in
  assert (List.is_empty pat_extra);
  assert (List.is_empty pat_attributes);
  let pat_type = compile_type pat_type in
  match pat_desc with
  | Tpat_any -> error_unimplemented ()
  | Tpat_var (ident, _label) -> pat_wrap pat_type @@ P_var ident
  | Tpat_alias (_, _, _) ->
    (* TODO: this one seems relatively easy *)
    error_unimplemented ()
  | Tpat_constant _ -> error_unsupported ()
  | Tpat_tuple fields ->
    let fields = List.map fields ~f:(fun field -> compile_pat field) in
    pat_wrap pat_type @@ P_tuple fields
  | Tpat_construct (_, constructor, payload, None) ->
    let payload = List.map payload ~f:(fun field -> compile_pat field) in
    let payload =
      match payload with
      | [] -> pat_wrap pat_type @@ P_unit
      | [ payload ] -> payload
      | payload -> pat_wrap pat_type @@ P_tuple payload
    in
    pat_wrap pat_type @@ P_variant (constructor, payload)
  | Tpat_construct (_, _, _, Some _) ->
    (* TODO: weird cases, likely should be supported *)
    error_unimplemented ()
  | Tpat_variant (_, _, _) -> error_unsupported ()
  | Tpat_record (labels, Closed) ->
    (* TODO: priority *)
    let labels =
      List.map labels ~f:(fun (_lident, label, pat) -> label, compile_pat pat)
    in
    pat_wrap pat_type @@ P_record labels
  | Tpat_record (_labels, Open) ->
    (* TODO: priority, but linearity *)
    error_unimplemented ()
  | Tpat_array _ -> error_unsupported ()
  | Tpat_lazy _ -> error_unsupported ()
  | Tpat_or (_, _, _) ->
    (* TODO: how hard would this one be? *)
    error_unsupported ()
  | Tpat_value _ -> error_unsupported ()
  | Tpat_exception _ -> error_unsupported ()


let rec compile_expr expr =
  let { exp_desc; exp_loc; exp_extra; exp_type; exp_env; exp_attributes } = expr in
  assert (List.is_empty exp_extra);
  assert (List.is_empty exp_attributes);
  let type_ = compile_type exp_type in
  match exp_desc with
  | Texp_ident (path, _lident, _desc) -> expr_wrap type_ @@ E_var path
  | Texp_constant constant ->
    let literal = compile_literal constant in
    expr_wrap type_ @@ E_literal literal
  | Texp_let (Recursive, [ value ], body) ->
    let { vb_pat; vb_expr; vb_attributes; vb_loc } = value in
    assert (List.is_empty vb_attributes);
    (* TODO: locs *)
    (* TODO: attributes *)
    (* TODO: recursive *)
    let var_pat = compile_var_pat vb_pat in
    (* TODO: will the type of this expression be mono? *)
    let value =
      (* TODO: poly value *)
      compile_expr vb_expr
    in
    let body = compile_expr body in
    expr_wrap type_ @@ E_let_rec (var_pat, value, body)
  | Texp_let (Nonrecursive, [ value ], body) ->
    (* TODO: duplicated *)
    let { vb_pat; vb_expr; vb_attributes; vb_loc } = value in
    assert (List.is_empty vb_attributes);
    (* TODO: locs *)
    (* TODO: attributes *)
    (* TODO: recursive *)
    let pat = compile_pat vb_pat in
    (* TODO: will the type of this expression be mono? *)
    let value =
      (* TODO: poly value *)
      compile_expr vb_expr
    in
    let body = compile_expr body in
    expr_wrap type_ @@ E_let (pat, value, body)
  | Texp_let (Recursive, _, _) -> error_unimplemented ()
  | Texp_let (Nonrecursive, _, _) -> error_unimplemented ()
  (* TODO: label, exp function *)
  | Texp_function { arg_label = Nolabel; param; cases; partial = Total } ->
    (* TODO: test both, multiple cases and  *)
    (* TODO: type function *)
    let param_type, body_type = split_arrow ~exp_env ~label:Nolabel exp_type in
    let param_type = compile_type param_type in
    let body_type = compile_type body_type in
    let body =
      let matchee = expr_wrap param_type @@ E_var (Pident param) in
      let cases = List.map cases ~f:(fun case -> compile_case case) in
      expr_wrap body_type @@ E_match (matchee, cases)
    in
    let param = var_pat_wrap param_type @@ param in
    expr_wrap type_ @@ E_lambda (param, body)
  | Texp_function { arg_label = Nolabel; partial = Partial; _ } -> error_unimplemented ()
  | Texp_function { arg_label = Labelled _ | Optional _; _ } -> error_unimplemented ()
  | Texp_apply (lambda, args) ->
    let lambda = compile_expr lambda in
    let args =
      List.map
        ~f:(fun (label, arg) ->
          match label, arg with
          | Nolabel, Some arg -> compile_expr arg
          | Nolabel, None -> error_unimplemented ()
          | (Labelled _ | Optional _), _ -> error_unsupported ())
        args
    in
    expr_wrap type_ @@ E_apply (lambda, args)
  | Texp_match (_, _, Partial) -> error_unimplemented ()
  | Texp_match (matchee, cases, Total) ->
    (* TODO: disc_label *)
    let matchee = compile_expr matchee in
    let cases = List.map cases ~f:(fun case -> compile_case case) in
    expr_wrap type_ @@ E_match (matchee, cases)
  | Texp_try (_, _) -> error_unsupported ()
  | Texp_tuple fields ->
    let fields = List.map fields ~f:(fun field -> compile_expr field) in
    let fields =
      match fields with
      | [] -> error_unreachable ()
      | field :: fields -> Ne_list.(field :: fields)
    in
    expr_wrap type_ @@ E_tuple fields
  | Texp_construct (_, _, _) -> error_unimplemented ()
  | Texp_variant (_, _) -> error_unsupported ()
  | Texp_record { fields; representation; extended_expression } ->
    assert (Option.is_none extended_expression);
    let () =
      match representation with
      | Record_regular -> ()
      | Record_float | Record_unboxed _ | Record_inlined _ | Record_extension _ ->
        error_unimplemented ()
    in
    let fields = Array.to_list fields in
    let fields = List.map fields ~f:(fun field -> compile_field field) in
    expr_wrap type_ @@ E_record fields
  | Texp_field (record, _, description) ->
    let record = compile_expr record in
    (* TODO: check / use all fields *)
    let { lbl_name
        ; lbl_res = _
        ; lbl_arg = _
        ; lbl_mut
        ; lbl_pos = _
        ; lbl_all
        ; lbl_repres = _
        ; lbl_private
        ; lbl_loc
        ; lbl_attributes
        ; lbl_uid = _
        }
      =
      description
    in
    let () =
      match lbl_mut with
      | Immutable -> ()
      | Mutable -> error_unimplemented ()
    in
    let () =
      match lbl_private with
      | Public -> ()
      | Private -> error_unimplemented ()
    in
    expr_wrap type_ @@ E_field (record, lbl_name)
  | Texp_setfield (_, _, _, _) ->
    (* TODO: mutation *)
    error_unsupported ()
  | Texp_array _ -> error_unsupported ()
  | Texp_ifthenelse (_, _, _) ->
    (* TODO: priority *)
    error_unimplemented ()
  | Texp_sequence (_, _) ->
    (* TODO: support this? *)
    error_unimplemented ()
  | Texp_while (_, _) -> error_unimplemented ()
  | Texp_for (_, _, _, _, _, _) -> error_unimplemented ()
  | Texp_send (_, _) -> error_unsupported ()
  | Texp_new (_, _, _) -> error_unsupported ()
  | Texp_instvar (_, _, _) -> error_unsupported ()
  | Texp_setinstvar (_, _, _, _) -> error_unsupported ()
  | Texp_override (_, _) -> error_unsupported ()
  | Texp_letmodule (_, _, _, _, _) -> error_unsupported ()
  | Texp_letexception (_, _) -> error_unsupported ()
  | Texp_assert _ -> error_unimplemented ()
  | Texp_lazy _ -> error_unsupported ()
  | Texp_object (_, _) -> error_unsupported ()
  | Texp_pack _ -> error_unsupported ()
  | Texp_letop _ -> error_unimplemented ()
  | Texp_unreachable -> error_unsupported ()
  | Texp_extension_constructor (_, _) -> error_unsupported ()
  | Texp_open (_, _) -> error_unimplemented ()


and compile_expr_let value body =
  let { vb_pat; vb_expr; vb_attributes; vb_loc } = value in
  assert (List.is_empty vb_attributes);
  (* TODO: locs *)
  (* TODO: attributes *)
  (* TODO: recursive *)
  let pat = compile_pat vb_pat in
  (* TODO: will the type of this expression be mono? *)
  let value = compile_expr vb_expr in
  let body = compile_expr body in
  pat, value, body


and compile_case : type a. a case -> _ =
 fun case ->
  let { c_lhs; c_guard; c_rhs } = case in
  assert (Option.is_none c_guard);
  let pat = compile_pat c_lhs in
  let body = compile_expr c_rhs in
  pat, body


and compile_field (description, definition) =
  (* TODO: check / use all fields *)
  let { lbl_name
      ; lbl_res = _
      ; lbl_arg = _
      ; lbl_mut
      ; lbl_pos = _
      ; lbl_all
      ; lbl_repres = _
      ; lbl_private
      ; lbl_loc
      ; lbl_attributes
      ; lbl_uid = _
      }
    =
    description
  in
  let () =
    match lbl_mut with
    | Immutable -> ()
    | Mutable -> error_unimplemented ()
  in
  let () =
    match lbl_private with
    | Public -> ()
    | Private -> error_unimplemented ()
  in
  match definition with
  | Kept _typ ->
    (* TODO: priority *)
    error_unimplemented ()
  | Overridden (lid, value) ->
    (* TODO: check data  of lid? *)
    let value = compile_expr value in
    (* TODO: use proper location *)
    lbl_name, value


let compile_stri stri =
  (* TODO: location *)
  let { str_desc; str_loc; str_env = _ } = stri in
  match str_desc with
  | Tstr_eval _ -> error_unimplemented ()
  | Tstr_value (Nonrecursive, [ value ]) ->
    (* TODO: duplicated'ish *)
    let { vb_pat; vb_expr; vb_attributes; vb_loc } = value in
    assert (List.is_empty vb_attributes);
    (* TODO: locs *)
    (* TODO: attributes *)
    (* TODO: recursive *)
    let var_pat = compile_var_pat vb_pat in
    (* TODO: will the type of this expression be mono? *)
    let value = compile_expr vb_expr in
    decl_wrap @@ D_value (var_pat, value)
  (* TODO: should and be supported at all?? *)
  | Tstr_value (Nonrecursive, _) -> error_unimplemented ()
  | Tstr_value (Recursive, _) ->
    (* TODO: priority *)
    error_unimplemented ()
  | Tstr_primitive _ -> error_unimplemented ()
  | Tstr_type (Nonrecursive, [ decl ]) ->
    let { typ_id
        ; typ_name = _
        ; typ_params = _
        ; typ_type
        ; typ_cstrs = _
        ; typ_kind = _
        ; typ_private = _
        ; typ_manifest = _
        ; typ_loc
        ; typ_attributes
        }
      =
      decl
    in
    (* TODO: attributes *)
    assert (List.is_empty typ_attributes);
    decl_wrap @@ D_type (typ_id, compile_type_declaration typ_type)
  (* TODO: should and be supported at all?? *)
  | Tstr_type (Nonrecursive, _) -> error_unimplemented ()
  | Tstr_type (Recursive, _) ->
    (* TODO: priority *)
    error_unimplemented ()
  | Tstr_typext _ -> error_unsupported ()
  | Tstr_exception _ -> error_unsupported ()
  | Tstr_module _ ->
    (* TODO: priority *)
    error_unimplemented ()
  | Tstr_recmodule _ -> error_unsupported ()
  | Tstr_modtype _ -> error_unimplemented ()
  | Tstr_open _ ->
    (* TODO: priority *)
    error_unimplemented ()
  | Tstr_class _ -> error_unsupported ()
  | Tstr_class_type _ -> error_unsupported ()
  | Tstr_include _ -> error_unimplemented ()
  | Tstr_attribute _ -> error_unsupported ()


let compile_str str =
  let { str_items; str_type = _; str_final_env = _ } = str in
  List.map str_items ~f:(fun stri -> compile_stri stri)
