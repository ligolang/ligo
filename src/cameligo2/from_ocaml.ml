open Ocaml_common
open Asttypes
open Types
open Parsetree
open Typedtree
open Ligo_prim
open Ast_core
open Caml_core

(* TODO: check all assert and failwith *)
(* TODO: improve error messages *)
let error_unimplemented () = failwith "unimplemented"
let error_unsupported () = failwith "unsupported"
let error_unreachable () = failwith "unreachable"

(* TODO: ideally this would not be needed  *)
let split_arrow ~exp_env ~label type_ =
  (* TODO: is this guaranteed to not fail? *)
  Ctype.filter_arrow exp_env type_ label


let type_wrap loc desc = { type_desc = desc; type_loc = loc }

let type_decl_wrap loc params desc =
  { type_decl_desc = desc; type_decl_params = params; type_decl_loc = loc }


let pat_wrap loc type_ desc = { pat_desc = desc; pat_type = type_; pat_loc = loc }

let var_pat_wrap loc type_ desc =
  { var_pat_desc = desc; var_pat_type = type_; var_pat_loc = loc }


let expr_wrap loc type_ desc = { expr_desc = desc; expr_type = type_; expr_loc = loc }
let decl_wrap loc desc = { decl_desc = desc; decl_loc = loc }
let mod_expr_wrap loc desc = { mod_expr_desc = desc; mod_expr_loc = loc }

(* TODO: magic ligo stuff *)
let extract_payload_string payload =
  match payload with
  | PStr
      [ { pstr_desc =
            Pstr_eval
              ( { pexp_desc = Pexp_constant (Pconst_string (payload, _, _))
                ; pexp_loc = _
                ; pexp_loc_stack = _
                ; pexp_attributes = []
                }
              , y )
        ; pstr_loc = _
        }
      ] -> payload
  | _ -> error_unsupported ()


(* TODO: ppxlib? *)
let extract_ligo_predef payload pattern =
  let path, expected_arity =
    let Typedtree.
          { pat_desc; pat_loc = _; pat_extra; pat_type; pat_env = _; pat_attributes }
      =
      pattern
    in
    assert (
      match pat_desc with
      | Tpat_any -> true
      | _ -> false);
    assert (List.is_empty pat_attributes);
    match get_desc pat_type with
    | Tarrow (Nolabel, left, _, _) ->
      (match get_desc left with
      | Tconstr (path, args, _) -> path, List.length args
      | _ -> error_unsupported ())
    | _ -> error_unsupported ()
  in
  let ident =
    match path with
    | Pident ident -> ident
    | Pdot (_, _) | Papply (_, _) -> error_unsupported ()
  in
  let constant = extract_payload_string payload in
  let constant =
    match Literal_types.of_string_opt constant with
    | Some constant -> constant
    | None -> error_unsupported ()
  in
  let arity = Literal_types.to_arity constant in
  assert (arity = expected_arity);
  ident, constant, arity


let extract_ligo_constant payload type_typ =
  (* TODO: assert properties of ligo constant? *)
  let { type_params = _
      ; type_arity = expected_arity
      ; type_kind = _
      ; type_private = _
      ; type_manifest = _
      ; type_variance = _
      ; type_separability = _
      ; type_is_newtype = _
      ; type_expansion_scope = _
      ; type_loc = _
      ; type_attributes = _
      ; type_immediate = _
      ; type_unboxed_default = _
      ; type_uid = _
      }
    =
    type_typ
  in
  let constant = extract_payload_string payload in
  let constant =
    match Literal_types.of_string_opt constant with
    | Some constant -> constant
    | None -> error_unsupported ()
  in
  let arity = Literal_types.to_arity constant in
  assert (arity = expected_arity);
  constant, arity


(* TODO: normal ocaml stuff *)
let compile_loc loc : Location.t =
  let open Simple_utils in
  let Warnings.{ loc_start; loc_end; loc_ghost } = loc in
  (* TODO: Location seems to be too complex in ligo *)
  match loc_ghost with
  | true ->
    (* TODO: test ghost *)
    Location.File Region.ghost
  | false ->
    (* TODO: test locations *)
    Location.make loc_start loc_end


(* TODO: better locations, maybe use core_type *)
let rec compile_type loc type_ =
  (* TODO: allow attributes *)
  let open Ocaml_common.Types in
  (* TODO: detect and reject rectypes *)
  match get_desc type_ with
  (* polymorphism *)
  | Tvar name | Tunivar name ->
    (* TODO: when this is false *)
    assert (get_level type_ = Btype.generic_level);
    type_wrap loc @@ T_var (name, get_id type_)
  | Tpoly (body, vars) ->
    let vars =
      List.map vars ~f:(fun var ->
          match get_desc var with
          | Tunivar name -> name, get_id var
          | _ ->
            (* TODO: is this actually guaranteed? *)
            error_unreachable ())
    in
    let body = compile_type loc body in
    type_wrap loc @@ T_forall (vars, body)
  (* type constructors *)
  | Tconstr (path, args, _abbrev) ->
    let args = List.map ~f:(fun arg -> compile_type loc arg) args in
    type_wrap loc @@ T_constr (path, args)
  (* arrow *)
  | Tarrow (Nolabel, type1, type2, _comm) ->
    let type1 = compile_type loc type1 in
    let type2 = compile_type loc type2 in
    (* TODO: what about param_names? *)
    type_wrap loc @@ T_arrow (type1, type2)
  | Tarrow (Labelled _, _type1, _type2, _comm) -> error_unimplemented ()
  | Tarrow (Optional _, _type1, _type2, _comm) -> error_unimplemented ()
  (* tuple *)
  | Ttuple fields ->
    let fields = List.map fields ~f:(fun field -> compile_type loc field) in
    type_wrap loc @@ T_tuple fields
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


let compile_label_declaration label =
  let { ld_id; ld_mutable; ld_type; ld_loc; ld_attributes; ld_uid = _ } = label in
  assert (
    match ld_mutable with
    | Immutable -> true
    | Mutable -> false);
  assert (List.is_empty ld_attributes);
  let loc = compile_loc ld_loc in
  let type_ = compile_type loc ld_type in
  { dl_id = ld_id; dl_type = type_; dl_loc = loc }


let compile_type_declaration decl =
  let { type_params
      ; type_arity = _
      ; type_kind
      ; type_private
      ; type_manifest
      ; type_variance = _
      ; type_separability
      ; type_is_newtype (* TODO: use this? *)
      ; type_expansion_scope = _
      ; type_loc = loc
      ; type_attributes
      ; type_immediate = _
      ; type_unboxed_default
      ; type_uid = _
      }
    =
    decl
  in
  assert (
    match type_private with
    | Private -> false
    | Public -> true);
  (* TODO: do something with variance? *)
  (* assert (List.is_empty type_variance); *)
  (* TODO: do something with separability? *)
  (* assert (List.is_empty type_separability); *)
  (* TODO: support new type *)
  assert (not type_is_newtype);
  assert (List.is_empty type_attributes);
  (* TODO: what is this flag below? *)
  (* assert (not type_unboxed_default); *)
  let loc = compile_loc loc in
  let params =
    List.map type_params ~f:(fun var ->
        match get_desc var with
        | Tvar name -> name, get_id var
        | _ ->
          (* TODO: this may be false with constraints *)
          error_unreachable ())
  in
  match type_kind, type_manifest with
  | Type_abstract, Some manifest ->
    let manifest = compile_type loc manifest in
    type_decl_wrap loc params @@ T_alias manifest
  | Type_abstract, None ->
    (* TODO: useful for aliasing *)
    error_unimplemented ()
  | Type_record (fields, Record_regular), None ->
    let fields = List.map fields ~f:(fun label -> compile_label_declaration label) in
    type_decl_wrap loc params @@ T_record fields
  | Type_record (cases, Record_regular), Some _ -> error_unimplemented ()
  | ( Type_record
        (_, (Record_float | Record_unboxed _ | Record_inlined _ | Record_extension _))
    , (Some _ | None) ) -> error_unimplemented ()
  | Type_variant (cases, Variant_regular), None ->
    let cases =
      List.map cases ~f:(fun case ->
          let { cd_id; cd_args; cd_res; cd_loc; cd_attributes; cd_uid } = case in
          (* TODO: maybe support GADTs syntax but not GADTs? *)
          assert (Option.is_none cd_res);
          assert (List.is_empty cd_attributes);
          let loc = compile_loc cd_loc in
          match cd_args with
          | Cstr_tuple fields ->
            let fields = List.map fields ~f:(fun field -> compile_type loc field) in
            C_tuple { dc_id = cd_id; dc_fields = fields; dc_loc = loc }
          | Cstr_record fields ->
            let fields =
              List.map fields ~f:(fun label -> compile_label_declaration label)
            in
            C_record { dc_id = cd_id; dc_fields = fields; dc_loc = loc })
    in
    type_decl_wrap loc params @@ T_variant cases
  | Type_variant (_, Variant_regular), Some _ -> error_unimplemented ()
  | Type_variant (_, Variant_unboxed), (Some _ | None) ->
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
  | Const_nativeint _i -> error_unsupported ()


let compile_constructor constructor =
  let { cstr_name
      ; cstr_res = _
      ; cstr_existentials
      ; cstr_args = _
      ; cstr_arity = _
      ; cstr_tag = _
      ; cstr_consts = _
      ; cstr_nonconsts = _
      ; cstr_generalized
      ; cstr_private
      ; cstr_loc = _
      ; cstr_attributes
      ; cstr_inlined = _
      ; cstr_uid = _
      }
    =
    constructor
  in
  assert (List.is_empty cstr_existentials);
  assert (not cstr_generalized);
  assert (
    match cstr_private with
    | Public -> true
    | Private -> false);
  assert (List.is_empty cstr_attributes);
  (* TODO: cstr_inlined may bite us in the future *)
  cstr_name


let compile_pat_extra pat_extra =
  match pat_extra with
  | Tpat_constraint _typ ->
    (* TODO: is this relevant? *)
    ()
  | Tpat_type (_, _) ->
    (* TODO: maybe *)
    error_unsupported ()
  | Tpat_open (_, _, _) ->
    (* TODO: QoL *)
    error_unimplemented ()
  | Tpat_unpack -> error_unsupported ()


let compile_pat_alias : type a. a general_pattern -> unit =
 fun pat ->
  (* TODO: this is just a check *)
  let { pat_desc; pat_loc; pat_extra; pat_type; pat_env; pat_attributes } = pat in
  assert (List.is_empty pat_extra);
  assert (List.is_empty pat_attributes);
  (* TODO: this should definitely be removed *)
  match pat_desc with
  | Tpat_any -> ()
  | Tpat_var (_, _) -> error_unsupported ()
  | Tpat_alias (_, _, _) -> error_unsupported ()
  | Tpat_constant _ -> error_unsupported ()
  | Tpat_tuple _ -> error_unsupported ()
  | Tpat_construct (_, _, _, _) -> error_unsupported ()
  | Tpat_variant (_, _, _) -> error_unsupported ()
  | Tpat_record (_, _) -> error_unsupported ()
  | Tpat_array _ -> error_unsupported ()
  | Tpat_lazy _ -> error_unsupported ()
  | Tpat_value _ -> error_unsupported ()
  | Tpat_exception _ -> error_unsupported ()
  | Tpat_or (_, _, _) -> error_unsupported ()


let compile_var_pat : type a. a general_pattern -> var_pat =
 fun pat ->
  let { pat_desc; pat_loc; pat_extra; pat_type; pat_env; pat_attributes } = pat in
  let () =
    List.iter pat_extra ~f:(fun (pat_extra, _loc, pat_extra_attributes) ->
        assert (List.is_empty pat_extra_attributes);
        compile_pat_extra pat_extra)
  in
  assert (List.is_empty pat_attributes);
  let loc = compile_loc pat_loc in
  let type_ = compile_type loc pat_type in
  match pat_desc with
  | Tpat_any -> error_unsupported ()
  | Tpat_var (ident, _label) -> var_pat_wrap loc type_ @@ ident
  | Tpat_alias (pat, ident, _label) ->
    (* TODO: this one may look easy, but linearity *)
    let () = compile_pat_alias pat in
    var_pat_wrap loc type_ @@ ident
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
  assert (List.is_empty pat_attributes);
  let () =
    List.iter pat_extra ~f:(fun (pat_extra, _loc, pat_extra_attributes) ->
        assert (List.is_empty pat_extra_attributes);
        compile_pat_extra pat_extra)
  in
  let loc = compile_loc pat_loc in
  let type_ = compile_type loc pat_type in
  match pat_desc with
  | Tpat_any -> error_unimplemented ()
  | Tpat_var (ident, _label) -> pat_wrap loc type_ @@ P_var ident
  | Tpat_alias (pat, ident, _label) ->
    (* TODO: this one may look easy, but linearity *)
    let () = compile_pat_alias pat in
    pat_wrap loc type_ @@ P_var ident
  | Tpat_constant _ ->
    (* TODO: priority? *)
    error_unimplemented ()
  | Tpat_tuple fields ->
    let fields = List.map fields ~f:(fun field -> compile_pat field) in
    pat_wrap loc type_ @@ P_tuple fields
  | Tpat_construct ({ txt = _lident; loc = lident_loc }, constructor, payload, None) ->
    let label = compile_constructor constructor in
    let label = Label.Label (label, compile_loc lident_loc) in
    let payload = List.map payload ~f:(fun field -> compile_pat field) in
    let payload =
      match payload with
      | [] -> pat_wrap loc type_ @@ P_unit
      | [ payload ] -> payload
      | payload -> pat_wrap loc type_ @@ P_tuple payload
    in
    pat_wrap loc type_ @@ P_variant (label, payload)
  | Tpat_construct (_, _, _, Some _) ->
    (* TODO: weird cases, likely should be supported *)
    error_unimplemented ()
  | Tpat_variant (_, _, _) -> error_unsupported ()
  | Tpat_record (labels, Closed) ->
    (* TODO: priority *)
    let labels =
      List.map labels ~f:(fun ({ txt = _lident; loc = lident_loc }, label, pat) ->
          (* TODO: assert properties of this label *)
          let { lbl_name
              ; lbl_res = _
              ; lbl_arg = _
              ; lbl_mut = _
              ; lbl_pos = _
              ; lbl_all = _
              ; lbl_repres = _
              ; lbl_private = _
              ; lbl_loc = _
              ; lbl_attributes = _
              ; lbl_uid = _
              }
            =
            label
          in
          Label.Label (lbl_name, compile_loc lident_loc), compile_pat pat)
    in
    pat_wrap loc type_ @@ P_record labels
  | Tpat_record (_labels, Open) ->
    (* TODO: priority, but linearity *)
    error_unimplemented ()
  | Tpat_array _ -> error_unsupported ()
  | Tpat_lazy _ -> error_unsupported ()
  | Tpat_or (_, _, _) ->
    (* TODO: how hard would this one be? *)
    error_unsupported ()
  | Tpat_value pat ->
    (* TODO: is this right? Understand Tpat_value *)
    compile_pat (pat :> value general_pattern)
  | Tpat_exception _ -> error_unsupported ()


let rec compile_expr expr =
  let { exp_desc; exp_loc; exp_extra; exp_type; exp_env; exp_attributes } = expr in
  let () =
    List.iter exp_extra ~f:(fun (exp_extra, _loc, exp_extra_attributes) ->
        compile_expr_extra exp_extra)
  in
  assert (List.is_empty exp_attributes);
  let loc = compile_loc exp_loc in
  let type_ = compile_type loc exp_type in
  match exp_desc with
  | Texp_ident (path, _lident, value_desc) ->
    (* TODO: assert value is a valid value, aka not primitive *)
    expr_wrap loc type_ @@ E_var path
  | Texp_constant constant ->
    let literal = compile_literal constant in
    expr_wrap loc type_ @@ E_literal literal
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
    expr_wrap loc type_ @@ E_let_rec (var_pat, value, body)
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
    expr_wrap loc type_ @@ E_let (pat, value, body)
  | Texp_let (Recursive, _, _) -> error_unimplemented ()
  | Texp_let (Nonrecursive, _, _) -> error_unimplemented ()
  (* TODO: label, exp function *)
  | Texp_function { arg_label = Nolabel; param; cases; partial = Total } ->
    (* TODO: test both, multiple cases and  *)
    (* TODO: type function *)
    let param_type, body_type = split_arrow ~exp_env ~label:Nolabel exp_type in
    let param_type = compile_type loc param_type in
    let body_type = compile_type loc body_type in
    let body =
      let matchee = expr_wrap loc param_type @@ E_var (Pident param) in
      let cases = List.map cases ~f:(fun case -> compile_case case) in
      expr_wrap loc body_type @@ E_match (matchee, cases)
    in
    let param = var_pat_wrap loc param_type @@ param in
    expr_wrap loc type_ @@ E_lambda (param, body)
  | Texp_function { arg_label = Nolabel; partial = Partial; _ } -> error_unimplemented ()
  | Texp_function { arg_label = Labelled _ | Optional _; _ } -> error_unimplemented ()
  | Texp_apply (lambda, args) -> compile_expr_apply ~loc ~type_ lambda args
  | Texp_match (_, _, Partial) -> error_unimplemented ()
  | Texp_match (matchee, cases, Total) ->
    (* TODO: disc_label *)
    let matchee = compile_expr matchee in
    let cases = List.map cases ~f:(fun case -> compile_case case) in
    expr_wrap loc type_ @@ E_match (matchee, cases)
  | Texp_try (_, _) -> error_unsupported ()
  | Texp_tuple fields ->
    let fields = List.map fields ~f:(fun field -> compile_expr field) in
    let fields =
      match fields with
      | [] -> error_unreachable ()
      | field :: fields -> Ne_list.(field :: fields)
    in
    expr_wrap loc type_ @@ E_tuple fields
  | Texp_construct ({ txt = _lident; loc = lident_loc }, constructor, fields) ->
    (* TODO: use this location? *)
    let label = compile_constructor constructor in
    let label = Label.Label (label, compile_loc lident_loc) in
    let args = List.map fields ~f:(fun field -> compile_expr field) in
    expr_wrap loc type_ @@ E_constructor (label, args)
  | Texp_variant (_, _) -> error_unsupported ()
  | Texp_record { fields; representation; extended_expression } ->
    assert (Option.is_none extended_expression);
    let () =
      match representation with
      | Record_regular -> ()
      | Record_inlined _ ->
        (* TODO: is this one always okay? *)
        ()
      | Record_float | Record_unboxed _ | Record_extension _ -> error_unimplemented ()
    in
    let fields = Array.to_list fields in
    let fields = List.map fields ~f:(fun field -> compile_field field) in
    expr_wrap loc type_ @@ E_record fields
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
    expr_wrap loc type_ @@ E_field (record, lbl_name)
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


and compile_expr_extra expr_extra =
  match expr_extra with
  | Texp_constraint _ -> ()
  | Texp_coerce (_, _) -> error_unimplemented ()
  | Texp_poly _ ->
    (* TODO: what is this? *)
    error_unimplemented ()
  | Texp_newtype _ ->
    (* TODO: supporting this is a good idea? *)
    error_unimplemented ()


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


and compile_expr_apply ~loc ~type_ lambda args =
  (* TODO: duplicated *)
  let { exp_desc; exp_loc; exp_extra; exp_type; exp_env; exp_attributes } = lambda in
  let () =
    List.iter exp_extra ~f:(fun (exp_extra, _loc, exp_extra_attributes) ->
        compile_expr_extra exp_extra)
  in
  assert (List.is_empty exp_attributes);
  (* TODO: this is really hackish *)
  match exp_desc with
  | Texp_ident
      ( _path
      , _label
      , { val_type = _
        ; val_kind = Val_prim prim
        ; val_loc = _
        ; val_attributes
        ; val_uid = _
        } ) ->
    assert (List.is_empty val_attributes);
    compile_expr_apply_primitive ~loc ~type_ prim args
  | _ -> compile_expr_apply_fallback ~loc ~type_ lambda args


and compile_expr_apply_primitive ~loc ~type_ prim args =
  (* TODO: check properties *)
  let Primitive.
        { prim_name
        ; prim_arity = _
        ; prim_alloc = _
        ; prim_native_name = _
        ; prim_native_repr_args = _
        ; prim_native_repr_res = _
        }
    =
    prim
  in
  (* TODO: much better error messages *)
  match prim_name, args with
  | ("%ligo.nat" | "%ligo.tez" | "%ligo.address"), [ (Nolabel, Some arg) ] ->
    let constant =
      (* TODO: duplicated *)
      let { exp_desc; exp_loc; exp_extra; exp_type; exp_env; exp_attributes } = arg in
      let () =
        List.iter exp_extra ~f:(fun (exp_extra, _loc, exp_extra_attributes) ->
            compile_expr_extra exp_extra)
      in
      assert (List.is_empty exp_attributes);
      (* TODO: this is clearly disgusting  *)
      match exp_desc with
      | Texp_constant constant -> constant
      | _ -> error_unsupported ()
    in
    compile_expr_ligo_literals ~loc ~type_ prim_name constant
  | _ -> error_unsupported ()


and compile_expr_ligo_literals ~loc ~type_ prim constant =
  (* TODO: this is duplicated code from checking *)
  (* TODO: this can be deleted whenever we start targetting Ast_typed *)
  match prim, constant with
  | "%ligo.nat", Const_int n ->
    let lit = Z.of_int n in
    expr_wrap loc type_ @@ E_literal (Literal_nat lit)
  | "%ligo.tez", Const_int n ->
    let lit = Z.of_int n in
    let lit = Z.mul (Z.of_int 1_000_000) lit in
    expr_wrap loc type_ @@ E_literal (Literal_mutez lit)
  | "%ligo.address", Const_string (lit, _loc, None) ->
    expr_wrap loc type_ @@ E_literal (Literal_address lit)
  | _ -> error_unsupported ()


and compile_expr_apply_fallback ~loc ~type_ lambda args =
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
  expr_wrap loc type_ @@ E_apply (lambda, args)


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


let rec compile_str str =
  let { str_items; str_type = _; str_final_env = _ } = str in
  List.filter_map str_items ~f:(fun stri -> compile_stri stri)


and compile_stri stri =
  (* TODO: location *)
  let { str_desc; str_loc; str_env = _ } = stri in
  match str_desc with
  | Tstr_eval _ -> error_unimplemented ()
  | Tstr_value (Nonrecursive, [ value ]) -> Some (compile_value_binding value)
  (* TODO: should `and` be supported at all?? *)
  | Tstr_value (Nonrecursive, _) -> error_unimplemented ()
  | Tstr_value (Recursive, _) ->
    (* TODO: priority *)
    error_unimplemented ()
  | Tstr_primitive value ->
    compile_primitive value;
    None
  | Tstr_type (Nonrecursive, [ decl ]) -> Some (compile_type_decl decl)
  (* TODO: should and be supported at all?? *)
  | Tstr_type (Nonrecursive, _) -> error_unimplemented ()
  | Tstr_type (Recursive, _) ->
    (* TODO: priority *)
    error_unimplemented ()
  | Tstr_typext _ -> error_unsupported ()
  | Tstr_exception _ -> error_unsupported ()
  | Tstr_module mb -> Some (compile_module_binding mb)
  | Tstr_recmodule _ -> error_unsupported ()
  | Tstr_modtype _ -> error_unimplemented ()
  | Tstr_open _ ->
    (* TODO: priority *)
    error_unimplemented ()
  | Tstr_class _ -> error_unsupported ()
  | Tstr_class_type _ -> error_unsupported ()
  | Tstr_include _ -> error_unimplemented ()
  | Tstr_attribute _ -> error_unsupported ()


and compile_value_binding vb =
  (* TODO: duplicated'ish *)
  let { vb_pat; vb_expr; vb_attributes; vb_loc } = vb in
  let loc = compile_loc vb_loc in
  match vb_attributes with
  | [] ->
    (* TODO: recursive *)
    let var_pat = compile_var_pat vb_pat in
    (* TODO: will the type of this expression be mono? *)
    let value = compile_expr vb_expr in
    decl_wrap loc @@ D_value (var_pat, value)
  | [ { attr_name = { txt = "ligo.internal.predef"; loc = _ }
      ; attr_payload
      ; attr_loc = _
      }
    ] ->
    let ident, constant, arity = extract_ligo_predef attr_payload vb_pat in
    (* TODO: not having params here is weird *)
    let type_decl = type_decl_wrap loc [] @@ T_constant (constant, arity) in
    decl_wrap loc @@ D_type (ident, type_decl)
  | _ -> error_unsupported ()


and compile_primitive vd =
  let { val_id = _
      ; val_name = _
      ; val_desc = _
      ; val_val = _
      ; val_prim
      ; val_loc = _
      ; val_attributes
      }
    =
    vd
  in
  (* TODO: check val_desc type? *)
  assert (List.is_empty val_attributes);
  match val_prim with
  (* TODO: those names are duplicated *)
  | [ ("%ligo.nat" | "%ligo.tez" | "%ligo.address") ] -> ()
  | _ -> error_unsupported ()


and compile_type_decl decl =
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
  let loc = compile_loc typ_loc in
  match typ_attributes with
  | [] -> decl_wrap loc @@ D_type (typ_id, compile_type_declaration typ_type)
  | [ { attr_name = { txt = "ligo.internal.constant"; loc = _ }; attr_payload; attr_loc }
    ] ->
    let constant, arity = extract_ligo_constant attr_payload typ_type in
    (* TODO: not having params here is weird *)
    let type_decl =
      type_decl_wrap loc []
      (* TODO: not having params here is weird *) @@ T_constant (constant, arity)
    in
    decl_wrap loc @@ D_type (typ_id, type_decl)
  | _ -> error_unsupported ()


and compile_module_binding mb =
  let { mb_id; mb_name = _; mb_presence; mb_expr; mb_attributes; mb_loc } = mb in
  assert (
    match mb_presence with
    | Mp_present -> true
    | Mp_absent -> false);
  assert (List.is_empty mb_attributes);
  let loc = compile_loc mb_loc in
  let ident =
    match mb_id with
    | Some ident -> ident
    | None -> error_unsupported ()
  in
  let body = compile_module_expr mb_expr in
  decl_wrap loc @@ D_module (ident, body)


and compile_module_expr mod_expr =
  let { mod_desc; mod_loc; mod_type = _; mod_env = _; mod_attributes } = mod_expr in
  (* TODO: use module_type? *)
  assert (List.is_empty mod_attributes);
  let loc = compile_loc mod_loc in
  match mod_desc with
  | Tmod_ident (path, _lident) -> mod_expr_wrap loc @@ M_var path
  | Tmod_structure str -> mod_expr_wrap loc @@ M_struct (compile_str str)
  | Tmod_functor (_, _) -> error_unsupported ()
  | Tmod_apply (_, _, _) -> error_unsupported ()
  | Tmod_constraint (_, _, _, _) -> error_unimplemented ()
  | Tmod_unpack (_, _) -> error_unsupported ()
