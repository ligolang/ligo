open Ocaml_common
open Asttypes
open Types
open Parsetree
open Typedtree
open Ligo_prim
open Ast_core
open Caml_core
open Caml_error

(* TODO: put this somewhere else *)
let ( let@@ ) f x = f x

(* TODO: should OCaml int map to Ligo int? What about overflow? *)
(* TODO: error recovery *)
(* TODO: non existential GADT's and FCM could be supported *)
(* TODO: a lot of existential could also be supported *)
(* TODO: use more core type *)
(* TODO: a general limitation of this,
  is that it only accepts code in more or less strict format
  simple things such as adding a let may break extraction  *)
(* TODO: this pass mostly shrinksn the OCaml tree *)

(* TODO: check all assert and failwith *)
(* TODO: improve error messages *)

(* TODO: ideally this would not be needed  *)
let split_arrow ~exp_env ~label type_ =
  (* TODO: is this guaranteed to not fail? *)
  Ctype.filter_arrow exp_env type_ label


(* TODO: normal ocaml stuff *)
(* TODO: maybe integrate loc on enter_region *)
let extract_loc ~loc : Location.t =
  let open Simple_utils in
  let Warnings.{ loc_start; loc_end; loc_ghost } = loc in
  (* TODO: Location seems to be too complex in ligo *)
  match loc_ghost with
  | true ->
    (* TODO: test ghost *)
    Location.File Region.ghost
  | false ->
    (* TODO: when problems cnum < bol *)
    (* TODO: test locations *)
    Location.make loc_start loc_end


(* TODO: this is a bad name *)
let extract_field_name lid =
  let { txt = lid; loc } = lid in
  let loc = extract_loc ~loc in
  Label.Label (Longident.last lid, loc)


(* TODO: magic ligo stuff *)
(* TODO: use this function?  *)
let _extract_payload_string payload =
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
  | _ -> raise_pre_error @@ E_unsupported


let extract_attrs ~init ~f attrs =
  List.fold_left attrs ~init ~f:(fun acc attr ->
      let { attr_name; attr_payload; attr_loc } = attr in
      let loc = extract_loc ~loc:attr_loc in
      let@@ () = try_enhance ~loc in
      assert (
        match attr_payload with
        | PStr [] -> true
        | PStr _ -> false
        | PSig _ -> false
        | PTyp _ -> false
        | PPat (_, _) -> false);
      let { txt = key; loc } = attr_name in
      (* TODO: support comment attributes and proper loc for comments *)
      match f ~key acc with
      | `Ok acc -> acc
      | `Invalid_attribute -> raise_pre_error @@ E_unsupported)


let extract_decl_attrs attrs =
  extract_attrs
    ~init:Value_attr.default_attributes
    ~f:(fun ~key acc -> Value_attr.apply_decl_attr ~key ~value:None acc)
    attrs


let extract_expr_attrs attrs =
  extract_attrs
    ~init:Value_attr.default_attributes
    ~f:(fun ~key acc -> Value_attr.apply_expr_attr ~key ~value:None acc)
    attrs


let extract_module_attrs attrs =
  extract_attrs
    ~init:Type_or_module_attr.default_attributes
    ~f:(fun ~key acc -> Type_or_module_attr.apply_mod_or_sig ~key ~value:None acc)
    attrs


let extract_signature_attrs attrs =
  extract_attrs
    ~init:Signature_attr.default_attributes
    ~f:(fun ~key acc -> Signature_attr.apply_sig_attr ~key ~value:None acc)
    attrs


let extract_sig_item_attrs attrs =
  extract_attrs
    ~init:Sig_item_attr.default_attributes
    ~f:(fun ~key acc -> Sig_item_attr.apply_sig_item_attr ~key ~value:None acc)
    attrs


(* TODO: better locations, probably using core_type *)
(* TODO: equirecursive types *)
let rec extract_type ~loc type_ =
  (* TODO: allow attributes *)
  let open Ocaml_common.Types in
  (* TODO: detect and reject rectypes *)
  let on_error exn = type_wrap loc @@ T_error exn in
  let@@ () = try_recover ~loc ~on_error in
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
            raise_pre_error @@ E_unreachable)
    in
    let body = extract_type ~loc body in
    type_wrap loc @@ T_forall (vars, body)
  (* type constructors *)
  | Tconstr (path, args, _abbrev) ->
    let args = List.map ~f:(fun arg -> extract_type ~loc arg) args in
    type_wrap loc @@ T_constr (path, args)
  (* arrow *)
  | Tarrow (Nolabel, type1, type2, _comm) ->
    let type1 = extract_type ~loc type1 in
    let type2 = extract_type ~loc type2 in
    (* TODO: what about param_names? *)
    type_wrap loc @@ T_arrow (type1, type2)
  | Tarrow (Labelled _, _type1, _type2, _comm) ->
    raise_pre_error @@ E_labelled_parameters_not_supported
  | Tarrow (Optional _, _type1, _type2, _comm) ->
    raise_pre_error @@ E_optional_parameters_not_supported
  (* tuple *)
  | Ttuple fields ->
    let fields = List.map fields ~f:(fun field -> extract_type ~loc field) in
    type_wrap loc @@ T_tuple fields
  (* variants *)
  | Tvariant _ -> raise_pre_error @@ E_poly_vars_not_supported
  (* first-class modules *)
  | Tpackage _ -> raise_pre_error @@ E_fcm_not_supported
  (* objects *)
  | Tobject _ -> raise_pre_error @@ E_objects_not_supported
  | Tfield _ -> raise_pre_error @@ E_objects_not_supported
  | Tnil -> raise_pre_error @@ E_objects_not_supported
  (* machinery *)
  | Tlink _ -> raise_pre_error @@ E_unexpected_typed_tree
  | Tsubst (_, _) -> raise_pre_error @@ E_unexpected_typed_tree


let extract_label_declaration label =
  let { ld_id; ld_mutable; ld_type; ld_loc = loc; ld_attributes; ld_uid = _ } = label in
  let loc = extract_loc ~loc in
  let@@ () = try_enhance ~loc in
  assert (
    match ld_mutable with
    | Immutable -> true
    | Mutable -> false);
  assert (List.is_empty ld_attributes);
  let type_ = extract_type ~loc ld_type in
  (* TODO: type_decl_label_wrap? *)
  { dl_id = ld_id; dl_type = type_; dl_loc = loc }


let extract_type_declaration decl =
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
  let loc = extract_loc ~loc in
  let on_error exn =
    (* TODO: params *)
    let params = [] in
    type_decl_wrap loc params @@ T_error exn
  in
  let@@ () = try_recover ~loc ~on_error in
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
  let params =
    List.map type_params ~f:(fun var ->
        match get_desc var with
        | Tvar name -> name, get_id var
        | _ ->
          (* TODO: this may be false with constraints *)
          raise_pre_error @@ E_unreachable)
  in
  match type_kind, type_manifest with
  | Type_abstract, Some manifest ->
    let manifest = extract_type ~loc manifest in
    type_decl_wrap loc params @@ T_alias manifest
  | Type_abstract, None -> raise_pre_error @@ E_abstract_types_not_supported
  | Type_record (fields, Record_regular), (None | Some _) ->
    (* TODO: does this manifest matters? *)
    let fields = List.map fields ~f:extract_label_declaration in
    type_decl_wrap loc params @@ T_record fields
  | ( Type_record
        (_, (Record_float | Record_unboxed _ | Record_inlined _ | Record_extension _))
    , (Some _ | None) ) -> raise_pre_error @@ E_unimplemented
  | Type_variant (cases, Variant_regular), (None | Some _) ->
    (* TODO: does this manifest matters? *)
    let cases =
      List.map cases ~f:(fun case ->
          let { cd_id; cd_args; cd_res; cd_loc; cd_attributes; cd_uid } = case in
          let loc = extract_loc ~loc:cd_loc in
          let@@ () = try_enhance ~loc in
          (* TODO: maybe support GADTs syntax but not GADTs? *)
          assert (Option.is_none cd_res);
          assert (List.is_empty cd_attributes);
          match cd_args with
          | Cstr_tuple fields ->
            let fields = List.map fields ~f:(fun field -> extract_type ~loc field) in
            C_tuple { dc_id = cd_id; dc_fields = fields; dc_loc = loc }
          | Cstr_record fields ->
            let fields = List.map fields ~f:extract_label_declaration in
            C_record { dc_id = cd_id; dc_fields = fields; dc_loc = loc })
    in
    type_decl_wrap loc params @@ T_variant cases
  | Type_variant (_, Variant_unboxed), (Some _ | None) ->
    (* TODO: high priority *)
    raise_pre_error @@ E_unimplemented
  | Type_open, (Some _ | None) -> raise_pre_error @@ E_unimplemented


let extract_literal constant =
  let open Literal_value in
  match constant with
  | Const_int n ->
    let n = Z.of_int n in
    Literal_int n
  | Const_char _ -> raise_pre_error @@ E_unsupported
  | Const_string (content, _loc, None) ->
    (* TODO: use string loc? *)
    (* TODO: standard vs verbatin *)
    Literal_string (Standard content)
  | Const_string (_content, _loc, Some _tag) -> raise_pre_error @@ E_unsupported
  | Const_float _ -> raise_pre_error @@ E_unsupported
  | Const_int32 _ -> raise_pre_error @@ E_unsupported
  | Const_int64 _ -> raise_pre_error @@ E_unsupported
  | Const_nativeint _i -> raise_pre_error @@ E_unsupported


let extract_pat_extra pat_extra =
  let pat_extra, _loc, pat_extra_attributes = pat_extra in
  (* TODO: put this in Caml_core and add loc *)
  assert (List.is_empty pat_extra_attributes);
  match pat_extra with
  | Tpat_constraint _typ ->
    (* TODO: is this relevant? *)
    ()
  | Tpat_type (_, _) ->
    (* TODO: maybe *)
    raise_pre_error @@ E_unsupported
  | Tpat_open (_, _, _) ->
    (* TODO: QoL *)
    raise_pre_error @@ E_unimplemented
  | Tpat_unpack -> raise_pre_error @@ E_fcm_not_supported


let extract_pat_alias : type a. a general_pattern -> unit =
 fun pat ->
  (* TODO: this is just a check *)
  let { pat_desc; pat_loc; pat_extra; pat_type; pat_env; pat_attributes } = pat in
  (* TOOD: use this pat_loc? *)
  assert (List.is_empty pat_extra);
  assert (List.is_empty pat_attributes);
  (* TODO: this should definitely be removed *)
  match pat_desc with
  | Tpat_any -> ()
  | Tpat_var (_, _) -> raise_pre_error @@ E_unsupported
  | Tpat_alias (_, _, _) -> raise_pre_error @@ E_unsupported
  | Tpat_constant _ -> raise_pre_error @@ E_unsupported
  | Tpat_tuple _ -> raise_pre_error @@ E_unsupported
  | Tpat_construct (_, _, _, _) -> raise_pre_error @@ E_unsupported
  | Tpat_variant (_, _, _) -> raise_pre_error @@ E_unsupported
  | Tpat_record (_, _) -> raise_pre_error @@ E_unsupported
  | Tpat_array _ -> raise_pre_error @@ E_unsupported
  | Tpat_lazy _ -> raise_pre_error @@ E_lazy_not_supported
  | Tpat_value _ -> raise_pre_error @@ E_unsupported
  | Tpat_exception _ -> raise_pre_error @@ E_unsupported
  | Tpat_or (_, _, _) -> raise_pre_error @@ E_unsupported


let rec extract_pat : type a. a general_pattern -> pat =
 fun pat ->
  let { pat_desc; pat_loc; pat_extra; pat_type; pat_env; pat_attributes } = pat in
  let loc = extract_loc ~loc:pat_loc in
  let@@ () = try_enhance ~loc in
  let type_ = extract_type ~loc pat_type in
  let on_error exn = pat_wrap loc type_ @@ P_error exn in
  let@@ () = try_recover ~loc ~on_error in
  assert (List.is_empty pat_attributes);
  let () = List.iter pat_extra ~f:extract_pat_extra in
  match pat_desc with
  | Tpat_any -> raise_pre_error @@ E_unimplemented
  | Tpat_var (ident, _label) -> pat_wrap loc type_ @@ P_var ident
  | Tpat_alias (pat, ident, _label) ->
    (* TODO: this one may look easy, but linearity *)
    let () = extract_pat_alias pat in
    pat_wrap loc type_ @@ P_var ident
  | Tpat_constant _ ->
    (* TODO: priority? *)
    raise_pre_error @@ E_unimplemented
  | Tpat_tuple fields ->
    let fields = List.map fields ~f:extract_pat in
    pat_wrap loc type_ @@ P_tuple fields
  | Tpat_construct (lident, constructor, payload, None) ->
    let label = extract_field_name lident in
    let payload = List.map payload ~f:extract_pat in
    let payload =
      match payload with
      | [] -> pat_wrap loc type_ @@ P_unit
      | [ payload ] -> payload
      | payload -> pat_wrap loc type_ @@ P_tuple payload
    in
    pat_wrap loc type_ @@ P_variant (label, payload)
  | Tpat_construct (_, _, _, Some _) ->
    (* TODO: weird cases, likely should be supported *)
    raise_pre_error @@ E_unimplemented
  | Tpat_variant (_, _, _) -> raise_pre_error @@ E_poly_vars_not_supported
  | Tpat_record (labels, Closed) ->
    (* TODO: priority *)
    let labels =
      List.map labels ~f:(fun (lident, _label, pat) ->
          extract_field_name lident, extract_pat pat)
    in
    pat_wrap loc type_ @@ P_record labels
  | Tpat_record (_labels, Open) ->
    (* TODO: priority, but linearity *)
    raise_pre_error @@ E_unimplemented
  | Tpat_array _ -> raise_pre_error @@ E_array_not_supported
  | Tpat_lazy _ -> raise_pre_error @@ E_lazy_not_supported
  | Tpat_or (_, _, _) ->
    (* TODO: how hard would this one be? *)
    raise_pre_error @@ E_unsupported
  | Tpat_value pat ->
    (* TODO: is this right? Understand Tpat_value *)
    extract_pat (pat :> value general_pattern)
  | Tpat_exception _ -> raise_pre_error @@ E_exceptions_not_supported


let signature_of_sig_expr sig_expr =
  let { sig_expr_desc; sig_expr_loc } = sig_expr in
  match sig_expr_desc with
  | S_var _ -> raise_pre_error @@ E_unsupported
  | S_sig signature -> signature


let rec extract_expr expr =
  let { exp_desc; exp_loc; exp_extra; exp_type; exp_env; exp_attributes } = expr in
  let loc = extract_loc ~loc:exp_loc in
  let@@ () = try_enhance ~loc in
  let type_ = extract_type ~loc exp_type in
  let on_error error = expr_wrap loc type_ @@ E_error error in
  let@@ () = try_recover ~loc ~on_error in
  let () = List.iter exp_extra ~f:extract_expr_extra in
  match exp_desc with
  | Texp_ident (path, _lident, value_desc) ->
    (* TODO: high priority *)
    (* TODO: assert value is a valid value, aka not primitive *)
    expr_wrap loc type_ @@ E_var path
  | Texp_constant constant ->
    let literal = extract_literal constant in
    expr_wrap loc type_ @@ E_literal literal
  | Texp_let (rec_flag, bindings, body) ->
    let binding =
      match bindings with
      | [] -> raise_pre_error @@ E_unexpected_typed_tree
      | [ value ] -> value
      | _first :: _second :: _rest ->
        (* TODO: support this? *)
        raise_pre_error @@ E_let_and_not_supported
    in
    let _loc, pat, attr, value = extract_expr_binding rec_flag binding in
    let attr = extract_expr_attrs attr in
    let body = extract_expr body in
    expr_wrap loc type_ @@ E_let (pat, attr, value, body)
  (* TODO: label, exp function *)
  | Texp_function { arg_label = Nolabel; param; cases; partial = Total } ->
    let param, body = extract_expr_function ~exp_env ~exp_type ~loc param cases in
    expr_wrap loc type_ @@ E_lambda (param, body)
  | Texp_function { arg_label = Nolabel; partial = Partial; _ } ->
    raise_pre_error @@ E_unimplemented
  | Texp_function { arg_label = Labelled _; _ } ->
    raise_pre_error @@ E_labelled_parameters_not_supported
  | Texp_function { arg_label = Optional _; _ } ->
    raise_pre_error @@ E_optional_parameters_not_supported
  | Texp_apply (lambda, args) -> extract_expr_apply ~loc ~type_ lambda args
  | Texp_match (matchee, cases, Total) ->
    (* TODO: disc_label *)
    let matchee = extract_expr matchee in
    let cases = List.map cases ~f:(fun case -> extract_case case) in
    expr_wrap loc type_ @@ E_match (matchee, cases)
  | Texp_match (_, _, Partial) -> raise_pre_error @@ E_partial_match_not_supported
  | Texp_try (_, _) -> raise_pre_error @@ E_exceptions_not_supported
  | Texp_tuple fields ->
    let fields = List.map fields ~f:(fun field -> extract_expr field) in
    let fields =
      match fields with
      | [] -> raise_pre_error @@ E_unreachable
      | field :: fields -> Ne_list.(field :: fields)
    in
    expr_wrap loc type_ @@ E_tuple fields
  | Texp_construct (lident, _constructor, fields) ->
    let label = extract_field_name lident in
    let args = List.map fields ~f:(fun field -> extract_expr field) in
    expr_wrap loc type_ @@ E_constructor (label, args)
  | Texp_variant (_, _) -> raise_pre_error @@ E_poly_vars_not_supported
  | Texp_record { fields; representation; extended_expression } ->
    assert (Option.is_none extended_expression);
    let () =
      match representation with
      | Record_regular -> ()
      | Record_inlined _ ->
        (* TODO: is this one always okay? *)
        ()
      | Record_float | Record_unboxed _ | Record_extension _ ->
        raise_pre_error @@ E_unimplemented
    in
    let fields = Array.to_list fields in
    let fields =
      List.map fields ~f:(fun (_label, definition) ->
          match definition with
          | Kept _typ ->
            (* TODO: priority *)
            raise_pre_error @@ E_unimplemented
          | Overridden (lid, value) ->
            (* TODO: check data  of lid? *)
            let value = extract_expr value in
            (* TODO: use proper location *)
            extract_field_name lid, value)
    in
    expr_wrap loc type_ @@ E_record fields
  | Texp_field (record, field, _label) ->
    let field = extract_field_name field in
    let record = extract_expr record in
    expr_wrap loc type_ @@ E_field (record, field)
  | Texp_setfield (_, _, _, _) -> raise_pre_error @@ E_mutation_not_supported
  | Texp_array _ -> raise_pre_error @@ E_array_not_supported
  | Texp_ifthenelse (_, _, _) ->
    (* TODO: priority *)
    raise_pre_error @@ E_unimplemented
  | Texp_sequence (_, _) ->
    (* TODO: support this? *)
    raise_pre_error @@ E_unimplemented
  | Texp_while (_, _) -> raise_pre_error @@ E_while_not_supported
  | Texp_for (_, _, _, _, _, _) -> raise_pre_error @@ E_for_not_supported
  | Texp_send (_, _) -> raise_pre_error @@ E_objects_not_supported
  | Texp_new (_, _, _) -> raise_pre_error @@ E_objects_not_supported
  | Texp_instvar (_, _, _) -> raise_pre_error @@ E_objects_not_supported
  | Texp_setinstvar (_, _, _, _) -> raise_pre_error @@ E_objects_not_supported
  | Texp_override (_, _) -> raise_pre_error @@ E_objects_not_supported
  | Texp_letmodule (mb_id, _mod_name, mb_presence, mb_expr, body) ->
    let ident, md_body, body =
      extract_expr_module ~loc ~mb_id ~mb_presence ~mb_expr body
    in
    expr_wrap loc type_ @@ E_let_module (ident, md_body, body)
  | Texp_letexception (_, _) -> raise_pre_error @@ E_exceptions_not_supported
  | Texp_assert _ -> raise_pre_error @@ E_unimplemented
  | Texp_lazy _ -> raise_pre_error @@ E_lazy_not_supported
  | Texp_object (_, _) -> raise_pre_error @@ E_objects_not_supported
  | Texp_pack _ -> raise_pre_error @@ E_fcm_not_supported
  | Texp_letop _ ->
    (* TODO: support this? Why? Option binding? *)
    raise_pre_error @@ E_unimplemented
  | Texp_unreachable -> raise_pre_error @@ E_refutation_not_supported
  | Texp_extension_constructor (_, _) ->
    (* TODO: this is very niche, probably not a good idea *)
    raise_pre_error @@ E_unsupported
  | Texp_open (_, _) ->
    (* TODO: priority *)
    raise_pre_error @@ E_unimplemented


and extract_expr_extra expr_extra =
  let expr_extra, _loc, _expr_extra_attributes = expr_extra in
  (* TODO: use this loc *)
  (* TODO: state about attributes on expressions *)
  match expr_extra with
  | Texp_constraint _ -> ()
  | Texp_coerce (_, _) -> raise_pre_error @@ E_unimplemented
  | Texp_poly _ ->
    (* TODO: what is this? *)
    raise_pre_error @@ E_unimplemented
  | Texp_newtype _ ->
    (* TODO: supporting this is a good idea? *)
    raise_pre_error @@ E_unimplemented


(* let [@attr] {rec,nonrec} x = M *)
and extract_expr_binding rec_flag binding =
  let { vb_pat; vb_expr; vb_attributes; vb_loc = loc } = binding in
  (* TODO: export this loc? *)
  let loc = extract_loc ~loc in
  let@@ () = try_enhance ~loc in
  let pat = extract_pat vb_pat in
  (* TODO: will the type of this expression be mono? *)
  (* TODO: poly value *)
  let value =
    match rec_flag with
    | Nonrecursive -> extract_expr vb_expr
    | Recursive -> extract_expr_recursive ~self:pat vb_expr
  in
  loc, pat, vb_attributes, value


and extract_expr_recursive ~self expr =
  let { exp_desc; exp_loc; exp_extra; exp_type; exp_env; exp_attributes } = expr in
  let loc = extract_loc ~loc:exp_loc in
  let@@ () = try_enhance ~loc in
  (* TODO: maybe extract recursive after extract_expr? *)
  let () = List.iter exp_extra ~f:extract_expr_extra in
  assert (List.is_empty exp_attributes);
  let type_ = extract_type ~loc exp_type in
  match exp_desc with
  (* TODO: label, exp function *)
  | Texp_function { arg_label = Nolabel; param; cases; partial = Total } ->
    let param, body = extract_expr_function ~exp_env ~exp_type ~loc param cases in
    expr_wrap loc type_ @@ E_lambda_rec { self; param; body }
  | Texp_function { arg_label = Nolabel; partial = Partial; _ } ->
    raise_pre_error @@ E_unimplemented
  | Texp_function { arg_label = Labelled _; _ } ->
    raise_pre_error @@ E_labelled_parameters_not_supported
  | Texp_function { arg_label = Optional _; _ } ->
    raise_pre_error @@ E_optional_parameters_not_supported
  | Texp_ident (_, _, _)
  | Texp_constant _
  | Texp_let (_, _, _)
  | Texp_apply (_, _)
  | Texp_match (_, _, _)
  | Texp_try (_, _)
  | Texp_tuple _
  | Texp_construct (_, _, _)
  | Texp_variant (_, _)
  | Texp_record _
  | Texp_field (_, _, _)
  | Texp_setfield (_, _, _, _)
  | Texp_array _
  | Texp_ifthenelse (_, _, _)
  | Texp_sequence (_, _)
  | Texp_while (_, _)
  | Texp_for (_, _, _, _, _, _)
  | Texp_send (_, _)
  | Texp_new (_, _, _)
  | Texp_instvar (_, _, _)
  | Texp_setinstvar (_, _, _, _)
  | Texp_override (_, _)
  | Texp_letmodule (_, _, _, _, _)
  | Texp_letexception (_, _)
  | Texp_assert _ | Texp_lazy _
  | Texp_object (_, _)
  | Texp_pack _ | Texp_letop _ | Texp_unreachable
  | Texp_extension_constructor (_, _)
  | Texp_open (_, _) -> raise_pre_error @@ E_recursive_bindings_must_be_a_function


and extract_expr_function ~exp_env ~exp_type ~loc param cases =
  (* TODO: test both, multiple cases and single cases *)
  let param_type, body_type = split_arrow ~exp_env ~label:Nolabel exp_type in
  let param_type = extract_type ~loc param_type in
  let body_type = extract_type ~loc body_type in
  let body =
    let matchee = expr_wrap loc param_type @@ E_var (Pident param) in
    let cases = List.map cases ~f:extract_case in
    expr_wrap loc body_type @@ E_match (matchee, cases)
  in
  let param = pat_wrap loc param_type @@ P_var param in
  param, body


and extract_expr_apply ~loc ~type_ lambda args =
  (* TODO: duplicated *)
  let { exp_desc; exp_loc; exp_extra; exp_type; exp_env; exp_attributes } = lambda in
  let () =
    let loc = extract_loc ~loc:exp_loc in
    let@@ () = try_enhance ~loc in
    let () = List.iter exp_extra ~f:extract_expr_extra in
    assert (List.is_empty exp_attributes)
  in
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
    extract_expr_apply_primitive ~loc ~type_ prim args
  | _ -> extract_expr_apply_fallback ~loc ~type_ lambda args


and extract_expr_apply_primitive ~loc ~type_ prim args =
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
      let () = List.iter exp_extra ~f:extract_expr_extra in
      assert (List.is_empty exp_attributes);
      (* TODO: this is clearly disgusting  *)
      match exp_desc with
      | Texp_constant constant -> constant
      | _ -> raise_pre_error @@ E_unsupported
    in
    extract_expr_ligo_literals ~loc ~type_ prim_name constant
  | _ -> raise_pre_error @@ E_unsupported


and extract_expr_ligo_literals ~loc ~type_ prim constant =
  (* TODO: this is duplicated code from checking *)
  (* TODO: this can be deleted whenever we start targetting Ast_typed *)
  (* TODO: attributes here *)
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
  | _ -> raise_pre_error @@ E_unsupported


and extract_expr_apply_fallback ~loc ~type_ lambda args =
  let lambda = extract_expr lambda in
  let args =
    List.map
      ~f:(fun (label, arg) ->
        match label, arg with
        | Nolabel, Some arg -> extract_expr arg
        | Nolabel, None -> raise_pre_error @@ E_unimplemented
        | Labelled _, _ -> raise_pre_error @@ E_labelled_parameters_not_supported
        | Optional _, _ -> raise_pre_error @@ E_labelled_parameters_not_supported)
      args
  in
  expr_wrap loc type_ @@ E_apply (lambda, args)


and extract_case : type a. a case -> _ =
 fun case ->
  let { c_lhs; c_guard; c_rhs } = case in
  assert (Option.is_none c_guard);
  let pat = extract_pat c_lhs in
  let body = extract_expr c_rhs in
  pat, body


and extract_expr_module ~loc ~mb_id ~mb_presence ~mb_expr body =
  (* TODO: this is mostly duplicated *)
  assert (
    match mb_presence with
    | Mp_present -> true
    | Mp_absent -> false);
  let ident =
    match mb_id with
    | Some ident -> ident
    | None -> raise_pre_error @@ E_modules_without_names_not_supported
  in
  let md_body = extract_module_expr mb_expr in
  let body = extract_expr body in
  ident, md_body, body


and extract_str str =
  let { str_items; str_type = _; str_final_env = _ } = str in
  List.map str_items ~f:extract_stri


and extract_stri stri =
  let { str_desc; str_loc; str_env = _ } = stri in
  let loc = extract_loc ~loc:str_loc in
  let on_error error = decl_wrap loc @@ D_error error in
  let@@ () = try_recover ~loc ~on_error in
  match str_desc with
  | Tstr_eval _ -> raise_pre_error @@ E_unimplemented
  | Tstr_value (rec_flag, bindings) -> extract_str_let rec_flag bindings
  | Tstr_primitive value -> extract_primitive ~loc value
  | Tstr_type (_, [ decl ]) -> extract_type_decl decl
  (* TODO: should and be supported at all?? *)
  | Tstr_type (Nonrecursive, _) -> raise_pre_error @@ E_unimplemented
  | Tstr_type (Recursive, _) ->
    (* TODO: priority *)
    raise_pre_error @@ E_unimplemented
  | Tstr_typext _ -> raise_pre_error @@ E_extensible_variants_not_supported
  | Tstr_exception _ -> raise_pre_error @@ E_exceptions_not_supported
  | Tstr_module mb -> extract_module_binding mb
  | Tstr_recmodule _ -> raise_pre_error @@ E_rec_modules_not_supported
  | Tstr_modtype decl ->
    let ident, attr, sig_expr = extract_mod_type_decl decl in
    let attr = extract_signature_attrs attr in
    decl_wrap loc @@ D_module_type (ident, attr, sig_expr)
  | Tstr_open _ ->
    (* TODO: priority *)
    raise_pre_error @@ E_unimplemented
  | Tstr_class _ -> raise_pre_error @@ E_objects_not_supported
  | Tstr_class_type _ -> raise_pre_error @@ E_objects_not_supported
  | Tstr_include include_decl ->
    let mod_expr = extract_str_include include_decl in
    decl_wrap loc @@ D_module_include mod_expr
  | Tstr_attribute attr -> extract_str_attr attr


and extract_str_let rec_flag bindings =
  let binding =
    match bindings with
    | [] -> raise_pre_error @@ E_unexpected_typed_tree
    | [ value ] -> value
    | _first :: _second :: _rest ->
      (* TODO: support this? *)
      raise_pre_error @@ E_let_and_not_supported
  in
  let loc, pat, attr, value = extract_expr_binding rec_flag binding in
  let attr = extract_decl_attrs attr in
  decl_wrap loc @@ D_let (pat, attr, value)


and extract_str_include include_decl =
  let { incl_mod; incl_type = _; incl_loc = loc; incl_attributes } = include_decl in
  let loc = extract_loc ~loc in
  let@@ () = try_enhance ~loc in
  match incl_attributes with
  | [] -> extract_module_expr incl_mod
  | [ { attr_name = { txt = "ligo.internal.ocaml"; loc }
      ; attr_payload = PStr []
      ; attr_loc = _loc
      }
    ] -> extract_str_include_ocaml_predef incl_mod
  | _ -> raise_pre_error @@ E_unsupported


and extract_str_include_ocaml_predef incl_mod =
  let { mod_desc; mod_loc; mod_type = _; mod_env = _; mod_attributes } = incl_mod in
  let loc = extract_loc ~loc:mod_loc in
  let@@ () = try_enhance ~loc in
  assert (List.is_empty mod_attributes);
  (* TODO: this is weird *)
  let mod_expr =
    match mod_desc with
    | Tmod_constraint (mod_expr, _mod_type, _mod_type_constraint, _mod_coercion) ->
      (* TODO: properties of the constraint should be (sig end) *)
      mod_expr
    | Tmod_ident (_, _)
    | Tmod_structure _
    | Tmod_functor (_, _)
    | Tmod_apply (_, _, _)
    | Tmod_unpack (_, _) -> failwith "ocaml predef should have a signature"
  in
  extract_module_expr mod_expr


and extract_str_attr attr =
  let { attr_name; attr_payload = _; attr_loc } = attr in
  let loc = extract_loc ~loc:attr_loc in
  let { txt = attr_name; loc = _ } = attr_name in
  let@@ () = try_enhance ~loc in
  match attr_name with
  | "ocaml.warning" -> decl_wrap loc @@ D_attribute
  | _ -> raise_pre_error @@ E_unimplemented


and extract_mod_type_decl decl =
  let { mtd_id; mtd_name = _; mtd_type; mtd_attributes; mtd_loc } = decl in
  let loc = extract_loc ~loc:mtd_loc in
  let@@ () = try_enhance ~loc in
  match mtd_type with
  | Some mtd_type -> mtd_id, mtd_attributes, extract_mod_type mtd_type
  | None -> raise_pre_error @@ E_abstract_module_types_not_supported


and extract_mod_type mty =
  let { mty_desc; mty_type = _; mty_env = _; mty_loc; mty_attributes } = mty in
  let loc = extract_loc ~loc:mty_loc in
  let@@ () = try_enhance ~loc in
  assert (List.is_empty mty_attributes);
  match mty_desc with
  | Tmty_ident (path, _lident) -> sig_expr_wrap loc @@ S_var path
  | Tmty_signature sig_ -> sig_expr_wrap loc @@ S_sig (extract_sig sig_)
  | Tmty_functor (_, _) -> raise_pre_error @@ E_unsupported
  | Tmty_with (_, _) -> raise_pre_error @@ E_unsupported
  | Tmty_typeof _ -> raise_pre_error @@ E_unsupported
  (* TODO: this is a nice one *)
  | Tmty_alias (_, _) -> raise_pre_error @@ E_unsupported


and extract_sig sig_ =
  let { sig_items; sig_type = _; sig_final_env = _ } = sig_ in
  (* TODO: this could be extracted from sig_type
      it erases some of the syntax sugar, such as Tsig_modsubst *)
  List.map sig_items ~f:(fun sigi -> extract_sigi sigi)


and extract_sigi sigi =
  let { sig_desc; sig_env = _; sig_loc } = sigi in
  let loc = extract_loc ~loc:sig_loc in
  let on_error error = sig_item_wrap loc @@ S_error error in
  let@@ () = try_recover ~loc ~on_error in
  match sig_desc with
  | Tsig_value binding -> extract_sig_value binding
  | Tsig_type (rec_flag, bindings) -> extract_sig_type rec_flag bindings
  | Tsig_typesubst _ ->
    (* TODO: think about this one *)
    raise_pre_error @@ E_unsupported
  | Tsig_typext _ -> raise_pre_error @@ E_extensible_variants_not_supported
  | Tsig_exception _ -> raise_pre_error @@ E_exceptions_not_supported
  | Tsig_module decl -> extract_sig_module decl
  | Tsig_modsubst _ ->
    (* TODO: think about this one *)
    raise_pre_error @@ E_unsupported
  | Tsig_recmodule _ -> raise_pre_error @@ E_rec_modules_not_supported
  | Tsig_modtype decl ->
    let ident, attr, sig_expr = extract_mod_type_decl decl in
    assert (List.is_empty attr);
    let signature = signature_of_sig_expr sig_expr in
    sig_item_wrap loc @@ S_module_type (ident, signature)
  | Tsig_modtypesubst _ ->
    (* TODO: think about this one *)
    raise_pre_error @@ E_unsupported
  | Tsig_open _ ->
    (* TODO: priority *)
    raise_pre_error @@ E_unsupported
  | Tsig_include _ ->
    (* TODO: priority *)
    raise_pre_error @@ E_unsupported
  | Tsig_class _ -> raise_pre_error @@ E_objects_not_supported
  | Tsig_class_type _ -> raise_pre_error @@ E_objects_not_supported
  | Tsig_attribute _ -> raise_pre_error @@ E_unsupported


and extract_sig_value binding =
  (* TODO: assert not a primitive? *)
  let { val_id; val_name = _; val_desc = _; val_val; val_prim; val_loc; val_attributes } =
    binding
  in
  let loc = extract_loc ~loc:val_loc in
  let@@ () = try_enhance ~loc in
  assert (List.is_empty val_prim);
  assert (List.is_empty val_attributes);
  let attr = extract_sig_item_attrs val_attributes in
  (* TODO: which loc to use? *)
  let type_ =
    let { val_type; val_kind; val_loc; val_attributes; val_uid = _ } = val_val in
    (match val_kind with
    | Val_reg -> ()
    | Val_prim _ | Val_ivar (_, _) | Val_self (_, _, _, _) | Val_anc (_, _, _) ->
      raise_pre_error @@ E_unsupported);
    assert (List.is_empty val_attributes);
    let loc = extract_loc ~loc:val_loc in
    extract_type ~loc val_type
  in
  sig_item_wrap loc @@ S_value (val_id, attr, type_)


and extract_sig_type rec_flag bindings =
  let binding =
    match bindings with
    | [] -> raise_pre_error @@ E_unexpected_typed_tree
    | [ value ] -> value
    | _first :: _second :: _rest ->
      (* TODO: support this? *)
      raise_pre_error @@ E_type_and_not_supported
  in
  (* TODO: this is duplicated code *)
  (* TODO: support type constructors? *)
  (* TODO: check all of those below *)
  let { typ_id
      ; typ_name
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
    binding
  in
  let loc = extract_loc ~loc:typ_loc in
  (* TODO: handle attributes such as ligo.internal.predef *)
  assert (List.is_empty typ_attributes);
  sig_item_wrap loc @@ S_type (typ_id, extract_type_declaration typ_type)


and extract_sig_module decl =
  let { md_id; md_name = _; md_presence; md_type; md_attributes; md_loc } = decl in
  let loc = extract_loc ~loc:md_loc in
  let id =
    match md_id with
    | Some id -> id
    | None -> raise_pre_error @@ E_modules_without_names_not_supported
  in
  (match md_presence with
  | Mp_present -> ()
  | Mp_absent ->
    (* TODO: when is this the case? *)
    (* TODO: write tests *)
    raise_pre_error @@ E_unsupported);
  assert (List.is_empty md_attributes);
  let sig_expr = extract_mod_type md_type in
  let signature = signature_of_sig_expr sig_expr in
  sig_item_wrap loc @@ S_module (id, signature)


and extract_primitive ~loc vd =
  let { val_id
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
  (* TODO: use val_loc? *)
  (* TODO: check val_desc type? *)
  match val_attributes, val_prim with
  | ( [ { attr_name = { txt = "ligo.internal.constant"; loc = _ }
        ; attr_payload = PStr []
        ; attr_loc = _
        }
      ]
    , [ prim ] ) ->
    (* TODO: store which constant' *)
    let constant' =
      match Constant.read_constant' prim with
      | Some constant' -> constant'
      | None -> raise_pre_error @@ E_unsupported
    in
    decl_wrap loc @@ D_constant (val_id, constant')
  | ([] | _ :: _), prim -> raise_pre_error @@ E_unsupported


and extract_type_decl decl =
  (* TODO: support type constructors? *)
  (* TODO: check all of those below *)
  let { typ_id
      ; typ_name
      ; typ_params = _
      ; typ_type
      ; typ_cstrs = _
      ; typ_kind = _
      ; typ_private = _
      ; typ_manifest
      ; typ_loc
      ; typ_attributes
      }
    =
    decl
  in
  let loc = extract_loc ~loc:typ_loc in
  let typ_manifest_id manifest =
    let manifest =
      match manifest with
      | Some manifest -> manifest
      | None -> raise_pre_error @@ E_unsupported
    in
    let { ctyp_desc; ctyp_type; ctyp_env; ctyp_loc; ctyp_attributes } = manifest in
    let path =
      match ctyp_desc with
      | Ttyp_constr (path, _, _) -> path
      | Ttyp_any | Ttyp_var _
      | Ttyp_arrow (_, _, _)
      | Ttyp_tuple _
      | Ttyp_object (_, _)
      | Ttyp_class (_, _, _)
      | Ttyp_alias (_, _)
      | Ttyp_variant (_, _, _)
      | Ttyp_poly (_, _)
      | Ttyp_package _ -> raise_pre_error @@ E_unsupported
    in
    match path with
    | Pident id -> id
    | Pdot (_, _) -> raise_pre_error @@ E_unsupported
    | Papply (_, _) -> raise_pre_error @@ E_unsupported
  in
  let@@ () = try_enhance ~loc in
  match typ_attributes with
  | [] -> decl_wrap loc @@ D_type (typ_id, extract_type_declaration typ_type)
  | [ { attr_name = { txt = "ligo.internal.predef"; loc = _ }
      ; attr_payload = PStr []
      ; attr_loc = _
      }
    ] ->
    let { txt = constant; loc = _ } = typ_name in
    let constant =
      match Literal_types.of_string_opt constant with
      | Some constant -> constant
      | None -> raise_pre_error @@ E_unsupported
    in
    let arity = Literal_types.to_arity constant in
    assert (arity = typ_type.type_arity);
    decl_wrap loc @@ D_type_predef (typ_id, constant, arity)
  | [ { attr_name = { txt = "ligo.internal.ocaml.predef"; loc = _ }
      ; attr_payload = PStr []
      ; attr_loc = _
      }
    ] ->
    let typ_id = typ_manifest_id typ_manifest in
    let constant = Ident.name typ_id in
    let constant =
      match Literal_types.of_string_opt constant with
      | Some constant -> constant
      | None -> raise_pre_error @@ E_unsupported
    in
    let arity = Literal_types.to_arity constant in
    assert (arity = typ_type.type_arity);
    decl_wrap loc @@ D_type_predef (typ_id, constant, arity)
  | [ { attr_name = { txt = "ligo.internal.ocaml.predef.weird"; loc = _ }
      ; attr_payload = PStr []
      ; attr_loc = _
      }
    ] ->
    let typ_id = typ_manifest_id typ_manifest in
    let typ_type = { typ_type with type_attributes = [] } in
    decl_wrap loc @@ D_type (typ_id, extract_type_declaration typ_type)
  | [ { attr_name = { txt = "ligo.internal.ocaml.predef.unsupported"; loc = _ }
      ; attr_payload = PStr []
      ; attr_loc = _
      }
    ] ->
    let typ_id = typ_manifest_id typ_manifest in
    decl_wrap loc @@ D_type_unsupported typ_id
  (* TODO: better error here *)
  (* | [ { attr_name = { txt; loc = _ }; attr_payload = PStr []; attr_loc = _ } ] ->
    Format.eprintf "Unsupported attribute: %s@." txt;
    raise_pre_error @@ E_unsupported *)
  | _ -> raise_pre_error @@ E_unsupported


and extract_module_binding mb =
  let { mb_id; mb_name = _; mb_presence; mb_expr; mb_attributes; mb_loc } = mb in
  let loc = extract_loc ~loc:mb_loc in
  let@@ () = try_enhance ~loc in
  assert (
    match mb_presence with
    | Mp_present -> true
    | Mp_absent -> false);
  assert (List.is_empty mb_attributes);
  let ident =
    match mb_id with
    | Some ident -> ident
    | None -> raise_pre_error @@ E_modules_without_names_not_supported
  in
  let attr = extract_module_attrs mb_attributes in
  let body = extract_module_expr mb_expr in
  decl_wrap loc @@ D_module (ident, attr, body)


and extract_module_expr mod_expr =
  let { mod_desc; mod_loc; mod_type = _; mod_env = _; mod_attributes } = mod_expr in
  let loc = extract_loc ~loc:mod_loc in
  let@@ () = try_enhance ~loc in
  (* TODO: use module_type? *)
  assert (List.is_empty mod_attributes);
  match mod_desc with
  | Tmod_ident (path, _lident) -> mod_expr_wrap loc @@ M_var path
  | Tmod_structure str -> mod_expr_wrap loc @@ M_struct (extract_str str)
  | Tmod_functor (_, _) -> raise_pre_error @@ E_unsupported
  | Tmod_apply (_, _, _) -> raise_pre_error @@ E_unsupported
  | Tmod_constraint (_, _, _, _) -> raise_pre_error @@ E_unimplemented
  | Tmod_unpack (_, _) -> raise_pre_error @@ E_unsupported


let extract_str str =
  (* TODO: proper location here *)
  let loc = Location.dummy in
  wrap_exn ~loc (fun () -> extract_str str)
