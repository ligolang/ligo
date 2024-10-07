open Ocaml_common
open Asttypes
open Types
open Parsetree
open Typedtree
open Ligo_prim
open Ast_core
open Caml_core

(* TODO: error recovery *)
(* TODO: non existential GADT's and FCM could be supported *)
(* TODO: a lot of existential could also be supported *)
(* TODO: use more core type *)
(* TODO: a general limitation of this,
  is that it only accepts code in more or less strict format
  simple things such as adding a let may break extraction  *)
(* TODO: this pass mostly shrinksn the OCaml tree *)
module Context : sig
  (* TODO: improve this docs *)
  (* This context relies on mutation to track locations
    as long as the context is not captured and leaked, this can be
    seen as an implementation of the reader monad.
    
    In general it leads to a nicer API with smaller types. *)
  type context
  type t = context

  (* location *)
  val enter_region : loc:Location.t -> context -> context
  val loc : context -> Location.t

  (* errors *)
  val error_unexpected_typed_tree : context -> 'a
  val error_let_and_not_supported : context -> 'a
  val error_type_and_not_supported : context -> 'a
  val error_labelled_parameters_not_supported : context -> 'a
  val error_optional_parameters_not_supported : context -> 'a
  val error_poly_vars_not_supported : context -> 'a
  val error_fcm_not_supported : context -> 'a
  val error_objects_not_supported : context -> 'a
  val error_partial_match_not_supported : context -> 'a
  val error_exceptions_not_supported : context -> 'a
  val error_extensible_variants_not_supported : context -> 'a
  val error_mutation_not_supported : context -> 'a
  val error_array_not_supported : context -> 'a
  val error_while_not_supported : context -> 'a
  val error_for_not_supported : context -> 'a
  val error_refutation_not_supported : context -> 'a
  val error_rec_modules_not_supported : context -> 'a
  val error_local_modules_not_supported : context -> 'a
  val error_lazy_not_supported : context -> 'a
  val error_abstract_types_not_supported : context -> 'a
  val error_abstract_module_types_not_supported : context -> 'a
  val error_modules_without_names_not_supported : context -> 'a
  val error_recursive_bindings_must_be_a_function : context -> 'a
  val error_unimplemented : context -> 'a
  val error_unsupported : context -> 'a
  val error_unreachable : context -> 'a

  (* external *)
  (* TODO: stop using exn directly here *)
  val run : (context -> 'a) -> ('a, exn) result
end = struct
  type context = Location.t
  type t = context

  let enter_region ~loc ctx =
    let (_ : Location.t) = ctx in
    loc


  (* TODO: this function is bad *)

  let loc ctx = ctx

  let error_unexpected_typed_tree ctx =
    failwith @@ Format.asprintf "unexpected typed tree at %a" Location.pp ctx


  let error_let_and_not_supported ctx =
    failwith @@ Format.asprintf "let and is not supported at %a" Location.pp ctx


  let error_type_and_not_supported ctx =
    failwith @@ Format.asprintf "type and is not supported at %a" Location.pp ctx


  let error_labelled_parameters_not_supported ctx =
    failwith
    @@ Format.asprintf "labelled parameters are not supported at %a" Location.pp ctx


  let error_optional_parameters_not_supported ctx =
    failwith
    @@ Format.asprintf "optional parameters are not supported at %a" Location.pp ctx


  let error_poly_vars_not_supported ctx =
    failwith
    @@ Format.asprintf "polymorphic variants are not supported at %a" Location.pp ctx


  let error_fcm_not_supported ctx =
    failwith
    @@ Format.asprintf "first-class modules are not supported at %a" Location.pp ctx


  let error_objects_not_supported ctx =
    failwith
    @@ Format.asprintf "classes and objects are not supported at %a" Location.pp ctx


  let error_partial_match_not_supported ctx =
    failwith
    @@ Format.asprintf "partial pattern matching is not supported at %a" Location.pp ctx


  let error_exceptions_not_supported ctx =
    failwith @@ Format.asprintf "exceptions are not supported at %a" Location.pp ctx


  let error_extensible_variants_not_supported ctx =
    failwith
    @@ Format.asprintf "extensible variants are not supported at %a" Location.pp ctx


  let error_mutation_not_supported ctx =
    failwith @@ Format.asprintf "mutation is not supported at %a" Location.pp ctx


  let error_array_not_supported ctx =
    failwith @@ Format.asprintf "array's are not supported at %a" Location.pp ctx


  let error_while_not_supported ctx =
    failwith @@ Format.asprintf "while loops are not supported at %a" Location.pp ctx


  let error_for_not_supported ctx =
    failwith @@ Format.asprintf "for loops are not supported at %a" Location.pp ctx


  let error_refutation_not_supported ctx =
    failwith @@ Format.asprintf "refutation's are not supported at %a" Location.pp ctx


  let error_rec_modules_not_supported ctx =
    failwith
    @@ Format.asprintf "recursive modules are not supported at %a" Location.pp ctx


  let error_local_modules_not_supported ctx =
    failwith @@ Format.asprintf "local modules are not supported at %a" Location.pp ctx


  let error_lazy_not_supported ctx =
    failwith @@ Format.asprintf "lazy values are not supported at %a" Location.pp ctx


  let error_abstract_types_not_supported ctx =
    failwith
    @@ Format.asprintf "abstract types are not supported YET at %a" Location.pp ctx


  let error_abstract_module_types_not_supported ctx =
    failwith
    @@ Format.asprintf "abstract module types are not supported at %a" Location.pp ctx


  let error_modules_without_names_not_supported ctx =
    failwith
    @@ Format.asprintf "modules without names not supported at %a" Location.pp ctx


  let error_recursive_bindings_must_be_a_function ctx =
    failwith
    @@ Format.asprintf "recursive bindings must be a function at %a" Location.pp ctx


  let error_unimplemented ctx =
    failwith @@ Format.asprintf "unimplemented at %a" Location.pp ctx


  let error_unsupported ctx =
    failwith @@ Format.asprintf "unsupported at %a" Location.pp ctx


  let error_unreachable ctx =
    failwith @@ Format.asprintf "unreachable at %a" Location.pp ctx


  let run k =
    (* TODO: better location here? *)
    let ctx = Location.dummy in
    try Ok (k ctx) with
    | exn -> Error exn
end

(* TODO: check all assert and failwith *)
(* TODO: improve error messages *)

open Context

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
let extract_payload_string ctx payload =
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
  | _ -> error_unsupported ctx


(* TODO: ppxlib? *)

(* TODO: use this function? *)
let _extract_ligo_constant ctx payload type_typ =
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
      ; type_loc = loc
      ; type_attributes = _
      ; type_immediate = _
      ; type_unboxed_default = _
      ; type_uid = _
      }
    =
    type_typ
  in
  let loc = extract_loc ~loc in
  let ctx = enter_region ~loc ctx in
  let constant = extract_payload_string ctx payload in
  let constant =
    match Literal_types.of_string_opt constant with
    | Some constant -> constant
    | None -> error_unsupported ctx
  in
  let arity = Literal_types.to_arity constant in
  assert (arity = expected_arity);
  constant, arity


(* TODO: better locations, maybe use core_type *)
let rec extract_type ctx type_ =
  (* TODO: allow attributes *)
  let open Ocaml_common.Types in
  (* TODO: detect and reject rectypes *)
  let loc = loc ctx in
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
            error_unreachable ctx)
    in
    let body = extract_type ctx body in
    type_wrap loc @@ T_forall (vars, body)
  (* type constructors *)
  | Tconstr (path, args, _abbrev) ->
    let args = List.map ~f:(fun arg -> extract_type ctx arg) args in
    type_wrap loc @@ T_constr (path, args)
  (* arrow *)
  | Tarrow (Nolabel, type1, type2, _comm) ->
    let type1 = extract_type ctx type1 in
    let type2 = extract_type ctx type2 in
    (* TODO: what about param_names? *)
    type_wrap loc @@ T_arrow (type1, type2)
  | Tarrow (Labelled _, _type1, _type2, _comm) ->
    error_labelled_parameters_not_supported ctx
  | Tarrow (Optional _, _type1, _type2, _comm) ->
    error_optional_parameters_not_supported ctx
  (* tuple *)
  | Ttuple fields ->
    let fields = List.map fields ~f:(fun field -> extract_type ctx field) in
    type_wrap loc @@ T_tuple fields
  (* variants *)
  | Tvariant _ -> error_poly_vars_not_supported ctx
  (* first-class modules *)
  | Tpackage _ -> error_fcm_not_supported ctx
  (* objects *)
  | Tobject _ -> error_objects_not_supported ctx
  | Tfield _ -> error_objects_not_supported ctx
  | Tnil -> error_objects_not_supported ctx
  (* machinery *)
  | Tlink _ -> error_unexpected_typed_tree ctx
  | Tsubst (_, _) -> error_unexpected_typed_tree ctx


let extract_label_declaration ctx label =
  let { ld_id; ld_mutable; ld_type; ld_loc = loc; ld_attributes; ld_uid = _ } = label in
  let loc = extract_loc ~loc in
  let ctx = enter_region ~loc ctx in
  assert (
    match ld_mutable with
    | Immutable -> true
    | Mutable -> false);
  assert (List.is_empty ld_attributes);
  let type_ = extract_type ctx ld_type in
  (* TODO: type_decl_label_wrap? *)
  { dl_id = ld_id; dl_type = type_; dl_loc = loc }


let extract_type_declaration ctx decl =
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
  let ctx = enter_region ~loc ctx in
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
          error_unreachable ctx)
  in
  match type_kind, type_manifest with
  | Type_abstract, Some manifest ->
    let manifest = extract_type ctx manifest in
    type_decl_wrap loc params @@ T_alias manifest
  | Type_abstract, None -> error_abstract_types_not_supported ctx
  | Type_record (fields, Record_regular), (None | Some _) ->
    (* TODO: does this manifest matters? *)
    let fields = List.map fields ~f:(fun label -> extract_label_declaration ctx label) in
    type_decl_wrap loc params @@ T_record fields
  | ( Type_record
        (_, (Record_float | Record_unboxed _ | Record_inlined _ | Record_extension _))
    , (Some _ | None) ) -> error_unimplemented ctx
  | Type_variant (cases, Variant_regular), (None | Some _) ->
    (* TODO: does this manifest matters? *)
    let cases =
      List.map cases ~f:(fun case ->
          let { cd_id; cd_args; cd_res; cd_loc; cd_attributes; cd_uid } = case in
          let loc = extract_loc ~loc:cd_loc in
          let ctx = enter_region ~loc ctx in
          (* TODO: maybe support GADTs syntax but not GADTs? *)
          assert (Option.is_none cd_res);
          assert (List.is_empty cd_attributes);
          match cd_args with
          | Cstr_tuple fields ->
            let fields = List.map fields ~f:(fun field -> extract_type ctx field) in
            C_tuple { dc_id = cd_id; dc_fields = fields; dc_loc = loc }
          | Cstr_record fields ->
            let fields =
              List.map fields ~f:(fun label -> extract_label_declaration ctx label)
            in
            C_record { dc_id = cd_id; dc_fields = fields; dc_loc = loc })
    in
    type_decl_wrap loc params @@ T_variant cases
  | Type_variant (_, Variant_unboxed), (Some _ | None) ->
    (* TODO: high priority *)
    error_unimplemented ctx
  | Type_open, (Some _ | None) -> error_unimplemented ctx


let extract_literal ctx constant =
  let open Literal_value in
  match constant with
  | Const_int n ->
    let n = Z.of_int n in
    Literal_int n
  | Const_char _ -> error_unsupported ctx
  | Const_string (content, _loc, None) ->
    (* TODO: use string loc? *)
    (* TODO: standard vs verbatin *)
    Literal_string (Standard content)
  | Const_string (_content, _loc, Some _tag) -> error_unsupported ctx
  | Const_float _ -> error_unsupported ctx
  | Const_int32 _ -> error_unsupported ctx
  | Const_int64 _ -> error_unsupported ctx
  | Const_nativeint _i -> error_unsupported ctx


let extract_pat_extra ctx pat_extra =
  match pat_extra with
  | Tpat_constraint _typ ->
    (* TODO: is this relevant? *)
    ()
  | Tpat_type (_, _) ->
    (* TODO: maybe *)
    error_unsupported ctx
  | Tpat_open (_, _, _) ->
    (* TODO: QoL *)
    error_unimplemented ctx
  | Tpat_unpack -> error_fcm_not_supported ctx


let extract_pat_alias : type a. _ -> a general_pattern -> unit =
 fun ctx pat ->
  (* TODO: this is just a check *)
  let { pat_desc; pat_loc; pat_extra; pat_type; pat_env; pat_attributes } = pat in
  assert (List.is_empty pat_extra);
  assert (List.is_empty pat_attributes);
  (* TODO: this should definitely be removed *)
  match pat_desc with
  | Tpat_any -> ()
  | Tpat_var (_, _) -> error_unsupported ctx
  | Tpat_alias (_, _, _) -> error_unsupported ctx
  | Tpat_constant _ -> error_unsupported ctx
  | Tpat_tuple _ -> error_unsupported ctx
  | Tpat_construct (_, _, _, _) -> error_unsupported ctx
  | Tpat_variant (_, _, _) -> error_unsupported ctx
  | Tpat_record (_, _) -> error_unsupported ctx
  | Tpat_array _ -> error_unsupported ctx
  | Tpat_lazy _ -> error_lazy_not_supported ctx
  | Tpat_value _ -> error_unsupported ctx
  | Tpat_exception _ -> error_unsupported ctx
  | Tpat_or (_, _, _) -> error_unsupported ctx


let rec extract_pat : type a. _ -> a general_pattern -> pat =
 fun ctx pat ->
  let { pat_desc; pat_loc; pat_extra; pat_type; pat_env; pat_attributes } = pat in
  let loc = extract_loc ~loc:pat_loc in
  let ctx = enter_region ~loc ctx in
  assert (List.is_empty pat_attributes);
  let () =
    List.iter pat_extra ~f:(fun (pat_extra, _loc, pat_extra_attributes) ->
        assert (List.is_empty pat_extra_attributes);
        extract_pat_extra ctx pat_extra)
  in
  let type_ = extract_type ctx pat_type in
  match pat_desc with
  | Tpat_any -> error_unimplemented ctx
  | Tpat_var (ident, _label) -> pat_wrap loc type_ @@ P_var ident
  | Tpat_alias (pat, ident, _label) ->
    (* TODO: this one may look easy, but linearity *)
    let () = extract_pat_alias ctx pat in
    pat_wrap loc type_ @@ P_var ident
  | Tpat_constant _ ->
    (* TODO: priority? *)
    error_unimplemented ctx
  | Tpat_tuple fields ->
    let fields = List.map fields ~f:(fun field -> extract_pat ctx field) in
    pat_wrap loc type_ @@ P_tuple fields
  | Tpat_construct (lident, constructor, payload, None) ->
    let label = extract_field_name lident in
    let payload = List.map payload ~f:(fun field -> extract_pat ctx field) in
    let payload =
      match payload with
      | [] -> pat_wrap loc type_ @@ P_unit
      | [ payload ] -> payload
      | payload -> pat_wrap loc type_ @@ P_tuple payload
    in
    pat_wrap loc type_ @@ P_variant (label, payload)
  | Tpat_construct (_, _, _, Some _) ->
    (* TODO: weird cases, likely should be supported *)
    error_unimplemented ctx
  | Tpat_variant (_, _, _) -> error_poly_vars_not_supported ctx
  | Tpat_record (labels, Closed) ->
    (* TODO: priority *)
    let labels =
      List.map labels ~f:(fun (lident, _label, pat) ->
          extract_field_name lident, extract_pat ctx pat)
    in
    pat_wrap loc type_ @@ P_record labels
  | Tpat_record (_labels, Open) ->
    (* TODO: priority, but linearity *)
    error_unimplemented ctx
  | Tpat_array _ -> error_array_not_supported ctx
  | Tpat_lazy _ -> error_lazy_not_supported ctx
  | Tpat_or (_, _, _) ->
    (* TODO: how hard would this one be? *)
    error_unsupported ctx
  | Tpat_value pat ->
    (* TODO: is this right? Understand Tpat_value *)
    extract_pat ctx (pat :> value general_pattern)
  | Tpat_exception _ -> error_exceptions_not_supported ctx


let var_pat_of_pat ctx pat =
  let { pat_desc; pat_type; pat_loc } = pat in
  let ctx = enter_region ~loc:pat_loc ctx in
  match pat_desc with
  | P_var ident -> var_pat_wrap pat_loc pat_type ident
  | P_unit -> error_unsupported ctx
  | P_tuple _ -> error_unsupported ctx
  | P_record _ -> error_unsupported ctx
  | P_variant _ -> error_unsupported ctx


let signature_of_sig_expr ctx sig_expr =
  let { sig_expr_desc; sig_expr_loc } = sig_expr in
  match sig_expr_desc with
  | S_var _ -> error_unsupported ctx
  | S_sig signature -> signature


let rec extract_expr ctx expr =
  let { exp_desc; exp_loc; exp_extra; exp_type; exp_env; exp_attributes } = expr in
  let loc = extract_loc ~loc:exp_loc in
  let ctx = enter_region ~loc ctx in
  let () =
    List.iter exp_extra ~f:(fun (exp_extra, _loc, exp_extra_attributes) ->
        extract_expr_extra ctx exp_extra)
  in
  assert (List.is_empty exp_attributes);
  let type_ = extract_type ctx exp_type in
  match exp_desc with
  | Texp_ident (path, _lident, value_desc) ->
    (* TODO: high priority *)
    (* TODO: assert value is a valid value, aka not primitive *)
    expr_wrap loc type_ @@ E_var path
  | Texp_constant constant ->
    let literal = extract_literal ctx constant in
    expr_wrap loc type_ @@ E_literal literal
  | Texp_let (rec_flag, values, body) ->
    extract_expr_let ctx ~loc ~type_ rec_flag values body
  (* TODO: label, exp function *)
  | Texp_function { arg_label = Nolabel; param; cases; partial = Total } ->
    let param, body = extract_expr_function ctx ~exp_env ~exp_type ~loc param cases in
    let param = var_pat_of_pat ctx param in
    expr_wrap loc type_ @@ E_lambda (param, body)
  | Texp_function { arg_label = Nolabel; partial = Partial; _ } -> error_unimplemented ctx
  | Texp_function { arg_label = Labelled _; _ } ->
    error_labelled_parameters_not_supported ctx
  | Texp_function { arg_label = Optional _; _ } ->
    error_optional_parameters_not_supported ctx
  | Texp_apply (lambda, args) -> extract_expr_apply ctx ~loc ~type_ lambda args
  | Texp_match (matchee, cases, Total) ->
    (* TODO: disc_label *)
    let matchee = extract_expr ctx matchee in
    let cases = List.map cases ~f:(fun case -> extract_case ctx case) in
    expr_wrap loc type_ @@ E_match (matchee, cases)
  | Texp_match (_, _, Partial) -> error_partial_match_not_supported ctx
  | Texp_try (_, _) -> error_exceptions_not_supported ctx
  | Texp_tuple fields ->
    let fields = List.map fields ~f:(fun field -> extract_expr ctx field) in
    let fields =
      match fields with
      | [] -> error_unreachable ctx
      | field :: fields -> Ne_list.(field :: fields)
    in
    expr_wrap loc type_ @@ E_tuple fields
  | Texp_construct (lident, _constructor, fields) ->
    let label = extract_field_name lident in
    let args = List.map fields ~f:(fun field -> extract_expr ctx field) in
    expr_wrap loc type_ @@ E_constructor (label, args)
  | Texp_variant (_, _) -> error_poly_vars_not_supported ctx
  | Texp_record { fields; representation; extended_expression } ->
    assert (Option.is_none extended_expression);
    let () =
      match representation with
      | Record_regular -> ()
      | Record_inlined _ ->
        (* TODO: is this one always okay? *)
        ()
      | Record_float | Record_unboxed _ | Record_extension _ -> error_unimplemented ctx
    in
    let fields = Array.to_list fields in
    let fields =
      List.map fields ~f:(fun (_label, definition) ->
          match definition with
          | Kept _typ ->
            (* TODO: priority *)
            error_unimplemented ctx
          | Overridden (lid, value) ->
            (* TODO: check data  of lid? *)
            let value = extract_expr ctx value in
            (* TODO: use proper location *)
            extract_field_name lid, value)
    in
    expr_wrap loc type_ @@ E_record fields
  | Texp_field (record, field, _label) ->
    let field = extract_field_name field in
    let record = extract_expr ctx record in
    expr_wrap loc type_ @@ E_field (record, field)
  | Texp_setfield (_, _, _, _) -> error_mutation_not_supported ctx
  | Texp_array _ -> error_array_not_supported ctx
  | Texp_ifthenelse (_, _, _) ->
    (* TODO: priority *)
    error_unimplemented ctx
  | Texp_sequence (_, _) ->
    (* TODO: support this? *)
    error_unimplemented ctx
  | Texp_while (_, _) -> error_while_not_supported ctx
  | Texp_for (_, _, _, _, _, _) -> error_for_not_supported ctx
  | Texp_send (_, _) -> error_objects_not_supported ctx
  | Texp_new (_, _, _) -> error_objects_not_supported ctx
  | Texp_instvar (_, _, _) -> error_objects_not_supported ctx
  | Texp_setinstvar (_, _, _, _) -> error_objects_not_supported ctx
  | Texp_override (_, _) -> error_objects_not_supported ctx
  | Texp_letmodule (_, _, _, _, _) -> error_local_modules_not_supported ctx
  | Texp_letexception (_, _) -> error_exceptions_not_supported ctx
  | Texp_assert _ -> error_unimplemented ctx
  | Texp_lazy _ -> error_unsupported ctx
  | Texp_object (_, _) -> error_objects_not_supported ctx
  | Texp_pack _ -> error_fcm_not_supported ctx
  | Texp_letop _ ->
    (* TODO: support this? Why? Option binding? *)
    error_unimplemented ctx
  | Texp_unreachable -> error_refutation_not_supported ctx
  | Texp_extension_constructor (_, _) ->
    (* TODO: this is very niche, probably not a good idea *)
    error_unsupported ctx
  | Texp_open (_, _) ->
    (* TODO: priority *)
    error_unimplemented ctx


and extract_expr_extra ctx expr_extra =
  match expr_extra with
  | Texp_constraint _ -> ()
  | Texp_coerce (_, _) -> error_unimplemented ctx
  | Texp_poly _ ->
    (* TODO: what is this? *)
    error_unimplemented ctx
  | Texp_newtype _ ->
    (* TODO: supporting this is a good idea? *)
    error_unimplemented ctx


(* let {rec, nonrec} x = N in M *)
and extract_expr_let ctx ~loc ~type_ rec_flag bindings body =
  let binding =
    match bindings with
    | [] -> error_unexpected_typed_tree ctx
    | [ value ] -> value
    | _first :: _second :: _rest ->
      (* TODO: support this? *)
      error_let_and_not_supported ctx
  in
  let _loc, pat, value = extract_expr_binding ctx rec_flag binding in
  let body = extract_expr ctx body in
  expr_wrap loc type_ @@ E_let (pat, value, body)


(* let {rec,nonrec} x = M *)
and extract_expr_binding ctx rec_flag binding =
  let { vb_pat; vb_expr; vb_attributes; vb_loc = loc } = binding in
  (* TODO: export this loc? *)
  let loc = extract_loc ~loc in
  let ctx = enter_region ~loc ctx in
  assert (List.is_empty vb_attributes);
  let pat = extract_pat ctx vb_pat in
  (* TODO: will the type of this expression be mono? *)
  (* TODO: poly value *)
  let value =
    match rec_flag with
    | Nonrecursive -> extract_expr ctx vb_expr
    | Recursive ->
      let pat = var_pat_of_pat ctx pat in
      extract_expr_recursive ctx ~self:pat vb_expr
  in
  loc, pat, value


and extract_expr_recursive ctx ~self expr =
  let { exp_desc; exp_loc; exp_extra; exp_type; exp_env; exp_attributes } = expr in
  let loc = extract_loc ~loc:exp_loc in
  let ctx = enter_region ~loc ctx in
  let () =
    List.iter exp_extra ~f:(fun (exp_extra, _loc, exp_extra_attributes) ->
        extract_expr_extra ctx exp_extra)
  in
  assert (List.is_empty exp_attributes);
  let type_ = extract_type ctx exp_type in
  match exp_desc with
  (* TODO: label, exp function *)
  | Texp_function { arg_label = Nolabel; param; cases; partial = Total } ->
    let param, body = extract_expr_function ctx ~exp_env ~exp_type ~loc param cases in
    let param = var_pat_of_pat ctx param in
    expr_wrap loc type_ @@ E_lambda_rec { self; param; body }
  | Texp_function { arg_label = Nolabel; partial = Partial; _ } -> error_unimplemented ctx
  | Texp_function { arg_label = Labelled _; _ } ->
    error_labelled_parameters_not_supported ctx
  | Texp_function { arg_label = Optional _; _ } ->
    error_optional_parameters_not_supported ctx
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
  | Texp_open (_, _) -> error_recursive_bindings_must_be_a_function ctx


and extract_expr_function ctx ~exp_env ~exp_type ~loc param cases =
  (* TODO: test both, multiple cases and single cases *)
  let param_type, body_type = split_arrow ~exp_env ~label:Nolabel exp_type in
  let param_type = extract_type ctx param_type in
  let body_type = extract_type ctx body_type in
  let body =
    let matchee = expr_wrap loc param_type @@ E_var (Pident param) in
    let cases = List.map cases ~f:(fun case -> extract_case ctx case) in
    expr_wrap loc body_type @@ E_match (matchee, cases)
  in
  let param = pat_wrap loc param_type @@ P_var param in
  param, body


and extract_expr_apply ctx ~loc ~type_ lambda args =
  (* TODO: duplicated *)
  let { exp_desc; exp_loc; exp_extra; exp_type; exp_env; exp_attributes } = lambda in
  let () =
    let loc = extract_loc ~loc:exp_loc in
    let ctx = enter_region ~loc ctx in
    let () =
      List.iter exp_extra ~f:(fun (exp_extra, _loc, exp_extra_attributes) ->
          extract_expr_extra ctx exp_extra)
    in
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
    extract_expr_apply_primitive ctx ~loc ~type_ prim args
  | _ -> extract_expr_apply_fallback ctx ~loc ~type_ lambda args


and extract_expr_apply_primitive ctx ~loc ~type_ prim args =
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
            extract_expr_extra ctx exp_extra)
      in
      assert (List.is_empty exp_attributes);
      (* TODO: this is clearly disgusting  *)
      match exp_desc with
      | Texp_constant constant -> constant
      | _ -> error_unsupported ctx
    in
    extract_expr_ligo_literals ctx ~loc ~type_ prim_name constant
  | _ -> error_unsupported ctx


and extract_expr_ligo_literals ctx ~loc ~type_ prim constant =
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
  | _ -> error_unsupported ctx


and extract_expr_apply_fallback ctx ~loc ~type_ lambda args =
  let lambda = extract_expr ctx lambda in
  let args =
    List.map
      ~f:(fun (label, arg) ->
        match label, arg with
        | Nolabel, Some arg -> extract_expr ctx arg
        | Nolabel, None -> error_unimplemented ctx
        | Labelled _, _ -> error_labelled_parameters_not_supported ctx
        | Optional _, _ -> error_labelled_parameters_not_supported ctx)
      args
  in
  expr_wrap loc type_ @@ E_apply (lambda, args)


and extract_case : type a. _ -> a case -> _ =
 fun ctx case ->
  let { c_lhs; c_guard; c_rhs } = case in
  assert (Option.is_none c_guard);
  let pat = extract_pat ctx c_lhs in
  let body = extract_expr ctx c_rhs in
  pat, body


let rec extract_str ctx str =
  let { str_items; str_type = _; str_final_env = _ } = str in
  List.map str_items ~f:(fun stri -> extract_stri ctx stri)


and extract_stri ctx stri =
  let { str_desc; str_loc; str_env = _ } = stri in
  let loc = extract_loc ~loc:str_loc in
  let ctx = enter_region ~loc ctx in
  match str_desc with
  | Tstr_eval _ -> error_unimplemented ctx
  | Tstr_value (rec_flag, bindings) -> extract_str_let ctx rec_flag bindings
  | Tstr_primitive value -> extract_primitive ctx ~loc value
  | Tstr_type (_, [ decl ]) -> extract_type_decl ctx decl
  (* TODO: should and be supported at all?? *)
  | Tstr_type (Nonrecursive, _) -> error_unimplemented ctx
  | Tstr_type (Recursive, _) ->
    (* TODO: priority *)
    error_unimplemented ctx
  | Tstr_typext _ ->
    (* TODO: this is not about exceptions *)
    error_exceptions_not_supported ctx
  | Tstr_exception _ -> error_unsupported ctx
  | Tstr_module mb -> extract_module_binding ctx mb
  | Tstr_recmodule _ -> error_rec_modules_not_supported ctx
  | Tstr_modtype decl ->
    let ident, sig_expr = extract_mod_type_decl ctx decl in
    decl_wrap loc @@ D_module_type (ident, sig_expr)
  | Tstr_open _ ->
    (* TODO: priority *)
    error_unimplemented ctx
  | Tstr_class _ -> error_objects_not_supported ctx
  | Tstr_class_type _ -> error_objects_not_supported ctx
  | Tstr_include _ ->
    (* TODO: judge this *)
    error_unimplemented ctx
  | Tstr_attribute _ -> error_unsupported ctx


and extract_str_let ctx rec_flag bindings =
  let binding =
    match bindings with
    | [] -> error_unexpected_typed_tree ctx
    | [ value ] -> value
    | _first :: _second :: _rest ->
      (* TODO: support this? *)
      error_let_and_not_supported ctx
  in
  let loc, pat, value = extract_expr_binding ctx rec_flag binding in
  let pat = var_pat_of_pat ctx pat in
  decl_wrap loc @@ D_let (pat, value)


and extract_mod_type_decl ctx decl =
  let { mtd_id; mtd_name = _; mtd_type; mtd_attributes; mtd_loc } = decl in
  assert (List.is_empty mtd_attributes);
  let loc = extract_loc ~loc:mtd_loc in
  let ctx = enter_region ~loc ctx in
  match mtd_type with
  | Some mtd_type -> mtd_id, extract_mod_type ctx mtd_type
  | None -> error_abstract_module_types_not_supported ctx


and extract_mod_type ctx mty =
  let { mty_desc; mty_type = _; mty_env = _; mty_loc; mty_attributes } = mty in
  let loc = extract_loc ~loc:mty_loc in
  let ctx = enter_region ~loc ctx in
  assert (List.is_empty mty_attributes);
  match mty_desc with
  | Tmty_ident (path, _lident) -> sig_expr_wrap loc @@ S_var path
  | Tmty_signature sig_ -> sig_expr_wrap loc @@ S_sig (extract_sig ctx sig_)
  | Tmty_functor (_, _) -> error_unsupported ctx
  | Tmty_with (_, _) -> error_unsupported ctx
  | Tmty_typeof _ -> error_unsupported ctx
  (* TODO: this is a nice one *)
  | Tmty_alias (_, _) -> error_unsupported ctx


and extract_sig ctx sig_ =
  let { sig_items; sig_type = _; sig_final_env = _ } = sig_ in
  (* TODO: this could be extracted from sig_type
      it erases some of the syntax sugar, such as Tsig_modsubst *)
  List.map sig_items ~f:(fun sigi -> extract_sigi ctx sigi)


and extract_sigi ctx sigi =
  let { sig_desc; sig_env = _; sig_loc } = sigi in
  let loc = extract_loc ~loc:sig_loc in
  let ctx = enter_region ctx ~loc in
  match sig_desc with
  | Tsig_value binding -> extract_sig_value ctx binding
  | Tsig_type (rec_flag, bindings) -> extract_sig_type ctx rec_flag bindings
  | Tsig_typesubst _ ->
    (* TODO: think about this one *)
    error_unsupported ctx
  | Tsig_typext _ -> error_extensible_variants_not_supported ctx
  | Tsig_exception _ -> error_exceptions_not_supported ctx
  | Tsig_module decl -> extract_sig_module ctx decl
  | Tsig_modsubst _ ->
    (* TODO: think about this one *)
    error_unsupported ctx
  | Tsig_recmodule _ -> error_rec_modules_not_supported ctx
  | Tsig_modtype decl ->
    let ident, sig_expr = extract_mod_type_decl ctx decl in
    let signature = signature_of_sig_expr ctx sig_expr in
    sig_item_wrap loc @@ S_module_type (ident, signature)
  | Tsig_modtypesubst _ ->
    (* TODO: think about this one *)
    error_unsupported ctx
  | Tsig_open _ ->
    (* TODO: priority *)
    error_unsupported ctx
  | Tsig_include _ ->
    (* TODO: priority *)
    error_unsupported ctx
  | Tsig_class _ -> error_objects_not_supported ctx
  | Tsig_class_type _ -> error_objects_not_supported ctx
  | Tsig_attribute _ -> error_unsupported ctx


and extract_sig_value ctx binding =
  (* TODO: assert not a primitive? *)
  let { val_id; val_name = _; val_desc = _; val_val; val_prim; val_loc; val_attributes } =
    binding
  in
  let loc = extract_loc ~loc:val_loc in
  let ctx = enter_region ~loc ctx in
  assert (List.is_empty val_prim);
  assert (List.is_empty val_attributes);
  (* TODO: which loc to use? *)
  let type_ =
    let { val_type; val_kind; val_loc; val_attributes; val_uid = _ } = val_val in
    (match val_kind with
    | Val_reg -> ()
    | Val_prim _ | Val_ivar (_, _) | Val_self (_, _, _, _) | Val_anc (_, _, _) ->
      error_unsupported ctx);
    assert (List.is_empty val_attributes);
    let loc = extract_loc ~loc:val_loc in
    let ctx = enter_region ~loc ctx in
    extract_type ctx val_type
  in
  sig_item_wrap loc @@ S_value (val_id, type_)


and extract_sig_type ctx rec_flag bindings =
  let binding =
    match bindings with
    | [] -> error_unexpected_typed_tree ctx
    | [ value ] -> value
    | _first :: _second :: _rest ->
      (* TODO: support this? *)
      error_type_and_not_supported ctx
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
  let ctx = enter_region ~loc ctx in
  (* TODO: handle attributes such as ligo.internal.predef *)
  assert (List.is_empty typ_attributes);
  sig_item_wrap loc @@ S_type (typ_id, extract_type_declaration ctx typ_type)


and extract_sig_module ctx decl =
  let { md_id; md_name = _; md_presence; md_type; md_attributes; md_loc } = decl in
  let loc = extract_loc ~loc:md_loc in
  let ctx = enter_region ~loc ctx in
  let id =
    match md_id with
    | Some id -> id
    | None -> error_modules_without_names_not_supported ctx
  in
  (match md_presence with
  | Mp_present -> ()
  | Mp_absent ->
    (* TODO: when is this the case? *)
    (* TODO: write tests *)
    error_unsupported ctx);
  assert (List.is_empty md_attributes);
  let sig_expr = extract_mod_type ctx md_type in
  let signature = signature_of_sig_expr ctx sig_expr in
  sig_item_wrap loc @@ S_module (id, signature)


and extract_primitive ctx ~loc vd =
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
  (* TODO: check val_desc type? *)
  assert (List.is_empty val_attributes);
  match val_prim with
  (* TODO: those names are duplicated *)
  | [ ("%ligo.nat" | "%ligo.tez" | "%ligo.address") ] ->
    (* TODO: store which primitve? *)
    decl_wrap loc @@ D_external val_id
  | _ -> error_unsupported ctx


and extract_type_decl ctx decl =
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
    decl
  in
  let loc = extract_loc ~loc:typ_loc in
  let ctx = enter_region ~loc ctx in
  match typ_attributes with
  | [] -> decl_wrap loc @@ D_type (typ_id, extract_type_declaration ctx typ_type)
  | [ { attr_name = { txt = "ligo.internal.predef"; loc = _ }
      ; attr_payload = PStr []
      ; attr_loc = _
      }
    ] ->
    let { txt = constant; loc = _ } = typ_name in
    let constant =
      match Literal_types.of_string_opt constant with
      | Some constant -> constant
      | None -> error_unsupported ctx
    in
    let arity = Literal_types.to_arity constant in
    assert (arity = typ_type.type_arity);
    decl_wrap loc @@ D_type_predef (typ_id, constant, arity)
  | [ { attr_name = { txt = "ligo.internal.predef.unsupported"; loc = _ }
      ; attr_payload = PStr []
      ; attr_loc = _
      }
    ] -> decl_wrap loc @@ D_type_unsupported typ_id
  | _ -> error_unsupported ctx


and extract_module_binding ctx mb =
  let { mb_id; mb_name = _; mb_presence; mb_expr; mb_attributes; mb_loc } = mb in
  let loc = extract_loc ~loc:mb_loc in
  let ctx = enter_region ~loc ctx in
  assert (
    match mb_presence with
    | Mp_present -> true
    | Mp_absent -> false);
  assert (List.is_empty mb_attributes);
  let ident =
    match mb_id with
    | Some ident -> ident
    | None -> error_unsupported ctx
  in
  let body = extract_module_expr ctx mb_expr in
  decl_wrap loc @@ D_module (ident, body)


and extract_module_expr ctx mod_expr =
  let { mod_desc; mod_loc; mod_type = _; mod_env = _; mod_attributes } = mod_expr in
  let loc = extract_loc ~loc:mod_loc in
  let ctx = enter_region ~loc ctx in
  (* TODO: use module_type? *)
  assert (List.is_empty mod_attributes);
  match mod_desc with
  | Tmod_ident (path, _lident) -> mod_expr_wrap loc @@ M_var path
  | Tmod_structure str -> mod_expr_wrap loc @@ M_struct (extract_str ctx str)
  | Tmod_functor (_, _) -> error_unsupported ctx
  | Tmod_apply (_, _, _) -> error_unsupported ctx
  | Tmod_constraint (_, _, _, _) -> error_unimplemented ctx
  | Tmod_unpack (_, _) -> error_unsupported ctx
