module rec Var : sig
  type t
  type var = t

  val equal : var -> var -> bool
end =
  Var

module Univ = struct
  type univ =
    | U_meta
    | U_code

  type t = univ

  let max left right =
    match left, right with
    | U_meta, U_meta -> U_meta
    | U_meta, U_code -> U_meta
    | U_code, U_meta -> U_meta
    | U_code, U_code -> U_code
end

(* (x : A : Meta) -> B : Meta *)
(* (x : A : Code) -> B *)

module FFI = struct
  open Ligo_prim

  type ffi_type = Literal_types.t
  type ffi_intro = Literal_value.t
  type ffi_elim = Var.t Constant.t
end

module Core = struct
  open FFI

  type term =
    { term_desc : term_desc
    ; term_annot : annot
    ; term_loc : Location.t
    }

  and term_desc =
    | T_var of Var.t
    | T_let of Var.t * term * term
    (* functions *)
    | T_forall of term * Var.t * term
    | T_lambda of Var.t * term
    | T_apply of
        { funct : term
        ; arg : term
        }
    (* meta *)
    | T_lift of { type_ : term }
    | T_quote of { content : term }
    | T_splice of { quoted : term }
    (* ffi *)
    (* TODO: better naming *)
    | T_ffi_type of
        { ffi_type : ffi_type
        ; type_ : term
        }
    | T_ffi_intro of
        { ffi_intro : ffi_intro
        ; intro : term
        }
    | T_ffi_elim of
        { ffi_elim : ffi_elim
        ; elim : term
        }

  and annot =
    { annot_desc : term_desc
    ; annot_loc : Location.t
    }
end

module Goal = struct
  open Var
  open FFI

  type type_ =
    { type_desc : type_desc
    ; type_loc : Location.t
    }

  and type_desc =
    | T_arrow of type_ * type_
    | T_ffi_type of ffi_type

  type expr =
    { expr_desc : expr_desc
    ; expr_type : type_
    ; expr_loc : Location.t
    }

  and expr_desc =
    | E_var of var
    | E_let of var * expr * expr
    | E_lambda of var * expr
    | E_apply of expr * expr
    | E_ffi_intro of ffi_intro
    | E_ffi_elim of ffi_elim
end

module Machinery = struct
  open Core
  open FFI

  type level

  type value =
    { val_id : int
    ; val_desc : value_desc
    ; val_level : level
    ; mutable val_link : value
    }

  and value_desc =
    | V_thunk of env * term (* L<M> *)
    | V_meta
    | V_code
    | V_forall of closure
    | V_lambda of closure
    | V_lift of value
    | V_quote of value
    | V_ffi_type of ffi_type * value
    | V_ffi_intro of ffi_intro * value
    (* open *)
    | VO_hole
    | VO_var of Var.t
    | VO_apply of value * value
    | VO_splice of value
    | VO_ffi_elim of ffi_elim * value

  and env

  (* TODO: strong evaluation for closures *)
  and closure = (* L[x := _]<M>*)
    | Closure of env * Var.t * term

  let lookup : env -> Var.t -> value = _
  let append : env -> Var.t -> value -> env = _
  let v_null : value = _
  let is_null : value -> bool = _
  let v_level : value -> Level.t = _
  let v_desc : value -> value_desc = _
  let is_linked ctx value = _

  let rec repr value =
    let val_link = value.val_link in
    match is_null value.val_link with
    | true -> value
    | false -> _


  (* path compression *)

  let same (left : value) (right : value) : bool =
    let { val_id = left_id; val_desc = _; val_level = _; val_loc = _ } = left in
    let { val_id = right_id; val_desc = _; val_level = _; val_loc = _ } = right in
    Int.equal left_id right_id


  let v_forall : value -> closure -> value = _
  let v_lambda : closure -> value = _
  let v_lift : value -> value = _
  let v_quote : thunk -> value = _
  let v_ffi_type : ffi_type -> value -> value = _
  let v_ffi_intro : ffi_intro -> value -> value = _
  let v_thunk : env -> term -> value = _
  let vo_apply : value -> value -> value = _
  let vo_splice : value -> value = _
  let vo_ffi_elim : ffi_elim -> value -> value = _

  let rec eval env term =
    let { term_desc; term_annot = _; term_loc } = term in
    match term_desc with
    | T_var var -> _
    | T_let (var, arg, body) ->
      let arg = v_thunk env arg in
      let env = append env var arg in
      eval env body
    | T_forall (param, var, body) ->
      let param = eval env param in
      let body = Closure (env, var, body) in
      v_forall param body
    | T_lambda (var, body) ->
      let body = Closure (env, var, body) in
      v_lambda body
    | T_apply { funct; arg } ->
      let funct = eval env funct in
      let arg = eval env arg in
      eval_apply funct arg
    | T_lift { type_ } ->
      let type_ = eval env type_ in
      v_lift type_
    | T_quote { content } ->
      let content = v_thunk env content in
      v_quote content
    | T_splice { quoted } ->
      let quoted = eval env quoted in
      eval_splice quoted
    | T_ffi_type { ffi_type; type_ } ->
      let type_ = eval env type_ in
      v_ffi_type ffi_type type_
    | T_ffi_intro { ffi_intro; intro } ->
      let intro = eval env intro in
      v_ffi_intro ffi_intro intro
    | T_ffi_elim { ffi_elim; elim } ->
      let elim = eval env elim in
      eval_ffi_elim ffi_elim elim


  and eval_thunk thunk = _

  and eval_apply funct arg =
    match v_desc funct with
    | V_thunk _ ->
      let funct = eval_thunk funct in
      eval_apply funct arg
    | V_lambda body ->
      let (Closure (env, var, body)) = body in
      let env = append env var arg in
      eval env body
    | V_quote content ->
      let content = eval_thunk content in
      eval_apply content arg
    | VO_var _ | VO_apply _ | VO_splice _ | VO_ffi_elim _ -> vo_apply funct arg
    | V_meta | V_code | V_forall _ | V_lift _ | V_ffi_type _ | VO_hole ->
      failwith "eval_apply: found type"
    | V_ffi_intro _ -> failwith "eval_apply: type mismatch"


  and eval_splice quoted =
    match v_desc quoted with
    | V_thunk _ ->
      let quoted = eval_thunk quoted in
      eval_splice quoted
    | V_quote content -> eval_thunk content
    | VO_var _ | VO_apply (_, _) | VO_splice _ | VO_ffi_elim (_, _) -> vo_splice quoted
    | V_meta | V_code | V_forall _ | V_lift _ | V_ffi_type _ | VO_hole ->
      failwith "eval_splice: found type"
    | V_lambda _ | V_ffi_intro _ -> failwith "eval_splice: type mismatch"


  and eval_ffi_elim ffi_elim content = term
end

module Level = struct
  type level
  type t = level

  let max : level -> level -> level = _
end

module Stage = struct
  open Core
  open Goal
  open FFI

  type value =
    { val_id : int
    ; val_desc : value_desc
    ; val_level : Level.t
    ; mutable val_link : value
    }

  and value_desc =
    (* TODO: strong evaluation for closures *)
    | V_meta
    | V_code
    | V_forall of
        { param : value
        ; body : closure
        }
    | V_lambda of { body : closure }
    | V_lift of { type_ : value }
    | V_quote of thunk
    | V_ffi_type of
        { ffi_type : ffi_type
        ; type_ : value
        }
    | V_ffi_intro of
        { ffi_value : ffi_intro
        ; value : value
        }
    (* open *)
    | VO_hole
    | VO_var of { var : Var.t }
    | VO_apply of
        { funct : value
        ; arg : value
        }
    | VO_splice of { quoted : value }
    | VO_ffi_elim of ffi_elim * value

  and env

  and thunk = (* L<M> *)
    | Thunk of env * term

  and closure = (* L[x := _]<M>*)
    | Closure of env * Var.t * term

  let v_null : value = _
  let is_null : value -> bool = _
  let v_new : Level.t -> value_desc -> value = _
  let v_desc : value -> value_desc = _
  let v_level : value -> Level.t = _
  let e_level : env -> Level.t = _
  let c_level : closure -> Level.t = _

  (* constructors *)
  let v_forall param body =
    let level = Level.max (v_level param) (c_level body) in
    v_new level @@ V_forall { param; body }


  let v_lambda body =
    let level = c_level body in
    v_new level @@ V_lambda { body }


  let v_lift type_ =
    let level = v_level type_ in
    v_new level @@ V_lift { type_ }


  let v_quote : thunk -> value = _

  let vo_apply funct arg =
    let level = Level.max (v_level funct) (v_level arg) in
    v_new level @@ VO_apply { funct; arg }


  let vo_splice quoted =
    let level = v_level quoted in
    v_new level @@ VO_splice { quoted }


  (* environment *)
  let append_meta : env -> Var.t -> term -> env = _
  let enter_var_code : env -> Var.t -> env = _
  let reify : value -> term = _

  let rec eval_term env term =
    let { term_desc; term_annot = _; term_loc } = term in
    match term_desc with
    | T_var var -> _
    | T_let (var, arg, body) ->
      let arg = v_thunk env arg in
      let env = append env var arg in
      eval_term env body
    | T_forall (param, var, body) ->
      let param = eval_term env param in
      let body = Closure (env, var, body) in
      v_forall param body
    | T_lambda (var, body) ->
      let body = Closure (env, var, body) in
      v_lambda body
    | T_apply { funct; arg } ->
      let funct = eval_term env funct in
      let arg = eval_term env arg in
      eval_apply funct arg
    | T_lift { type_ } ->
      let type_ = eval_term env type_ in
      v_lift type_
    | T_quote { content } ->
      let content = v_thunk env content in
      v_quote content
    | T_splice { quoted } ->
      let quoted = eval_term env quoted in
      eval_splice quoted
    | T_ffi_type { ffi_type; type_ } ->
      let type_ = eval_term env type_ in
      v_ffi_type ffi_type type_
    | T_ffi_intro { ffi_intro; intro } ->
      let intro = eval_term env intro in
      v_ffi_intro ffi_intro intro
    | T_ffi_elim { ffi_elim; elim } ->
      let elim = eval_term env elim in
      eval_ffi_elim ffi_elim elim


  and eval_thunk thunk = _

  and eval_apply funct arg =
    match v_desc funct with
    | V_thunk _ ->
      let funct = eval_thunk funct in
      eval_apply funct arg
    | V_lambda body ->
      let (Closure (env, var, body)) = body in
      let env = append env var arg in
      eval_term env body
    | V_quote content ->
      let content = eval_thunk content in
      eval_apply content arg
    | VO_var _ | VO_apply _ | VO_splice _ | VO_ffi_elim _ -> vo_apply funct arg
    | V_meta | V_code | V_forall _ | V_lift _ | V_ffi_type _ | VO_hole ->
      failwith "eval_apply: found type"
    | V_ffi_intro _ -> failwith "eval_apply: type mismatch"


  and eval_splice quoted =
    match v_desc quoted with
    | V_thunk _ ->
      let quoted = eval_thunk quoted in
      eval_splice quoted
    | V_quote content -> eval_thunk content
    | VO_var _ | VO_apply (_, _) | VO_splice _ | VO_ffi_elim (_, _) -> vo_splice quoted
    | V_meta | V_code | V_forall _ | V_lift _ | V_ffi_type _ | VO_hole ->
      failwith "eval_splice: found type"
    | V_lambda _ | V_ffi_intro _ -> failwith "eval_splice: type mismatch"


  and eval_ffi_elim ffi_elim content = term

  let eval_annot : env -> annot -> value = _
  let split_quote : value -> Goal.term = _

  let rec stage_term env term =
    let { term_desc; term_annot; term_loc } = term in
    let expr_type = stage_annot env term_annot in
    let e_wrap desc = { expr_desc = desc; expr_type; expr_loc = term_loc } in
    match term_desc with
    | T_splice quoted ->
      let quoted = eval_term env quoted in
      let env, quoted = split_quote quoted in
      stage_term env quoted
    (* implicit splice *)
    | T_quote content -> stage_term env content
    | T_var var -> e_wrap @@ E_var var
    | T_let (var, arg, body) ->
      let arg = stage_term env arg in
      let body =
        let env = enter_var_code env var in
        stage_term env body
      in
      e_wrap @@ E_let (var, arg, body)
    | T_lambda (var, body) ->
      let body =
        let env = enter_var_code env var in
        stage_term env body
      in
      e_wrap @@ E_lambda (var, body)
    | T_apply { funct; arg } ->
      let funct = stage_term env funct in
      let arg = stage_term env arg in
      e_wrap @@ E_apply (funct, arg)
    | T_ffi_intro { ffi_intro; intro = _ } ->
      (* TODO: do something with this intro? *)
      e_wrap @@ E_ffi_intro ffi_intro
    | T_ffi_elim { ffi_elim; elim = _ } ->
      (* TODO: do something with this elim? *)
      e_wrap @@ E_ffi_elim ffi_elim
    (* *)
    | T_ffi_type _ | T_forall (_, _, _) | T_lift _ -> failwith "stage_term: type error"


  and stage_annot env annot =
    let annot = eval_annot env annot in
    match v_desc annot with
    | V_forall { param; body } -> _
    | V_ffi_type { ffi_type; type_ = _ } -> _
    | V_meta -> _
    | V_code -> _
    | V_lambda _ -> _
    | V_lift _ -> _
    | V_quote _ -> _
    | V_ffi_intro (_, _) -> _
    | VO_hole -> _
    | VO_var _ -> _
    | VO_apply (_, _) -> _
    | VO_splice _ -> _
    | VO_ffi_elim (_, _) -> _


  let rec subtype ~received ~expected =
    match v_desc received, v_desc expected with
    | ( V_forall { param = received_param; body = received_body }
      , V_forall { param = expected_param; body = expected_body } ) ->
      subtype ~received:expected_param ~expected:received_param;
      subtype_under ~received:received_body ~expected:expected_body
    | V_lift { type_ = received_type }, V_lift { type_ = expected_type } ->
      subtype ~received:received_type ~expected:expected_type
    | ( V_ffi_type { ffi_type = received_ffi; type_ = received_type }
      , V_ffi_type { ffi_type = expected_ffi; type_ = expected_type } ) ->
      (* TODO: do something with received_ffi and expected_ffi *)
      subtype ~received:received_type ~expected:expected_type
    
    | _, _ > _


  and subtype_under ~received ~expected = _
end

module Extract = struct
  open Core
  open Typedtree

  let t_unit ~loc () = _
  let pair ~loc left right = _
  let t_arrow ~loc param body = _
  let t_if ~loc (pred : term) (then_ : term) (else_ : term) : term = _
  let x = Typedtree.Computation

  let rec extract_type ~loc ctx type_ =
    let x = Btype.mark_type in
    assert false


  and extract_type_desc ~loc ctx type_ = _

  let rec extract_type ~loc ctx type_ =
    let x = Types.get_id in
    match Types.get_desc type_ with
    | Tvar _label -> _
    | Tarrow (Nolabel, param, body, comm) ->
      (* TODO: they need to have the same universe *)
      let param = extract_type ~loc param in
      let body = extract_type ~loc body in
      t_arrow ~loc param body
    | Tarrow (_, _, _, _) -> _
    | Ttuple els -> _
    | Tconstr (_, _, _) -> _
    | Tobject (_, _) -> _
    | Tfield (_, _, _, _) -> _
    | Tnil -> _
    | Tlink _ -> _
    | Tsubst (_, _) -> _
    | Tvariant _ -> _
    | Tunivar _ -> _
    | Tpoly (_, _) -> _
    | Tpackage (_, _) -> _


  let rec extract_expr expr =
    let { exp_desc; exp_loc = loc; exp_extra; exp_type; exp_env; exp_attributes } =
      expr
    in
    match exp_desc with
    | Texp_ident (path, _lid, value_desc) ->
      (* TODO: fix this *)
      let x = value_desc.val_kind in
      assert (
        match value_desc.val_kind with
        | Val_reg -> true
        | Val_prim _ | Val_ivar (_, _) | Val_self (_, _, _, _) | Val_anc (_, _, _) ->
          false);
      _
    | Texp_constant _ -> _
    | Texp_let (_, _, _) -> _
    | Texp_function _ -> _
    | Texp_apply (funct, args) -> _
    | Texp_match (pred, cases, Total) ->
      let pred = extract_expr pred in
      let cases = _ in
      _
    | Texp_match (pred, cases, Partial) -> _
    | Texp_try (_, _) -> _
    | Texp_tuple els ->
      (* TODO: use the same encoding as CameLigo *)
      let last = unit ~loc () in
      List.fold_right els ~init:last ~f:(fun el tl -> pair ~loc el tl)
    | Texp_construct (_, _, _) -> _
    | Texp_variant (_, _) -> _
    | Texp_record _ -> _
    | Texp_field (_, _, _) -> _
    | Texp_setfield (_, _, _, _) -> _
    | Texp_array _ -> _
    | Texp_ifthenelse (pred, then_, else_) ->
      let pred = extract_expr pred in
      let then_ = extract_expr then_ in
      let else_ =
        match else_ with
        | None -> t_unit ~loc ()
        | Some else_ -> extract_expr else_
      in
      t_if ~loc pred then_ else_
    | Texp_sequence (_, _) -> _
    | Texp_while (_, _) -> _
    | Texp_for (_, _, _, _, _, _) -> _
    | Texp_send (_, _) -> _
    | Texp_new (_, _, _) -> _
    | Texp_instvar (_, _, _) -> _
    | Texp_setinstvar (_, _, _, _) -> _
    | Texp_override (_, _) -> _
    | Texp_letmodule (_, _, _, _, _) -> _
    | Texp_letexception (_, _) -> _
    | Texp_assert _ -> _
    | Texp_lazy _ -> _
    | Texp_object (_, _) -> _
    | Texp_pack _ -> _
    | Texp_letop _ -> _
    | Texp_unreachable -> _
    | Texp_extension_constructor (_, _) -> _
    | Texp_open (_, _) -> _


  and extract_mod_expr = _
end

module Typed = struct
  open FFI

  type value

  type term =
    { term_desc : term_desc
    ; term_annot : term_desc
    ; term_loc : Location.t
    }

  and term_desc =
    (* TODO: nominal / shape? *)
    | T_var of Var.t
    | T_let of term * Var.t * term
    (* functions *)
    | T_forall of term * Var.t * term
    | T_lambda of term * Var.t * term
    | T_apply of term * term
    (* meta *)
    | T_lift of term
    | T_quote of term
    | T_splice of term
    (* ffi *)
    | T_ffi_type of ffi_type * term
    | T_ffi_intro of ffi_intro * term
    | T_ffi_elim of ffi_elim * term

  and annot =
    { annot_desc : term_desc
    ; annot_loc : Location.t
    }
end

module Elaborate = struct
  open Core
  open Typed

  type value
  type coerce
  type closure
  type context

  let solve : context -> Var.t -> value = _
  let enter : context -> Var.t -> value -> context = _
  let split_forall : value -> coerce * value * closure = _
  let subtype : received:value -> expected:value -> coerce = _
  let thunk : context -> term -> value = _
  let with_var : closure -> Var.t -> value = _
  let with_subst : closure -> value -> value = _
  let ( let@@ ) = ( @@ )

  (* TODO: currently not doing elaboration *)
  let rec infer_term ctx term =
    let Core.{ term_desc; term_loc } = term in
    let return type_ desc = type_, { term_desc = desc; term_loc } in
    match term_desc with
    | T_annot (term, annot) ->
      let received = infer_annot ctx term annot in
      check_term ctx term received
    | T_var var ->
      let received = solve ctx var in
      return received @@ T_var var
    | _ -> _


  and check_term ctx term (expected : value) =
    let Core.{ term_desc; term_loc } = term in
    let return desc = { term_desc = desc; term_type = expected; term_loc } in
    match term_desc with
    | T_annot (term, annot) ->
      let received = infer_annot ctx term annot in
      let coerce = subtype ~received ~expected in
      plug coerce @@ check_term ctx term received
    | T_var var ->
      let received = solve ctx var in
      let coerce = subtype ~received ~expected in
      return @@ T_var var
    | T_let (arg, var, body) ->
      (* TODO: universe of both should be the same? *)
      let arg, arg_type = infer_term ctx arg in
      let body =
        let ctx = enter ctx var arg_type in
        check_term ctx body expected
      in
      return @@ T_let (arg, var, body)
    | T_forall (param, var, body) ->
      (* TODO: univ *)
      let univ = split_univ expected in
      let param_type = check_term ctx param univ in
      let body_type =
        let param_type = thunk ctx param_type in
        let ctx = enter ctx var param_type in
        check_term ctx body univ
      in
      return @@ T_forall (param_type, var, body_type)
    | T_lambda (var, body) ->
      let coerce, param_type, body_type = split_forall expected in
      let body =
        let ctx = enter ctx var param_type in
        let body_type = with_var body_type var in
        check_term ctx body body_type
      in
      plug coerce @@ return @@ T_lambda (var, body)
    | T_apply (funct, arg) ->
      let funct, param, body_type =
        let funct, forall = infer_term ctx funct in
        let coerce, param, body_type = split_forall forall in
        let funct = plug coerce funct in
        funct, param, body_type
      in
      let arg = check_term ctx arg param in
      let coerce =
        let arg = thunk ctx arg in
        let received = with_subst body_type arg in
        subtype ~received ~expected
      in
      plug coerce @@ return @@ T_apply (funct, arg)
    | T_lift type_ ->
      let x = _ in
      _
    | T_splice term ->
      let expected = lift expected in
      let term = check_term ctx term expected in
      return @@ T_splice term
    | T_quote term ->
      let term = split_lift expected in
      _
      (* TODO: check ffi *)
    | T_ffi_type (ffi, term) ->
      let term = check_term ctx term expected in
      _
    | T_ffi_intro (ffi, term) ->
      let expected = split_ffi_type expected in
      let term = check_term ctx term expected in
      return @@ T_ffi_intro (ffi, term)
    | T_ffi_elim (ffi, term) ->
      let term, type_ = infer_term ctx term in
      let received = split_ffi_type type_ in
      let coerce = subtype ~received ~expected in
      plug coerce @@ return @@ T_ffi_elim (ffi, term)


  and check_annot ctx type_ ~univ = _
end

module Machinery = struct
  open FFI
  open Core
  open Ligo_prim

  module Level = struct
    type t = int

    let ( < ) : t -> t -> bool = _
  end

  type value =
    { val_id : int
    ; val_desc : value_desc
    ; val_level : Level.t
    ; mutable val_link : value
    }

  and value_desc =
    (* TODO: strong evaluation for closures *)
    | V_meta
    | V_code
    | V_forall of closure
    | V_lambda of closure
    | V_lift of value
    | V_quote of thunk
    | V_ffi_type of ffi_type * value
    | V_ffi_intro of ffi_intro * value
    (* open *)
    | VO_hole
    | VO_var of Var.t
    | VO_apply of value * value
    | VO_splice of value
    | VO_ffi_elim of ffi_elim * value

  and env

  and thunk = (* L<M> *)
    | Thunk of env * term

  and closure = (* L[x := _]<M>*)
    | Closure of env * Var.t * term

  let rec reify_term env term =
    let open Goal in
    let { term_desc; term_annot; term_loc } = term in
    match term_desc with
    | T_var _ -> _
    | T_let (_, _, _) -> _
    | T_forall (_, _, _) -> _
    | T_lambda (_, _) -> _
    | T_apply (_, _) -> _
    | T_lift _ -> _
    | T_splice _ -> _
    | T_quote _ -> _
    | T_ffi_type (_, _) -> _
    | T_ffi_intro (_, _) -> _
    | T_ffi_elim (_, _) -> _


  and reify_annot env annot = _

  let lookup : env -> Var.t -> value = _
  let append : env -> Var.t -> value -> env = _
  let v_null : value = _
  let is_null : value -> bool = _
  let v_level : value -> Level.t = _
  let v_desc : value -> value_desc = _
  let is_linked ctx value = _

  let rec repr value =
    let val_link = value.val_link in
    match is_null value.val_link with
    | true -> value
    | false -> _


  (* path compression *)

  let same (left : value) (right : value) : bool =
    let { val_id = left_id; val_desc = _; val_level = _; val_loc = _ } = left in
    let { val_id = right_id; val_desc = _; val_level = _; val_loc = _ } = right in
    Int.equal left_id right_id


  let rec unify_check ~at hole ~in_ =
    (* this is *)
    match Level.(at < v_level in_) with
    | true -> ()
    | false ->
      (match same hole in_ with
      | true -> failwith "unify_check: occurs and escape check failed"
      | false -> unify_check_ext ~at hole ~in_)


  and unify_check_ext ~at hole ~in_ =
    match v_desc in_ with
    | V_univ -> _
    (* TODO: this could benefit from strong evaluation *)
    | V_forall (param, body) ->
      unify_check ~at hole ~in_:param;
      unify_check_closure ~at hole ~in_:body
    | V_lambda (param, body) ->
      unify_check ~at hole ~in_:param;
      unify_check_closure ~at hole ~in_:body
    | V_lift type_ -> unify_check ~at hole ~in_:type_
    | V_quote term -> unify_check ~at hole ~in_:term
    | V_ffi_type (_, _) -> _
    | V_ffi_intro (_, _) -> _
    | VO_var var -> _
    | VO_apply (funct, arg) ->
      unify_check ~at hole ~in_:funct;
      unify_check ~at hole ~in_:arg
    | VO_splice term -> unify_check ~at hole ~in_:term
    | VO_ffi_elim (_, _) -> _


  and unify_check_closure ~at hole ~in_ = _

  let v_forall : value -> closure -> value = _
  let v_lambda : closure -> value = _
  let v_lift : value -> value = _
  let v_quote : thunk -> value = _
  let vo_apply : value -> value -> value = _
  let vo_splice : value -> value = _

  let split_var value =
    match v_desc value with
    | VO_var var -> var
    | V_univ
    | V_forall (_, _)
    | V_lambda (_, _)
    | V_lift _ | V_quote _
    | V_ffi_type (_, _)
    | V_ffi_intro (_, _)
    | VO_apply (_, _)
    | VO_splice _
    | VO_ffi_elim (_, _) -> _


  let rec unify ctx received expected =
    match same received expected with
    | true -> ()
    | false ->
      (* TODO: short cut *)
      unify_ext ctx received expected


  and equal_var ctx ~received ~expected =
    match Var.equal received expected with
    | true -> ()
    | false ->
      (match is_linked ctx received, is_linked ctx expected with
      | Some received, Some expected -> equal ctx ~received ~expected
      | Some received, None ->
        let received = split_var received in
        equal_var ctx ~received ~expected
      | None, Some expected ->
        let expected = split_var expected in
        equal_var ctx ~received ~expected
      | None, None -> failwith "equal_var: var clash")


  and unify_ext ctx received expected =
    match v_desc received, v_desc expected with
    | V_meta, V_meta -> ()
    | V_code, V_code -> ()
    | V_forall received, V_forall expected -> unify_ext_closure ctx received expected
    | V_lambda received, V_lambda expected -> unify_ext_closure ctx received expected
    | V_lift received, V_lift expected -> equal ctx ~received ~expected
    | V_quote received, V_quote expected -> equal ctx ~received ~expected
    | V_ffi_type (received_ffi, received), V_ffi_type (expected_ffi, expected) ->
      assert (Literal_types.equal received_ffi expected_ffi);
      equal ctx ~received ~expected
    | V_ffi_intro (received_ffi, received), V_ffi_intro (expected_ffi, expected) ->
      assert (Literal_value.equal received_ffi expected_ffi);
      equal ctx ~received ~expected
    | VO_var received, VO_var expected -> equal_var ctx ~received ~expected
    | VO_apply (received_funct, received_arg), VO_apply (expected_funct, expected_arg) ->
      equal ctx ~received:received_funct ~expected:expected_funct;
      equal ctx ~received:received_arg ~expected:expected_arg
    | VO_splice received, VO_splice expected -> equal ctx ~received ~expected
    | VO_ffi_elim (received_ffi, received), VO_ffi_elim (expected_ffi, expected) ->
      let received_args = received_ffi.arguments in
      let expected_args = expected_ffi.arguments in
      equal_ffi_elim ctx ~received_args ~expected_args;
      equal ctx ~received ~expected
    | _, _ -> _


  and unify_ext_closure ctx received expected =
    let received =
      let (Closure (env, var, body)) = received_body in
      eval env ~var body
    in
    let expected =
      let (Closure (env, var, body)) = expected_body in
      eval env ~var body
    in
    equal ctx ~received ~expected


  and equal_under ctx ~received ~expected =
    let received =
      let (Closure (env, var, body)) = received_body in
      eval env ~var body
    in
    let expected =
      let (Closure (env, var, body)) = expected_body in
      eval env ~var body
    in
    equal ctx ~received ~expected


  and equal_ffi_elim ctx ~received_args ~expected_args =
    match received_args, expected_args with
    | [], [] -> ()
    | received :: received_args, expected :: expected_args ->
      equal_var ctx ~received ~expected;
      equal_ffi_elim ctx ~received_args ~expected_args
    | _ :: _, [] | [], _ :: _ -> failwith "equal_ffi_elim: args clash"


  let rec subtype ctx ~received ~expected =
    match same received expected with
    | true -> ()
    | false ->
      let x = subtype_ext ctx ~received ~expected in
      _


  and subtype_var ctx ~received ~expected = _

  and subtype_ext ctx ~received ~expected =
    match v_desc received, v_desc expected with
    | V_univ, V_univ -> ()
    | V_forall _, V_forall _ -> _
    | V_lambda _, V_lambda _ -> _
    | V_lift _, V_lift _ -> _
    | V_quote _, V_quote _ -> _
    | V_ffi_type _, V_ffi_type _ -> _
    | V_ffi_intro _, V_ffi_intro _ -> _
    | VO_var _, VO_var _ -> _
    | VO_apply _, _ -> _
    | VO_splice _, _ -> _
    | VO_ffi_elim _, _ -> _
    | _, _ -> _
end

module Typer = struct
  open Core
  open Typed

  let rec infer_term env term =
    let Core.{ term_desc; term_loc } = term in
    match term_desc with
    | T_annot (_, _) -> _
    | T_var var -> _
    | T_forall (param, body) -> _
    | T_lambda (param, body) -> _
    | T_apply (funct, arg) ->
      let funct = infer_term env funct in
      let arg = check_term env term in
      _
    | T_let (_, _, _) -> _
    | T_lift _ -> _
    | T_splice _ -> _
    | T_quote _ -> _


  and check_term env term ~expected = _
  and infer_pat env pat = _
  and check_annot env term = _
end

module Lower = struct
  type type_ =
    | T_string
    | T_arrow of type_ * type_

  type expr =
    { expr_desc : expr_desc
    ; expr_type : type_
    ; expr_loc : Location.t
    }

  and expr_desc =
    | E_var of var
    | E_lambda of pat * expr
    | E_apply of expr * expr
    | E_let of pat * expr * expr
    | E_string of string

  and pat
end
