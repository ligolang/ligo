module M = struct end

module Ocaml_ish = struct
  open Typedtree

  (* Pattern matching is decomposed *)
  (* tuples, records and modules are decomposed *)
  (* function labels are gone *)

  (* TODO: recursive types
    - regular tree types

    *)
  (* TODO: exceptions *)
  (* TODO: effects *)
  (* TODO: objects and poly vars *)
  (* TODO: class system *)
  (* TODO: meta mutation *)
  (* TODO: lazy? *)
  (* TODO: mutual recursion system *)
  (* TODO: termination checker *)
  (* TODO: module primitive? *)
  (* TODO: extensionible variants *)

  type var
  type sort = S_meta | S_code

  type term =
    (* TODO: T_path? *)
    | T_var of var
    | T_let of var * term * term
    | T_annot of term * term * sort
    (* functions *)
    | TF_pi of var * term * term
    | TI_lambda of var * term
    | TE_apply of term * term
    (* unit *)
    | TF_unit
    | TI_unit
    | TE_drop of term * term
    (* pair *)
    | TF_sigma of var * term * term
    | TI_pair of term * term
    | TE_split of var * var * term * term
    (* either *)
    | TF_either of term * term
    | TI_left of term
    | TI_right of term
    | TE_case of term * var * term * var * term
    (* equalities *)
    | TF_eq of term * term * term
    | TI_refl
    | TE_transport of term * var * term * term
    (* meta *)
    | TF_lift of term
    | TI_quote of term
    | TE_splice of term
end

module Syntax = struct
  type var

  type term =
    | T_loc of term * Location.t
    | T_annot of term * term
    | T_meta
    | T_code
    | T_var of var
    | T_let of var * term * term
    (* *)
    | TF_pi of var * term * term
    | TI_lambda of var * term
    | TE_apply of term * term
    (* *)
    | TF_sigma of var * term * term
    | TI_pair of term * term
    | TE_split of var * var * term * term
    (* *)
    | TF_lift of term
    | TI_quote of term
    | TE_splice of term

  let var_equal : var -> var -> bool = _
end

module Core = struct
  type var = Syntax.var

  type term =
    | T_var of var
    (* *)
    | T_self of term
    | T_fix of var * term
    | T_unfold of term
    (* *)
    | T_lambda of var * term
    | T_apply of term * term
    (* *)
    | T_pair of term * term
    | T_fst of term
    | T_snd of term

  type value =
    | V_var of var
    (* *)
    | V_self of value
    | V_fix of value
    | V_unfold of value
    (* *)
    | V_pi of value * value
    | V_lambda of env * var * term
    | V_apply of value * value
    (* *)
    | V_sigma of value * value
    | V_pair of value * value
    | V_fst of value
    | V_snd of value

  and env = E_hole | E_let of env * var * value
  and thunk = Pending of env * term | Evaluated of value

  let eval_lookup env var = _

  let rec eval env term =
    match term with
    | T_var var -> _
    | T_fix (var, body) -> V_fix (env, var, body)
    | T_unfold fix -> (
        let fix = eval env fix in
        match fix with
        | V_fix (env, var, body) ->
            let env = E_let (env, var, fix) in
            eval env body
        | V_var _ | V_unfold _ | V_lambda _ | V_apply _ | V_pair _ | V_fst _
        | V_snd _ ->
            V_unfold fix)
    | T_lambda (var, body) -> V_lambda (env, var, body)
    | T_apply (funct, arg) -> (
        let funct = eval env funct in
        let arg = eval env arg in
        match funct with
        | V_lambda (env, var, body) ->
            let env = E_let (env, var, arg) in
            eval env body
        | V_var _ | V_fix _ | V_unfold _ | V_apply _ | V_pair _ | V_fst _
        | V_snd _ ->
            V_apply (funct, arg))
    | T_pair (fst, snd) ->
        let fst = eval env fst in
        let snd = eval env snd in
        V_pair (fst, snd)
    | T_fst pair -> (
        let pair = eval env pair in
        match pair with
        | V_pair (fst, _snd) -> fst
        | V_var _ | V_fix _ | V_unfold _ | V_lambda _ | V_apply _ | V_fst _
        | V_snd _ ->
            V_fst pair)
    | T_snd pair -> (
        let pair = eval env pair in
        match pair with
        | V_pair (_fst, snd) -> snd
        | V_var _ | V_fix _ | V_unfold _ | V_lambda _ | V_apply _ | V_fst _
        | V_snd _ ->
            V_snd pair)

  let rec eval env term =
    match term with
    | T_var _ -> _
    | T_hoist (var, body) ->
        let arg = V_forward (ref None) in
        let env = E_let (env, var, arg) in
        eval env body
    | T_fix (var, arg, body) ->
        let fix = eval_lookup env var in
        let arg = eval env arg in
        let () =
          match fix with
          | V_forward forward -> (
              match !forward with
              | None -> forward := Some (V_apply (fix, arg))
              | Some _ -> failwith "eval: forward already initialized")
          | V_var _ | V_closure _ | V_apply _ | V_thunk _ ->
              failwith "eval: not a forward"
        in
        eval env body
    | T_lambda (var, body) -> V_closure (env, var, body)
    | T_apply (funct, arg) -> _

  let delay env term = V_thunk (ref (Pending (env, term)))

  let rec eval env term =
    match term with
    | T_var var -> _
    | T_let (var, arg, body) -> _
    | T_lambda (var, body) ->
        let body = delay env body in
        V_lambda (var, body)
    | T_apply (funct, arg) -> (
        let funct = eval env funct in
        let arg = delay env arg in
        match funct with
        | V_lambda (var, body) ->
            let env = E_let (E_hole, var, arg) in
            eval_force env body
        | V_var _ -> _
        | V_apply (_, _) -> _
        | V_thunk _ -> _)

  and eval_force env value =
    match (value : value) with
    | V_var _ -> _
    | V_lambda (_, _) -> _
    | V_apply (_, _) -> _
    | V_thunk _ -> _
end

module Core = struct
  type var = Syntax.var

  type term =
    | T_var of var
    | T_block of var * term
    | T_lambda of var * term
    | T_apply of term * term

  and block = B_hole | B_hoist of var * block | B_let of var * term * block

  and value =
    | V_var of var
    | V_hoist of env * var
    | V_lambda of env * var * term
    | V_apply of value * value

  and env = E_hole | E_let of env * var * value

  let rec lookup env var = _
  (* TODO: check primitives? *)
end

module Core = struct
  type var = Syntax.var

  type term = { struct_ : term_struct; loc : Location.t }

  and term_struct =
    | T_var of var
    | T_lambda of var * term
    | T_apply of term * term
    | T_pair of term * term
    (* TODO: check primitives? *)
    | T_prim of prim

  and prim =
    | P_bool of term
    | P_true of term
    | P_false of term
    | P_case of term * term * term
    (* list *)
    | P_list of term
    | P_nil of term
    | P_cons of term * term
    | P_if_cons of term * term * term
    | P_fold_cons of term * term * term
end

module Goal = struct
  type var = Syntax.var
  type type_ = T_bool | T_arrow of type_ * type_

  type expr = { struct_ : expr_struct; loc : Location.t }

  and expr_struct =
    | E_var of var
    | E_let of var * expr * expr
    | E_lambda of var * type_ * expr * type_
    | E_apply of expr * expr
    | E_true
    | E_false
    | E_case of expr * expr * expr
    | E_nil of type_
    | E_cons of expr * expr
    | E_if_cons of expr * expr * expr

  open Core

  let rec extract_expr term =
    match (term : Core.term).struct_ with
    | T_var _ -> _
    | T_lambda (_, _) -> _
    | T_apply (_, _) -> _
    | T_pair (_, _) -> _
    | T_prim prim -> _

  and extract_expr_prim prim term =
    match prim with
    | P_bool _term ->
        (* TODO: do something about the term? *)
        _
    | P_true _term -> _
    | P_false _term -> _
    | P_case (pred, then_, else_) ->
        let pred = extract_expr pred in
        let then_ = extract_expr then_ in
        let else_ = extract_expr else_ in
        E_case (pred, then_, else_)
    | P_list _ -> _
    | P_nil _ -> _
    | P_cons (head, tail) ->
        let head = extract_expr head in
        let tail = extract_expr tail in
        E_cons (head, tail)
    | P_if_cons (pred, then_, else_) ->
        let pred = extract_expr pred in
        let then_ = extract_expr then_ in
        let else_ = extract_expr else_ in
        E_if_cons (pred, then_, else_)
    | P_fold_cons (_, _, _) -> _
end

module Extract = struct end

(* module Core = struct
  type term = 
end *)
module Core = struct
  type stage = S_meta | S_code
  type var = Syntax.var

  type term = {
    struct_ : term_struct;
    loc : Location.t;
    type_ : value;
    stage : stage;
  }

  and term_struct =
    | T_meta
    | T_code
    (* *)
    | T_var of var
    | T_let of var * term * term
    (* *)
    | T_pi of var * term * term
    | T_lambda of var * term
    | T_apply of term * term
    (* *)
    | T_sigma of var * term * term
    | T_pair of term * term
    | T_split of var * var * term * term
    (* *)
    | T_lift of term
    | T_quote of term
    | T_splice of term
    (* *)
    | T_prim of term prim

  and 'a prim =
    | P_bool of 'a
    | P_true of 'a
    | P_false of 'a
    | P_case of 'a * 'a * 'a
    (* list *)
    | P_list of 'a
    | P_nil of 'a
    | P_cons of 'a * 'a
    | P_if_cons of 'a * 'a * 'a
    | P_fold_cons of 'a * 'a * 'a

  (* TODO: locs and links on values? *)
  and value =
    | V_meta
    | V_code
    | V_var of var
    (* *)
    | V_pi of stage * value * value
    | V_closure of env * var * term
    | V_apply of value * value
    (* *)
    | V_bool
    | V_true
    | V_false
    (* TODO: lazy on both sides? Two closures? Eval both? *)
    | V_case of value * value * value
    (* *)
    | V_lift of value
    | V_quote of env * term
    | V_splice of value
    | V_prim of value prim

  and env = E_hole | E_let of env * var * value

  let t_struct : term -> term_struct = _

  let rec eval_lookup env var =
    match env with
    | E_hole ->
        failwith @@ Format.asprintf "eval_lookup: unbound var: %a" pp_var var
    | E_let (env, env_var, arg) -> (
        match Syntax.var_equal var env_var with
        | true -> arg
        | false -> eval_lookup env var)

  (* TODO: cycles? *)
  let rec eval env term =
    match t_struct term with
    | T_meta -> V_meta
    | T_code -> V_code
    | T_var var -> eval_lookup env var
    | T_let (var, arg, body) ->
        let arg = eval env arg in
        let env = E_let (env, var, arg) in
        eval env body
    | T_pi (var, param, body) ->
        let param = eval env param in
        let body = V_closure (env, var, body) in
        V_pi (param, body)
    | T_lambda (var, body) -> V_closure (env, var, body)
    | T_apply (funct, arg) ->
        let funct = eval env funct in
        let arg = eval env arg in
        eval_apply funct arg
    | T_bool -> V_bool
    | T_true -> V_true
    | T_false -> V_false
    | T_case (pred, then_, else_) ->
        let pred = eval env pred in
        eval_case env pred then_ else_
    | T_lift type_ ->
        let type_ = eval env type_ in
        V_lift type_
    | T_quote term -> V_quote (env, term)
    | T_splice quote ->
        let quote = eval env quote in
        eval_splice quote
    | T_prim prim -> _

  and eval_prim env prim =
    match prim with
    | P_bool _ -> _
    | P_true _ -> _
    | P_false _ -> _
    | P_case (pred, then_, else_) -> (
        let pred = eval env pred in
        match pred with
        | V_meta -> _
        | V_code -> _
        | V_var _ -> _
        | V_pi (_, _, _) -> _
        | V_closure (_, _, _) -> _
        | V_apply (_, _) -> _
        | V_bool -> _
        | V_true -> _
        | V_false -> _
        | V_case _ -> _
        | V_lift _ -> _
        | V_quote _ -> _
        | V_splice _ -> _
        | V_prim _ -> _)
    | P_list _ -> _
    | P_nil _ -> _
    | P_cons (_, _) -> _
    | P_if_cons (_, _, _) -> _
    | P_fold_cons (_, _, _) -> _

  and eval_apply funct arg =
    match funct with
    | V_closure (env, var, body) ->
        let env = E_let (env, var, arg) in
        eval env body
    | V_meta | V_code | V_var _ | V_pi _ | V_apply _ | V_bool | V_true | V_false
    | V_case _ | V_lift _ | V_quote _ | V_splice _ ->
        V_apply (funct, arg)

  and eval_case env pred then_ else_ =
    match pred with
    | V_true -> eval env then_
    | V_false -> eval env else_
    | V_meta | V_code | V_var _ | V_pi _ | V_closure _ | V_apply _ | V_bool
    | V_case _ | V_lift _ | V_quote _ | V_splice _ ->
        (* TODO: this is weird *)
        let then_ = eval env then_ in
        let else_ = eval env else_ in
        V_case (pred, then_, else_)

  and eval_splice quote =
    match quote with
    | V_quote (env, quote) -> eval env quote
    | V_meta | V_code | V_var _ | V_pi _ | V_closure _ | V_apply _ | V_bool
    | V_true | V_false | V_case _ | V_lift _ | V_splice _ ->
        V_splice quote

  let rec equal received expected =
    match ((received : value), (expected : value)) with
    | V_meta, V_meta -> ()
    | V_code, V_code -> ()
    | V_var received, V_var expected -> _
    | V_pi (received_param, received_body), V_pi (expected_param, expected_body)
      ->
        (* TODO: contravariance? *)
        equal received_param expected_param;
        equal received_body expected_body
    | ( V_closure (received_env, received_var, received_body),
        V_closure (expected_env, expected_var, expected_body) ) ->
        (* TODO: strong evaluation for closures *)
        let var = skolem in
        let received =
          let received_env = E_let (received_env, received_var, V_var var) in
          eval received_env received_body
        in
        let expected =
          let expected_env = E_let (expected_env, expected_var, V_var var) in
          eval expected_env expected_body
        in
        equal received expected
    | ( V_apply (received_funct, received_arg),
        V_apply (expected_funct, expected_arg) ) ->
        equal received_funct expected_funct;
        equal received_arg expected_arg
    | V_bool, V_bool -> ()
    | V_true, V_true -> ()
    | V_false, V_false -> ()
    | ( V_case (received_pred, received_then, received_else),
        V_case (expected_pred, expected_then, expected_else) ) ->
        equal received_pred expected_pred;
        equal received_then expected_then;
        equal received_else expected_else
    | V_lift received, V_lift expected -> equal received expected
    | V_quote (_, _), V_quote (_, _) ->
        (* TODO: strong evaluation here? *)
        _
    | V_splice received, V_splice expected -> equal received expected
    | V_meta, _
    | V_code, _
    | V_var _, _
    | V_pi (_, _), _
    | V_closure (_, _, _), _
    | V_apply (_, _), _
    | V_bool, _
    | V_true, _
    | V_false, _
    | V_case (_, _, _), _
    | V_lift _, _
    | V_quote (_, _), _
    | V_splice _, _ ->
        _

  let term_of_stage ~loc = _
  let make_t ~loc type_ stage struct_ = { struct_; loc; type_; stage }

  (* let t_var ~loc type_ var = make_t ~loc type_ S_meta @@ T_var var *)
  let t_lift ~loc term =
    (* TODO: rename all to sort instead of stage *)
    let sort = S_meta in
    let type_ = term_of_stage ~loc sort in
    make_t ~loc type_ sort @@ T_lift term

  let t_quote ~loc type_ stage term = make_t ~loc type_ stage @@ T_quote term
  let t_splice ~loc type_ stage term = make_t ~loc type_ stage @@ T_splice term
end

module M = struct
  open Syntax
  open Core

  type state = Infer | With_sort of stage | Check of term * stage

  type env =
    | E_hole
    | E_def of env * var * term * stage
    | E_let of env * var * term * stage * term

  let lookup env var = _

  let rec elab ~loc env term mode =
    match ((term : Syntax.term), mode) with
    | T_loc (term, loc), state ->
        (* TODO: loc stack *)
        elab ~loc env term state
    | T_annot (term, type_), Infer ->
        let type_, sort = infer_type ~loc env type_ mode in
        let term = check_term ~loc env term type_ sort in
        (term, type_, sort)
    | T_annot (term, annot), With_sort expected_sort ->
        let type_ = check_type ~loc env annot expected_sort in
        let term = check_term ~loc env term type_ expected_sort in
        (term, type_, expected_sort)
    | T_annot _, Check _ -> coerce_to_infer_term_with_sort ~loc env term mode
    | T_meta, Infer -> (T_meta, T_meta, S_meta)
    | T_meta, (With_sort _ | Check _) -> coerce_infer_term ~loc env term mode
    (* TODO: is this right? *)
    | T_code, Infer -> (T_code, T_code, S_code)
    | T_code, _ -> _
    | T_var var, Infer ->
        let type_ = lookup env var in
        t_var ~loc type_ ~var
    | T_var _, (With_sort _ | Check _) -> coerce_infer_term ~loc env term mode
    | T_let (var, arg, body), mode ->
        let arg, arg_type, arg_sort = elab ~loc env arg @@ let_arg_mode mode in
        let body, body_type, body_sort =
          let env = E_let (env, var, arg_type, arg_sort, arg) in
          elab ~loc env body mode
        in
        t_let ~loc ~var ~arg ~body
    | T_pi (var, param, body), state ->
        let param, param_sort = elab_type ~loc env param state in
        let body, body_sort =
          let env = _ in
          to_infer_type ~loc env body state
        in
        let sort, param, body =
          coerce_pi ~var ~param ~param_sort ~body ~body_sort
        in
        let type_ = type_of_sort sort in
        (T_pi (var, param, body), type_, sort)
    | T_pi (var, param, body), state ->
        let param, param_sort = to_infer_type ~loc env param state in
        let body, body_sort =
          let env = _ in
          to_infer_type ~loc env body state
        in
        let sort, param, body =
          coerce_pi ~var ~param ~param_sort ~body ~body_sort
        in
        let type_ = type_of_sort sort in
        (T_pi (var, param, body), type_, sort)
    | T_lambda (_, _), (Infer | With_sort _) -> _
    | T_lambda (var, body), Check (expected, expected_sort) ->
        let param_type, body_type = match_pi expected in
        let body =
          let env = E_def (env, var, param_type, expected_sort) in
          let body_type = eval_apply body_type @@ T_var var in
          check_term ~loc env body body_type expected_sort
        in
        (T_lambda (var, body), expected, expected_sort)
    | T_apply (funct, arg), Infer ->
        let funct, funct_type, funct_sort = infer_term ~loc env funct in
        let param_type, body_type = match_pi funct_type in
        let arg = check_term ~loc env arg param_type funct_sort in
        let type_ = _ in
        (T_apply (funct, arg), type_, funct_sort)
    | T_apply _, (With_sort _ | Check _) -> coerce_infer_term ~loc env term mode
    | T_lift type_, Infer ->
        let sort = S_meta in
        let type_ = check_type ~loc env type_ @@ S_code in
        (T_lift type_, type_of_sort sort, sort)
    | T_lift _, (With_sort _ | Check _) -> coerce_infer_type ~loc env term mode
    | T_quote content, Infer ->
        let content, content_type = infer_term_with_sort ~loc env term S_code in
        let type_ = T_lift content_type in
        (T_quote content, type_, S_meta)
    | T_quote content, With_sort expected_sort ->
        let () = match_tu_meta expected_sort in
        elab ~loc env term Infer
    | T_quote content, Check (expected, expected_sort) ->
        let () = match_tu_meta expected_sort in
        let expected = match_lift expected in
        let content = check_term ~loc env term expected S_code in
        (T_quote content, expected, expected_sort)
    | T_splice quote, Infer ->
        let quote, quote_type = infer_term_with_sort ~loc env quote @@ S_code in
        _
    | T_splice quote, With_sort expected_sort ->
        let () = match_tu_code expected_sort in
        elab ~loc env term Infer
    | T_splice quote, Check (expected, expected_sort) ->
        let () = match_tu_code expected_sort in
        let quoted_type = _ in
        let quoted = check_term ~loc env quote in
        _

  and infer_term ~loc env term = _
  and infer_term_with_sort ~loc env term expected_sort = _
  and check_term ~loc env term expected expected_sort = _
  and infer_type ~loc env type_ = _
  and check_type ~loc env type_ = _
  and to_infer_term ~loc env term state = _
  and to_infer_type ~loc env term state = _
  and elab_as_infer state ~loc env term = _

  type step =
    | S_annot
    | S_var of var
    | S_let_infer of var * term * term
    | S_let_with_sort of stage * var * term * term
    | S_let_check of term * stage * var * term * term
    | S_pi_infer of var * term * term
    | S_pi_check of stage * var * term * term
    | S_lambda of term * var * term
    | S_apply of term * term
    | S_lift of term
    | S_lift_with_sort of stage * term
    | S_quote of term
    | S_quote_
    | S_splice of term
end

module Extract = struct
  open Core
  open Goal

  type env = E_hole | E_def of env * var | E_let_meta of env * var * term
  type value

  let eval : env -> term -> value = _

  let rec extract_expr env term =
    let Core.{ struct_; type_ } = term in
    match struct_ with
    | T_var var -> _
    | T_let (var, S_code, arg, body) ->
        let arg = extract_expr env arg in
        let body =
          let env = E_def (env, var) in
          extract_expr env body
        in
        E_let (var, arg, body)
    | T_let (var, S_meta, arg, body) ->
        let env = E_let_meta (env, var, arg) in
        extract_expr env body
    | T_lambda (var, param, body) ->
        let body =
          let env = E_def (env, var) in
          extract_expr env body
        in
        E_lambda (var, body)
    | T_apply (funct, arg) ->
        let funct = extract_expr env funct in
        let arg = extract_expr env arg in
        E_apply (funct, arg)
    | T_true -> E_true
    | T_false -> E_false
    | T_case (pred, then_, else_) ->
        let pred = extract_expr env pred in
        let then_ = extract_expr env then_ in
        let else_ = extract_expr env else_ in
        E_case (pred, then_, else_)
    | T_splice quoted ->
        let quoted = eval env quoted in
        let content = match_quote quoted in
        extract_expr env content
    | T_meta -> _
    | T_code -> _
    | T_pi (_, _, _) -> _
    | T_bool -> _
    | T_lift _ -> _
    | T_quote _ -> _
end

module M = struct
  open Syntax

  type sort = S_meta | S_code

  type env =
    | E_hole
    | E_def of env * var * term * sort
    | E_let of env * var * term * sort * term

  let coerce_pi_lambda ~var ~param ~param_sort ~body_sort ~body =
    match (param_sort, body_sort) with
    | S_meta, S_meta -> (S_meta, param, body)
    | S_code, S_code -> (S_code, param, body)
    | S_meta, S_code ->
        let body = T_lift body in
        (S_code, param, body)
    | S_code, S_meta ->
        (* TODO: different from traditional 2LTT *)
        let param = T_lift param in
        let body = T_let_lower (var, T_splice (T_var var), body) in
        (S_meta, param, body)

  let rec infer env term =
    match (term : term) with
    | T_meta -> _
    | T_code -> _
    | T_var _ -> _
    | T_let (var, arg, body) ->
        let arg, arg_type, arg_sort = infer env arg in
        let body, body_type, body_sort =
          let env = E_let (env, var, arg_type, arg_sort, arg) in
          infer env body
        in
        let x, y, z = coerce_dep ~var in
        _
    | T_pi (var, param, body) ->
        let param, param_sort = infer_type env param in
        let body, body_sort =
          let env = E_def (env, var, param, param_sort) in
          infer_type env body
        in
        let sort, param, body =
          coerce_pi_lambda ~var ~param ~param_sort ~body_sort ~body
        in
        let type_ = term_of_sort in
        (T_pi (var, param, body), sort)
    | T_lambda (var, param, body) ->
        let param, param_sort = infer_type env param in
        let body, body_type, body_sort =
          let env = E_def (env, var, param, param_sort) in
          infer env body
        in
        let sort, param, body = _ in
        (T_lambda (var, param, body), _, _)
    | T_apply (funct, arg) ->
        let funct, funct_type, funct_sort = infer env funct in
        let param_type, body_type = match_pi funct_type in
        let arg = check_term env arg param_type sort in

        _
    | T_lift type_ -> _
    | T_quote content ->
        let content, content_type = infer_term_check_sort env content S_code in
        (T_quote content, T_lift content_type, S_meta)
    | T_splice quoted ->
        let quoted, quoted_type = infer_term_check_sort env quoted S_meta in
        let content_type = match_lift quoted_type in
        (T_splice quoted, content_type, S_code)

  and infer_term_check_sort env term sort = _
  and check_term env term expected expected_sort = _
  and infer_type env term = _
end

module M = struct
  open Syntax

  type sort = S_meta | S_code

  type env =
    | E_hole
    | E_def of env * var * term * sort
    | E_let of env * var * term * sort * term

  let coerce_dep ~var ~param ~param_sort ~body_sort ~body =
    match (param_sort, body_sort) with
    | S_meta, S_meta -> (S_meta, param, body)
    | S_code, S_code -> (S_code, param, body)
    | S_meta, S_code ->
        let body = T_lift body in
        (S_code, param, body)
    | S_code, S_meta ->
        (* TODO: different from traditional 2LTT *)
        let param = T_lift param in
        let body = T_let_lower (var, T_splice (T_var var), body) in
        (S_meta, param, body)

  let rec infer env term =
    match term with
    | T_let (var, arg, body) ->
        let arg, arg_type, arg_sort = infer env arg in
        let body, body_type, body_sort =
          let env = E_let (env, var, arg_type, arg_sort, arg) in
          infer env body
        in
        let x = coerce_dep in
        (body, body_type, sort)
    | T_meta -> (T_meta, T_meta, S_meta)
    | T_code -> (T_code, T_meta, S_meta)
    | T_var var -> infer_var env var
    | T_apply (funct, arg) ->
        let funct, funct_type, sort = infer env funct in
        let param_type, body_type = match_pi funct_type in
        let arg = check_term env arg param_type sort in
        let type_ = _ in
        (T_apply (funct, arg), type_, sort)
    | T_quote term ->
        (* TODO: why infer quote? *)
        let term, term_type = infer_check_sort env term S_code in
        (T_quote term, T_lift term_type, S_meta)
    | T_splice term ->
        let term, term_type = infer_check_sort env term S_meta in
        let type_ = match_quote term_type in
        (T_splice term, type_, S_code)
    | T_lambda _ -> _
    | T_pi _ -> _
    | T_lift _ -> _

  and infer_var env var =
    match env with
    | E_hole -> failwith "infer_var: unbound var"
    | E_def (env, env_var, type_, sort) | E_let (env, env_var, type_, sort, _)
      -> (
        match var_equal var env_var with
        | true -> (T_var env_var, type_, sort)
        | false -> infer_var env var)

  and infer_check_sort env term expected =
    match term with
    | T_let (var, arg, body) ->
        let arg, arg_type = infer_check_sort env arg expected in
        let body, body_type =
          let env = E_let (env, var, arg_type, expected, arg) in
          infer_check_sort env body expected
        in
        (body, body_type)
    | T_meta | T_code | T_pi _ | T_lift _ ->
        let sort = expected in
        let expected = term_of_sort expected in
        let term = check_term env term expected sort in
        (term, expected)
    | T_var _ | T_lambda _ | T_apply _ | T_quote _ | T_splice _ -> _

  and check_term env term expected expected_sort =
    match term with
    | T_let (var, arg, body) ->
        let arg, arg_type = infer_check_sort env arg expected_sort in
        let body =
          let env = E_let (env, var, arg_type, expected_sort, arg) in
          check_term env body expected expected_sort
        in
        T_let (var, arg, body)
    | T_pi (var, param, body) ->
        let expected = match_sort expected in
        let param = check_type env param expected in
        let body =
          let env = E_def (env, var, param, sort) in
          check_type env body expected
        in
        T_pi (var, param, body)
    | T_lambda (var, body) ->
        let param_type, body_type = match_pi expected in
        let body =
          let env = E_def (env, var, param_type, expected_sort) in
          let body_type = eval_apply body_type _ in
          check_term env body body_type expected_sort
        in
        T_lambda (var, body)
    | T_lift content ->
        let () = split_tu_code expected in
        let content = check_term env content T_code S_code in
        T_lift content
    | T_quote content ->
        let () = match_lift expected in
        let content = check_term env content T_code S_code in
        T_quote content
    | T_meta | T_code | T_var _ | T_apply (_, _) | T_splice _ ->
        let term, received = infer_check_sort env term expected_sort in
        let () = equal received expected in
        term

  and check_type env term expected = _
end

module Core = struct
  type var = Ocaml_ish.var

  type term =
    | TU_meta
    | TU_data
    | TU_line
    | T_var of var
    | T_let of var * term * term
    (* unit *)
    | TF_unit
    | TI_unit
    | TE_drop of term * term
    (* *)
    | TF_pi of var * term * term
    | TI_lambda of var * term
    | TE_apply of term * term
    (* *)
    | TF_sigma of var * term * term
    | TI_pair of term * term
    | TE_split of var * var * term * term
    (* *)
    | TF_either of term * term
    | TI_left of term
    | TI_right of term
    | TE_if_left of term * term * term
    (* *)
    | TF_lift of term
    | TI_quote of term
    | TE_splice of term
end

module Core = struct
  type term = { struct_ : term_struct; loc : Location.t }

  and term_struct =
    | T_var of var
    | T_let of var * term * term
    (* *)
    | TF_forall of var * term * term
    | TI_lambda of var * term
    | TE_apply of term * term
    (* *)
    | TF_either of term * term
    | TI_left of term
    | TI_right of term
    | TE_if_left of term * term * term
    (* *)
    | TF_lift of term
    | TI_quote of term
    | TE_splice of term

  and var = { mutable to_ : value }

  and value =
    | V_var of var
    | VF_forall of value * value
    | VI_lambda of var * term
    | VE_apply of value * value
    | VF_either of value * value
    | VI_left of value
    | VI_right of value
    | VE_if_left of value * value * value
    | VF_lift of value
    | VI_quote of term
    | VE_splice of value

  and env = E_hole | E_let of env * var * term

  let t_struct : term -> term_struct = _
  let v_with env var ~to_ f = _
  let ( let@@ ) = ( @@ )

  let rec eval env term =
    match t_struct term with
    | T_var var -> var.to_
    | T_let (var, arg, body) ->
        let arg = eval_lazy env arg in
        let env = E_let (env, var, arg) in
        eval env body
    | TF_forall (var, param, body) ->
        let param = eval env param in
        _
    | TI_lambda (_, _) -> _
    | TE_apply (funct, arg) -> (
        let funct = eval env funct in
        let arg = eval_lazy env arg in
        match t_struct funct with
        | TI_lambda (var, body) ->
            let@@ env = v_with env var ~to_:arg in
            eval env body
        | T_var _ | T_let _ | TF_forall _ | TE_apply _ | TF_either _ | TI_left _
        | TI_right _ | TE_if_left _ | TF_lift _ | TI_quote _ | TE_splice _ ->
            _)
    | TF_either (_, _) -> _
    | TI_left _ -> _
    | TI_right _ -> _
    | TE_if_left (_, _, _) -> _
    | TF_lift _ -> _
    | TI_quote _ -> _
    | TE_splice _ -> _

  and eval_lazy env term = _
  and eval_apply env funct arg = _

  let equal received expected = _
end

module Checker = struct
  open Ocaml_ish
  open Core

  type env =
    | E_hole
    | E_def of env * var * term
    | E_let of env * var * term * term

  let eval_apply funct arg = _
  let lift_sort = _
  let unit_sort = _
  let unit_type = TF_unit
  let pair_sort = _

  let rec infer_term env term =
    match (term : Ocaml_ish.term) with
    | T_var var -> _
    | T_let (var, arg, body) -> _
    | T_annot (term, type_, sort) ->
        let type_ = check_type env type_ sort in
        let term = check_term env term type_ in
        (term, type_)
    | TE_apply (funct, arg) ->
        let funct, funct_type = infer_term env funct in
        let param_type, body_type = match_pi funct_type in
        let arg = check_term env arg param_type in
        let type_ =
          (* TODO: arg env ? *)
          let arg = _ in
          eval_apply funct arg
        in
        (TE_apply (funct, arg), body_type)
    | TE_drop (unit, body) ->
        let unit = check_term env unit unit_type in
        let body, body_type = infer_term_with_sort env body in
        (* TODO: check body type universe *)
        let x = _ in
        (TE_drop (unit, body), body_type)
    | TE_split (fst_var, snd_var, pair, body) ->
        let pair, pair_type = infer_term env pair in
        let fst_type, snd_type = split_sigma pair_type in
        let body, body_type =
          let env = E_def (env, fst_var, fst_type) in
          let env = E_def (env, snd_var, snd_type) in
          infer_term_with_sort env body
        in
        _
    | TE_case (either, then_var, then_, else_var, else_) ->
        let either, either_type = infer_term env either in
        let left_type, right_type = split_either either_type in
        let then_, then_type =
          let env = E_def (env, then_var, left_type) in
          infer_term env then_
        in
        let else_, else_type =
          let env = E_def (env, else_var, right_type) in
          infer_term env else_
        in
        let () = equal then_type else_type in
        _
    | TE_transport (_, _, _, _) -> _
    | TE_splice quoted ->
        let quoted, quoted_type = infer_term env quoted in
        let content_type = split_lift quoted_type in
        (TE_splice quoted, content_type)
    | TF_pi (_, _, _) -> _
    | TF_unit -> _
    | TF_sigma (_, _, _) -> _
    | TF_either (_, _) -> _
    | TF_eq (_, _, _) -> _
    | TF_lift type_ -> _
    | TI_lambda (_, _) -> _
    | TI_unit -> _
    | TI_pair (_, _) -> _
    | TI_left _ -> _
    | TI_right _ -> _
    | TI_refl -> _
    | TI_quote content -> _

  and check_term env term expected =
    match (term : Ocaml_ish.term) with
    | T_var _ -> _
    | T_let _ -> _
    | TI_lambda (var, body) ->
        let param_type, body_type = match_pi expected in
        let body =
          let env = E_def (env, var, param_type) in
          let body_type = eval_apply body_type _ in
          check_term env body body_type
        in
        TI_lambda (var, body)
    | TI_unit ->
        let () = match_unit expected in
        TI_unit
    | TI_pair (fst, right) ->
        let fst_type, snd_type = match_sigma expected in
        let fst = check_term env fst fst_type in
        let snd =
          (* TODO: variable for fst? *)
          let snd_type = eval_apply _ fst in
          check_term env right snd_type
        in
        _
    | TI_left _ -> _
    | TI_right _ -> _
    | TI_refl -> _
    | TI_quote _ -> _
    | TF_pi _ | TF_unit | TF_sigma _ | TF_either _ | TF_eq _ | TF_lift _ ->
        let sort = match_sort expected in
        check_type env term expected sort
    | T_annot _ | TE_apply _ | TE_drop _ | TE_split _ | TE_case _
    | TE_transport _ | TE_splice _ ->
        _

  and check_type env term expected : Core.term =
    match (term : Ocaml_ish.term) with
    | T_var _ | T_let _ | TF_pi (var, param, body) ->
        let param = check_type env param expected in
        let body =
          let env = E_def (env, var, param) in
          check_type env body expected
        in
        TF_pi (var, param, body)
    | TF_unit ->
        let () = split_tu_code expected in
        TF_unit
    | TF_sigma (var, fst, snd) ->
        let fst = check_type env fst expected in
        let snd =
          let env = E_def (env, var, fst) in
          check_type env snd expected
        in
        TF_sigma (var, fst, snd)
    | TF_either (left, right) ->
        let () = split_tu_code expected in
        let left = check_type env left expected in
        let right = check_type env right expected in
        TF_either (left, right)
    | TF_eq (type_, left, right) ->
        let type_ = check_term env type_ S_code in
        let left = check_term env left type_ in
        let right = check_term env right left_type in
        TF_eq (left, right)
    | TF_lift content_type ->
        let () = split_tu_code expected in
        let content_type = check_type env content_type S_code in
        TF_lift content_type
    | TI_lambda _ | TI_unit | TI_pair _ | TI_left _ | TI_right _ | TI_refl
    | TI_quote _ ->
        _
    | TE_apply _ | TE_drop _ | TE_split _ | TE_case _ | TE_transport _
    | TE_splice _ ->
        _
end

module Goal = struct
  type var
  type type_ = T_bool | T_arrow of type_ * type_

  type expr =
    | E_var of var
    | E_let of var * expr * expr
    | E_lambda of var * expr
    | E_apply of expr * expr
    | E_true
    | E_false
    | E_if of expr * expr * expr
end

module Extract = struct
  open Syntax
  open Goal

  let rec extract_type term =
    match (term : Syntax.term) with
    | T_bool -> T_bool
    | T_forall (param, body) -> (
        let param = extract_type param in
        match body with
        | T_lambda (_var, body) ->
            let body = extract_type body in
            T_arrow (param, body)
        | _ -> _)
    | T_var _ -> _
    | T_let (_, _, _) -> _
    | T_lambda (_, _) -> _
    | T_apply (_, _) -> _
    | T_true | T_false -> _
    | T_if (_, _, _) -> _
    | T_lift _ -> _
    | T_quote _ -> _
    | T_splice _ -> _

  let rec extract_expr term =
    match (term : term) with
    | T_var _ -> _
    | T_let (var, arg, body) -> _
    | T_forall (_, _) -> _
    | T_lambda (_, _) -> _
    | T_apply (funct, arg) ->
        let funct = extract_expr funct in
        let arg = extract_expr arg in
        E_apply (funct, arg)
    | T_bool -> _
    | T_true -> E_true
    | T_false -> E_false
    | T_if (pred, then_, else_) ->
        let pred = extract_expr pred in
        let then_ = extract_expr then_ in
        let else_ = extract_expr else_ in
        E_if (pred, then_, else_)
    | T_lift _ -> _
    | T_quote _ -> _
    | T_splice term -> _
end

module Syntax = struct
  type var = int

  type term =
    | T_meta
    | T_code
    | T_var of var
    | T_let of var * term * term
    | T_forall of var * term * term
    | T_lambda of var * term
    | T_apply of term * term
    | T_lift of term
    | T_quote of term
    | T_splice of term
end

module M = struct
  open Syntax

  type env
  type sort = S_meta | S_code

  let equal : env -> term -> term = _
  let split_sort : env -> term -> sort = _
  let equal : term -> term -> unit = _

  type coerce = C_hole | C_quote of coerce | C_splice of coerce

  let subtype received expected =
    (* TODO: nested subtyping *)
    match (received, expected) with
    | T_lift received, T_lift expected ->
        let () = equal received expected in
        C_hole
    | T_lift received, expected ->
        equal received expected;
        C_splice C_hole
    | received, T_lift expected ->
        equal received expected;
        C_quote C_hole
    | _ ->
        equal received expected;
        C_hole

  let split_forall : term -> term * term = _

  let coerce_dep ~var ~param ~param_sort ~body ~body_sort =
    match (param_sort, body_sort) with
    | S_meta, S_meta -> (S_meta, param, body)
    | S_code, S_code -> (S_code, param, body)
    | S_meta, S_code ->
        let body = T_lift body in
        (S_code, param, body)
    | S_code, S_meta ->
        (* TODO: fresh var *)
        let param = T_lift param in
        let body = T_let (var, T_splice (T_var var), body) in
        (S_meta, param, body)

  let rec infer_term env term =
    match (term : Syntax.term) with
    | T_meta -> _
    | T_code -> _
    | T_var _ -> _
    | T_let (var, arg, body) -> _
    | T_forall (var, param, body) ->
        let param, param_sort = infer_annot env param in
        let body, body_sort =
          let env = _ in
          infer_annot env body
        in
        let sort, param, body =
          coerce_dep ~var ~param ~param_sort ~body ~body_sort
        in
        (T_forall (var, param, body), _)
    | T_lambda (var, body) ->
        let param, param_sort = infer_annot env param in
        let body, body_type, body_sort =
          let env = _ in
          infer_term env body
        in
        let sort = _ in
        let param = coerce param param_sort ~to_:sort in
        _
    | T_apply (funct, arg) ->
        let funct, forall, sort = infer_term env funct in
        let param_type, body_type = split_forall forall in
        let arg = check_term env arg param_type in
        let type_ =
          (* TODO: is this bad? *)
          T_apply (body_type, arg)
        in
        _
    | T_lift _ -> _
    | T_quote _ -> _
    | T_splice _ -> _

  and infer_annot env term = _

  and check_term env term expected expected_sort =
    (* TODO: subtyping is needed if T_meta is ever generated *)
    let term, received, received_sort = infer_term env term in
    let term, received =
      match (received_sort, expected_sort) with
      | S_meta, S_meta -> (term, received)
      | S_code, S_code -> (term, received)
      | S_meta, S_code ->
          let received = split_lift received in
          let term = T_splice term in
          (term, received)
      | S_code, S_meta ->
          let received = T_lift received in
          let term = T_quote term in
          (term, received)
    in
    equal received expected;
    _
end

module M = struct
  type var
  type sort = S_meta | S_code

  type term = { struct_ : term_struct; type_ : term }

  and term_struct =
    | T_meta
    | T_code
    | T_var of var
    | T_let of var * term * term
    | T_forall of var * term * term
    | T_lambda of var * term * term
    | T_apply of term * term
    | T_lift of term
    | T_quote of term
    | T_splice of term

  and typed
  and type_
end

module Goal = struct
  type var
  type type_ = T_bool | T_arrow of type_ * type_

  type expr =
    | E_var of var
    | E_let of var * expr * expr
    | E_lambda of var * expr
    | E_apply of expr * expr
    | E_true
    | E_false
    | E_if of expr * expr * expr
end

module Core = struct
  type term =
    | T_meta
    | T_code
    | T_var
    | T_let of var * term * term
    | T_forall of term * term
    | T_lambda of var * term
    | T_apply of term * term
    | T_bool
    | T_true
    | T_false
    | T_case of term * term * term
    | T_lift of term
    | T_quote of term
    | T_splice of term

  and var = term
  and value = term
end

module Typer = struct
  open Syntax
  open Core

  let split_forall : term -> term * term = _
  let arrow : param:term -> return:term -> term = _ _
  let v_delay env term = _

  type sort = S_meta | S_code

  let split_sort : value -> sort = _
  let forall_type ~param_type ~body_type = _

  let rec infer_term env term =
    match (term : Syntax.term) with
    | T_var var -> _
    | T_let (var, arg, bodzy) ->
        let arg, arg_type = infer_term env arg in
        let arg = delay env arg in

        _
    | T_forall (param, body) ->
        let param, param_sort = infer_annot env param in
        let body, body_sort = check_annot env body param_sort in
        let sort = sort in
        let param = coerce param param_sort ~to_:body_sort in
        let body = coerce param body_sort ~to_:body_sort in
        (T_forall (param, body), type_, sort)
    | T_lambda (var, param, body) ->
        let param, param_sort = infer_annot env param in
        let body, body_type, body_sort = check_term env body param_sort in
        let param = coerce param param_sort body_sort in
        _
    | T_apply (funct, arg) ->
        let funct, forall, forall_sort = infer_term env funct in
        let param_type, return_type = split_forall forall in
        let arg = check_term env arg param_type forall_sort in
        let arg = delay env arg in
        let type_ = eval_apply return_type arg in
        (T_apply (funct, arg), type_)
    | T_lift _ | T_quote _ | T_splice _ -> _

  and infer_annot env param = _

  and check_term env term expected expected_sort =
    let term, received, received_sort = infer_term env term in
    let received = coerce received received_sort ~to_:expected_sort in
    equal received expected;
    term

  and check_annot env term sort = _
end

module M = struct
  type term = { struct_ : term_struct; mutable link : term }

  and term_struct =
    | T_type
    | T_var
    | T_let of var * term * term
    | T_forall of term * term
    | T_lambda of var * term
    | T_apply of term * term

  and var = term

  type head =
    | H_type
    | H_var of term
    | H_hole of term
    | H_forall of term * term
    | H_lambda of var * term

  type spine = S_hole | S_apply of term * spine
end

module Core = struct
  type term = { struct_ : term_struct; mutable link : value }

  and term_struct =
    | T_type
    | T_var
    | T_let of var * term * term
    | T_forall of term * term
    | T_lambda of var * term
    | T_apply of term * term

  and value = term
  and var = term

  let v_same : value -> value -> bool = _
  let v_struct : value -> value_struct = _
  let unify_hole : hole:value -> to_:value -> unit = _
  let t_struct : term -> term_struct = _
  let ( let@@ ) = ( @@ )
  let v_with : var -> to_:value -> (unit -> 'a) -> 'a = _

  type head =
    | H_type
    | H_var of term
    | H_hole of term
    | H_forall of term * term
    | H_lambda of var * term

  type spine =
    | S_hole
    | S_let of spine * var * value
    | S_apply of spine * value

  let split_term : term -> head * spine = _

  let rec unify received expected = _

  and unify_struct received expected =
    let received_head, received_spine = split_term received in
    let expected_head, expected_spine = split_term expected in
    match (received_head, expected_head) with
    | H_type, H_type -> _
    | H_var _, H_var _ -> _
    | H_hole _, _ ->
        unify_hole ~hole:received ~hole_spine:received_spine ~to_:expected
    | _, H_hole _ ->
        unify_hole ~hole:expected ~hole_spine:expected_spine ~to_:received
    | H_forall (r_param, r_body), H_forall (e_param, e_body) ->
        unify r_param e_param;
        unify r_body e_body
    | H_lambda (_, _), H_lambda (_, _) -> _
    | _ -> _

  and unify_hole ~hole ~hole_spine ~to_ = _

  let rec plug_spine term spine =
    match (spine : spine) with
    | S_hole -> _
    | S_let (_, _, _) -> _
    | S_apply (funct, arg) -> _

  let rec expand_head term spine =
    match t_struct term with
    | T_type | T_var | T_forall _ | T_lambda _ -> (term, spine)
    | T_let (var, arg, body) ->
        let arg = eval arg in
        let@@ () = v_with var ~to_:arg in
        eval body @@ S_let (spine, var, arg)
    | T_apply (funct, arg) -> eval funct @@ S_apply (spine, arg)

  and eval term =
    let head, spine = expand_head term S_hole in
    _

  let rec eval term =
    match t_struct term with
    | T_type -> _
    | T_var -> _
    | T_let (var, arg, body) ->
        let@@ () = v_with var ~to_:arg in
        _
    | T_forall (_, _) -> _
    | T_lambda (_, _) -> _
    | T_apply (funct, arg) -> (
        let funct = eval funct in
        match t_struct funct with
        | T_lambda (var, body) -> _
        | T_type -> _
        | T_var -> _
        | T_let (_, _, _) -> _
        | T_forall (_, _) -> _
        | T_apply (_, _) -> _)

  let rec unify received expected =
    match v_same received expected with
    | true -> ()
    | false -> unify_struct received expected

  and unify_struct received expected =
    match (v_struct received, v_struct expected) with
    | V_type, V_type -> ()
    | V_hole, _ -> unify_hole ~hole:received ~to_:expected
    | _, V_hole -> unify_hole ~hole:received ~to_:expected
    | V_var, V_var -> ()
    | V_forall (r_param, r_body), V_forall (e_param, e_body) ->
        unify r_param e_param;
        unify r_body e_body
    | V_lambda (r_var, r_body), V_lambda (e_var, e_body) -> _
    | V_apply (r_funct, r_arg), V_apply (e_funct, e_arg) ->
        unify r_funct e_funct;
        unify r_arg e_arg
    | _ -> _

  let v_same : value -> value -> bool = _
  let unify received expected = _

  let rec eval term =
    match term with
    | T_type -> _
    | T_var _ -> _
    | T_let (_, _, _) -> _
    | T_forall (_, _) -> _
    | T_lambda (_, _) -> _
    | T_apply (funct, arg) -> _
    | T_delay content ->
        let content = eval content in
        _
    | T_force thunk -> _

  let e_null : env = _
  let lookup : env -> var -> value = _
  let delay env term = T_thunk { env; term }

  let rec eval env term =
    match term with
    | T_type -> T_type
    | T_var _ -> _
    | T_let (var, arg, body) ->
        let arg = eval env arg in
        let env = E_let { env; var; arg } in
        eval env body
    | T_forall (param, body) ->
        let param = eval env param in
        let body = eval env body in
        T_forall (param, body)
    | T_lambda (var, body) ->
        let body =
          let env = E_def (env, var) in
          eval env body
        in
        T_lambda (var, body)
    | T_apply (funct, arg) -> (
        let funct = eval env funct in
        let arg = eval env arg in
        match funct with
        | T_lambda (var, body) ->
            let env = E_let { env = E_hole; var; arg } in
            eval env body
        | T_type | T_var _
        | T_let (_, _, _)
        | T_forall (_, _)
        | T_apply (_, _)
        | T_thunk _ ->
            T_apply (funct, arg))
    | T_delay term -> T_thunk { env; term }
    | T_force term -> (
        let term = eval env term in
        match term with
        | T_thunk _ -> _
        | T_type -> _
        | T_var _ -> _
        | T_let (_, _, _) -> _
        | T_forall (_, _) -> _
        | T_lambda (_, _) -> _
        | T_apply (_, _) -> _
        | T_delay _ -> _
        | T_force _ -> _)
    | T_thunk ({ env = inner_env; term } as thunk) -> (
        (* TODO: physical identity *)
        match inner_env == e_null with
        | true -> eval env term
        | false ->
            let term = eval inner_env term in
            thunk.env <- e_null;
            thunk.term <- term;
            (* break sharing *)
            eval env term)

  let eval_apply funct arg = _
end

module M = struct
  type var

  type term =
    | T_var of var
    | T_let of var * term * term
    | T_forall of term * term
    | T_lambda of var * term
    | T_apply of term * term
    | T_thunk of { mutable env : env; mutable term : term }

  and env = E_hole | E_let of env * var * term | E_def of env * var

  let rec eval env term =
    match term with
    | T_var _ -> _
    | T_let (_, _, _) -> _
    | T_forall (_, _) -> _
    | T_lambda (_, _) -> _
    | T_apply (funct, arg) -> _
    | T_thunk ({ env = inner_env; term } as shared) ->
        let term = eval inner_env term in
        shared.env <- E_hole;
        shared.term <- term;
        (* break sharing *)
        eval env term

  let split_forall : term -> term * term = _

  let rec infer env term =
    match term with
    | T_var var -> _
    | T_let (var, arg, body) -> _
    | T_forall (param, body) -> _
    | T_lambda (var, body) -> _
    | T_apply (funct, arg) ->
        let funct, forall = infer env funct in
        let param_type, return_type = split_forall forall in
        let arg = check env arg param_type in
        let arg = T_shared { env; term = arg } in
        let type_ = T_apply (return_type, arg) in
        (T_apply (funct, arg), type_)
    | T_shared { term } -> _

  and check env term expected = _
end

module M = struct
  type index = int
  type level = int

  type value = {
    struct_ : value_struct;
    mutable level : level;
    mutable link : value;
  }

  and value_struct =
    | V_var of index
    | V_forall of value * value
    | V_lambda of value
    | V_apply of value * value
    | V_thunk of env * value
    | V_shift of value * int

  type env = E_hole | E_let of env * value | E_def of env

  let v_repr : value -> value = _
  let v_same : value -> value -> bool = _
  let v_struct : value -> value_struct = _

  let rec unify received expected =
    match v_same received expected with
    | true -> ()
    | false -> unify_struct received expected

  and unify_struct received expected =
    match (v_struct received, v_struct expected) with
    | V_var r_var, V_var e_var -> _
    | V_forall (r_param, r_body), V_forall (e_param, e_body) ->
        unify r_param e_param;
        unify r_body e_body
    | V_lambda r_body, V_lambda e_body -> unify r_body e_body
    | V_apply (r_funct, r_arg), V_apply (e_funct, e_arg) ->
        unify r_funct e_funct;
        unify r_arg e_arg
    | _ -> _

  let v_forall : param:value -> body:value -> value = _
  let v_lambda : body:value -> value = _
  let v_apply : funct:value -> arg:value -> value = _

  let rec eval env value =
    match v_struct value with
    | V_var var -> _
    | V_forall (param, body) ->
        let param = eval env param in
        let body = eval env body in
        v_forall ~param ~body
    | V_lambda (var, body) ->
        let body =
          let env = E_def (env, var) in
          eval env body
        in
        v_lambda ~var ~body
    | V_apply (funct, arg) -> (
        let funct = eval env funct in
        let arg = eval env arg in
        match v_struct funct with
        | V_lambda (var, body) ->
            let env = E_let (env, var, arg) in
            eval env body
        | V_var _ | V_forall (_, _) | V_apply (_, _) -> v_apply ~funct ~arg)

  let rec v_subst var ~to_ body =
    let body = v_repr body in
    match body.level >= var.level with true -> _ | false -> body

  and v_subst_struct var ~to_ body =
    match body.struct_ with
    | V_var _ -> _
    | V_forall (param, body) -> (
        let param' = v_subst var ~to_ param in
        let body' = v_subst var ~to_ body in
        match v_same param param' && v_same body body' with
        | true -> body
        | false -> _)
    | V_lambda (_, _) -> _
    | V_apply (_, _) -> _
end

module M = struct
  type var

  type term =
    | T_annot of term * term
    | T_hole of hole
    | T_type
    | T_var of var
    | T_forall of term * term
    | T_lambda of var * term
    | T_apply of term * term

  and hole

  let rec infer term = _

  and check term expected =
    match term with
    | T_annot (term, expected) -> _
    | T_hole _ -> _
    | T_type -> _
    | T_var var -> _
    | T_forall (param, body) ->
        check param T_type;
        _
    | T_lambda (var, body) -> _
    | T_apply (funct, arg) ->
        let x = check funct in
        infer env funct;
        infer env arg
end

module M = struct
  type var

  type term =
    | T_var of var
    | T_lambda of var * term
    | T_apply of term * term
    | T_shared of { mutable term : term }
    | T_with of env * term

  and env = E_hole | E_let of env * var * term

  let delay env term =
    let term = T_with (env, term) in
    T_shared { term }

  let rec normalize env term =
    match term with
    | T_var var -> _
    | T_lambda (var, body) ->
        let body = normalize env body in
        T_lambda (var, body)
    | T_apply (funct, arg) -> (
        let funct = normalize env funct in
        let arg = delay env arg in
        match funct with
        | T_lambda (var, body) ->
            let env = E_let (E_hole, var, arg) in
            normalize env body
        | T_var _ | T_apply _ | T_shared _ | T_with _ -> T_apply (funct, arg))
    | T_shared ({ term } as shared) ->
        let term = normalize E_hole term in
        shared.term <- term;
        (* break sharing *)
        normalize env term
    | T_with (E_hole, body) -> normalize env body
    | T_with (E_let (inner_env, var, arg), body) ->
        let env = E_let (inner_env, var, arg) in
        normalize env body
end

module M = struct
  type var

  type term =
    | T_hole of hole
    | T_var of var
    | T_forall of term * term
    | T_lambda of var * term
    | T_apply of term * term

  and hole

  let subst : var -> to_:term -> term -> term = _

  let rec normalize term =
    match term with
    | T_hole hole -> T_hole hole
    | T_var var -> T_var var
    | T_forall (param, body) ->
        let param = normalize param in
        let body = normalize body in
        T_forall (param, body)
    | T_lambda (var, body) ->
        let body = normalize body in
        T_lambda (var, body)
    | T_apply (funct, arg) -> (
        let funct = normalize funct in
        let arg = normalize arg in
        match funct with
        | T_lambda (var, body) -> normalize @@ subst var ~to_:arg body
        | T_hole _ | T_var _ | T_forall _ | T_apply _ -> T_apply (funct, arg))

  let unify received expected = _
end

module Staging = struct
  type term =
    | T_type
    | T_forall of term * term
    | T_lambda of term
    | T_apply of term * term
    | T_lift of term
    | T_splice of term
    | T_quote of term
end

type var = int

type term =
  | T_type
  | T_var of var
  | T_forall of term * term
  | T_lambda of term
  | T_apply of term * term
  | T_exists of term * term
  | T_pair of term * term
  | T_fst of term
  | T_snd of term

type env = E_hole | E_let of env * var * term
type value

let v_type : value = _
let v_arrow : param:value -> body:value -> value = _
let v_split_forall : value -> value * value = _
let v_split_exists : value -> value * value = _
let eval_delay : env -> term -> value = _
let eval_apply : value -> value -> value = _

let rec infer env term =
  match term with
  | T_type -> v_type
  | T_var _ -> _
  | T_forall (param, body) ->
      check env param v_type;
      let body_type =
        let param = eval_delay env param in
        v_arrow ~param ~body:v_type
      in
      check env body body_type;
      v_type
  | T_lambda _ -> _
  | T_apply (funct, arg) ->
      let forall = infer env funct in
      let param_type, return_type = v_split_forall forall in
      check env arg param_type;
      let arg = eval_delay env arg in
      eval_apply return_type arg
  | T_exists (fst, snd) ->
      check env fst v_type;
      let fst = eval_delay env fst in
      let snd_type = v_arrow ~param:fst ~body:v_type in
      check env snd snd_type;
      v_type
  | T_pair _ -> _
  | T_fst pair ->
      let exists = infer env pair in
      let fst_type, _snd_type = v_split_exists exists in
      fst_type
  | T_snd pair ->
      let exists = infer env pair in
      let _fst_type, snd_type = v_split_exists exists in
      let fst = eval_delay env (T_fst pair) in
      eval_apply snd_type fst

and check env term expected =
  match term with
  | T_lambda body ->
      let param_type, return_type = v_split_forall expected in
      let env = _ in
      check env body return_type
  | T_pair (fst, snd) ->
      let fst_type, snd_type = v_split_exists expected in
      check env fst fst_type;
      let fst = eval_delay env fst in
      let snd_type = eval_apply snd_type fst in
      check env snd snd_type
  | T_type | T_var _ | T_forall _ | T_apply _ | T_exists _ | T_fst _ | T_snd _
    ->
      let received = infer env term in
      equal received expected

(* compute *)
type index
type level

(* TODO: unboxed option *)
type value = {
  struct_ : value_struct;
  mutable link : value;
  mutable level : level;
}

and value_struct =
  | V_hole
  | V_var
  | V_forall of value * value
  | V_lambda of term
  | V_apply of value * value

and term =
  | T_var of index
  | T_forall of term * term
  | T_lambda of term
  | T_apply of term * term
  | T_reify of value

let v_repr : value -> value = _
let v_same : value -> value -> bool = _
let v_struct : value -> value_struct = _
let v_link : value -> to_:value -> unit = _
let v_level : value -> level = _
let v_skolem : level -> value = _
let eval : env -> term -> value = _

let rec unify received expected =
  let received = v_repr received in
  let expected = v_repr expected in
  match v_same received expected with
  | true -> ()
  | false ->
      v_link received ~to_:expected;
      unify_struct received expected

and unify_struct received expected =
  match (v_struct received, v_struct expected) with
  | V_hole, _ -> _
  | _, V_hole -> _
  | V_forall (r_param, r_body), V_forall (e_param, e_body) ->
      unify r_param e_param;
      unify r_body e_body
  | V_lambda r_body, V_lambda e_body -> _
  | V_apply (r_funct, r_arg), V_apply (e_funct, e_arg) ->
      unify r_funct e_funct;
      unify r_arg e_arg
  | _, _ -> _

and unify_under received expected =
  let env = _ in
  let received = eval env received in
  let expected = eval env expected in
  unify received expected

type index
type level

type term =
  | T_var of index
  | T_forall of term * term
  | T_lambda of term
  | T_apply of term * term
  | T_thunk of value
  | T_force of term

and value =
  | V_var of level
  | V_forall of value * value
  | V_lambda of term
  | V_apply of value * value
  | V_thunk of env * value
  | V_force of value

and env = E_hole | E_let of env * value

type spine = S_apply of spine * env * term | S_force of spine

let rec eval env term =
  match term with
  | T_var _ -> _
  | T_forall (_, _) -> _
  | T_lambda _ -> _
  | T_apply (funct, arg) -> _
  | T_thunk value -> V_thunk (env, value)
  | T_force thunk -> (
      let thunk = eval env thunk in
      match thunk with
      | V_thunk (env, body) -> eval env @@ reify body
      | V_var _ | V_forall _ | V_lambda _ | V_apply _ | V_force _ -> _)

and reify value =
  match value with
  | V_var _ -> _
  | V_forall (_, _) -> _
  | V_lambda _ -> _
  | V_apply (_, _) -> _
  | V_thunk _ -> T_thunk (V_force value)
  | V_force _ -> _

let rec eval_down env head spine =
  match head with
  | T_var _ -> _
  | T_forall (var, body) -> _
  | T_lambda body -> _
  | T_apply (funct, arg) -> _
  | T_thunk body -> V_thunk (env, body)
  | T_force thunk -> _

and reify value =
  match value with
  | V_var _ -> _
  | V_forall (param, body) ->
      let param = reify param in
      let body = reify body in
      T_forall (param, body)
  | V_lambda body -> T_lambda body
  | V_apply (funct, arg) ->
      let funct = reify funct in
      let arg = reify arg in
      T_apply (funct, arg)
  | V_thunk _ -> T_delay (T_force (T_reify value))
  | V_force thunk ->
      let thunk = reify thunk in
      T_force thunk

let rec eval env term =
  match term with
  | T_var index -> _
  | T_forall (param, body) ->
      let param = eval env param in
      let body = eval env body in
      V_forall (param, body)
  | T_lambda body ->
      let body = eval env body in
      let body = reify body in
      V_lambda body
  | T_apply (funct, arg) -> (
      let funct = eval env funct in
      let arg = eval env arg in
      match funct with
      | V_lambda body ->
          let env = E_let (E_hole, arg) in
          eval env body
      | V_var _ | V_forall _ | V_apply _ | V_thunk _ | V_force _ ->
          (* TODO: crash on V_forall *)
          V_apply (funct, arg))
  | T_delay body -> V_thunk (env, body)
  | T_force thunk -> (
      let thunk = eval env thunk in
      match thunk with
      | V_thunk (env, body) ->
          (* TODO: commit thunk *)
          _
      | V_var _ | V_forall _ | V_lambda _ | V_apply _ | V_force _ ->
          V_force thunk)
  | T_reify value ->
      (* TODO: improve this *)
      let term = reify value in
      eval env term

and reify value =
  match value with
  | V_var level -> T_var var
  | V_forall (param, body) ->
      let param = reify param in
      let body = reify body in
      T_forall (param, body)
  | V_lambda body -> T_lambda body
  | V_apply (funct, arg) ->
      let funct = reify funct in
      let arg = reify arg in
      T_apply (funct, arg)
  | V_thunk _ -> T_delay (T_reify value)
  | V_force body ->
      let body = reify body in
      T_force body

type value = { struct_ : value_struct; mutable link : value }

and value_struct =
  | V_var of var
  | V_forall of value * value
  | V_lambda of term
  | V_apply of value * value
  | V_thunk of env * term

and env

let equal received expected =
  match (received, expected) with
  | V_var _, V_var _ -> _
  | V_forall (_, _), _ -> _
  | V_lambda received, V_lambda expected -> _
  | V_apply (_, _), _ | V_thunk (_, _), _ | V_force _, _ -> _
  | _, _ -> _

type var = int

type term =
  | T_var of var
  | T_forall of var * term * term
  | T_lambda of var * term
  | T_apply of term * term
  | T_delay of term
  | T_force of term

and value =
  | V_var of var
  | V_forall of var * value * value
  | V_lambda of var * term
  | V_apply of value * value
  | V_thunk of env * term
  | V_force of value

and env = E_hole | E_let of env * var * value
