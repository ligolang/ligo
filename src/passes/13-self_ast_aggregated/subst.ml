open Ligo_prim
open Ast_aggregated
module Free_variables = Helpers.Free_variables

(* Reference implementation:
   https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/interp/lambda-subst/main.ml

   ...but, it has at least one bug: in subst,
   `let new_body = replace e' y fresh in ...` should be:
   `let new_body = replace e' fresh y in ...`,
   due to the arg order choice for replace.

   Below, this bug is fixed by adopting the other order choice for
   replace (as well as subst).  *)

let replace_var : Value_var.t -> Value_var.t -> Value_var.t -> Value_var.t =
 fun v x y -> if Value_var.equal v x then y else v


(* replace in `e` the variable `x` with `y`.

   But only replace the _free_ x.
*)
let rec replace : expression -> Value_var.t -> Value_var.t -> expression =
 fun e x y ->
  let replace e = replace e x y in
  let return expression_content = { e with expression_content } in
  let replace_var v = replace_var v x y in
  let ( = ) = Value_var.equal in
  match e.expression_content with
  | E_variable z ->
    let z = replace_var z in
    return @@ E_variable z
  | E_lambda { binder; output_type; result } ->
    let result =
      if Param.is_imm binder && Param.get_var binder = x then result else replace result
    in
    return @@ E_lambda { binder; output_type; result }
  | E_recursive
      { fun_name; fun_type; lambda = { binder; output_type; result }; force_lambdarec } ->
    let result =
      if (Param.is_imm binder && Param.get_var binder = x) || fun_name = x
      then result
      else replace result
    in
    return
    @@ E_recursive
         { fun_name; fun_type; lambda = { binder; output_type; result }; force_lambdarec }
  | E_let_in { let_binder; rhs; let_result; attributes } ->
    let rhs = replace rhs in
    let let_result =
      if List.exists (Pattern.binders let_binder) ~f:(Binder.apply (( = ) x))
      then let_result
      else replace let_result
    in
    return @@ E_let_in { let_binder; rhs; let_result; attributes }
  | E_constant { cons_name; arguments } ->
    let arguments = List.map ~f:replace arguments in
    return @@ E_constant { cons_name; arguments }
  | E_application { lamb; args } ->
    let lamb, args = Simple_utils.Tuple.map2 replace (lamb, args) in
    return @@ E_application { lamb; args }
  | E_type_abstraction { type_binder; result } ->
    let result = replace result in
    return @@ E_type_abstraction { type_binder; result }
  | E_type_inst { forall; type_ } ->
    let forall = replace forall in
    return @@ E_type_inst { forall; type_ }
  | E_constructor { constructor; element } ->
    let element = replace element in
    return @@ E_constructor { constructor; element }
  | E_matching { matchee; disc_label; cases } ->
    let matchee = replace matchee in
    let cases =
      List.map cases ~f:(fun { pattern; body } ->
          let body =
            if List.exists (Pattern.binders pattern) ~f:(Binder.apply (( = ) x))
            then body
            else replace body
          in
          ({ pattern; body } : _ Types.Match_expr.match_case))
    in
    return @@ E_matching { matchee; disc_label; cases }
  | E_literal _ -> e
  | E_raw_code { language; code } ->
    let code = replace code in
    return @@ E_raw_code { language; code }
  | E_record m ->
    let m = Record.map ~f:replace m in
    return @@ E_record m
  | E_accessor { struct_; path } ->
    let struct_ = replace struct_ in
    return @@ E_accessor { struct_; path }
  | E_update { struct_; path; update } ->
    let struct_ = replace struct_ in
    let update = replace update in
    return @@ E_update { struct_; path; update }
  | E_assign { binder; expression } ->
    let expression = replace expression in
    return @@ E_assign { binder; expression }
  | E_coerce { anno_expr; type_annotation } ->
    let anno_expr = replace anno_expr in
    return @@ E_coerce { anno_expr; type_annotation }
  | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    let rhs = replace rhs in
    let let_result = replace let_result in
    return @@ E_let_mut_in { let_binder; rhs; let_result; attributes }
  | E_deref _ -> e
  | E_for { binder; start; final; incr; f_body } ->
    let start = replace start in
    let final = replace final in
    let incr = replace incr in
    let f_body = if binder = x then f_body else replace f_body in
    return @@ E_for { binder; start; final; incr; f_body }
  | E_while while_loop ->
    let while_loop = While_loop.map replace while_loop in
    return @@ E_while while_loop
  | E_for_each
      { fe_binder = (binder1, binder2) as fe_binder
      ; collection
      ; collection_type
      ; fe_body
      } ->
    let collection = replace collection in
    let binders = binder1 :: Option.to_list binder2 in
    let fe_body = if List.mem binders x ~equal:( = ) then fe_body else replace fe_body in
    return @@ E_for_each { fe_binder; collection; collection_type; fe_body }


and replace_lambda
    Lambda.{ binder; output_type; result }
    (x : Value_var.t)
    (y : Value_var.t)
  =
  let result =
    if Param.is_imm binder && Value_var.equal (Param.get_var binder) x
    then result
    else replace result x y
  in
  Lambda.{ binder; output_type; result }


(* Given an implementation of substitution on an arbitary type of
   body, implements substitution on a binder (pair of bound variable
   and body) *)
let subst_binder
    : type body.
      (body:body -> x:Value_var.t -> expr:expression -> body)
      -> (body -> Value_var.t -> Value_var.t -> body)
      -> body:Value_var.t * body
      -> x:Value_var.t
      -> expr:expression
      -> Value_var.t * body
  =
 fun subst replace ~body:(y, body) ~x ~expr ->
  (* if x is shadowed, binder doesn't change *)
  if Value_var.equal x y
  then y, body (* else, if no capture, subst in binder *)
  else if not (List.mem ~equal:Value_var.equal (Free_variables.expression expr) y)
  then y, subst ~body ~x ~expr (* else, avoid capture and subst in binder *)
  else (
    let fresh = Value_var.fresh_like y in
    let body = replace body y fresh in
    fresh, subst ~body ~x ~expr)


(**
   Computes `body[x := expr]`.
**)
let rec subst_expression
    : body:expression -> x:Value_var.t -> expr:expression -> expression
  =
 fun ~body ~x ~expr ->
  let self body = subst_expression ~body ~x ~expr in
  let return_id = body in
  let return expression_content = { body with expression_content } in
  let subst_binder1 = subst_binder subst_expression replace in
  let subst_binder2 =
    subst_binder subst_binder1 (fun (x, body) y z -> replace_var x y z, replace body y z)
  in
  let subst_lambda ~body:Lambda.{ binder; output_type; result } ~x ~expr =
    let var, result =
      match Param.get_mut_flag binder with
      | Immutable -> subst_binder1 ~body:(Param.get_var binder, result) ~x ~expr
      | Mutable -> Param.get_var binder, self result
    in
    let binder = Param.set_var binder var in
    Lambda.{ binder; output_type; result }
  in
  let subst_pattern (pattern, body) ~x ~expr =
    let ys = List.map (Pattern.binders pattern) ~f:Binder.get_var in
    (* if x is shadowed, binder doesn't change *)
    if List.mem ~equal:Value_var.equal ys x
    then pattern, body (* else, if no capture, subst in binder *)
    else (
      let fvs = Free_variables.expression expr in
      let f body y =
        if not (List.mem ~equal:Value_var.equal fvs y)
        then y, body (* else, avoid capture and subst in binder *)
        else (
          let fresh = Value_var.fresh_like y in
          let body = replace body y fresh in
          fresh, body)
      in
      let body, pattern =
        Pattern.fold_map_pattern
          (fun body pattern ->
            match pattern.wrap_content with
            | P_var binder ->
              let y = Binder.get_var binder in
              let y, body = f body y in
              let binder = Binder.set_var binder y in
              body, { pattern with wrap_content = P_var binder }
            | _ -> body, pattern)
          body
          pattern
      in
      (* let ys, body = List.fold ~f ~init:([], body) ys in *)
      pattern, subst_expression ~body ~x ~expr)
  in
  match body.expression_content with
  | E_variable x' -> if Value_var.equal x' x then expr else return_id
  | E_lambda lambda ->
    let lambda = subst_lambda ~x ~expr ~body:lambda in
    return @@ E_lambda lambda
  | E_recursive { fun_name; fun_type; lambda; force_lambdarec } ->
    let fun_name, lambda =
      subst_binder subst_lambda replace_lambda ~body:(fun_name, lambda) ~x ~expr
    in
    return @@ E_recursive { fun_name; fun_type; lambda; force_lambdarec }
  | E_let_in { let_binder; rhs; let_result; attributes } ->
    let rhs = self rhs in
    let let_binder, let_result = subst_pattern (let_binder, let_result) ~x ~expr in
    return @@ E_let_in { let_binder; rhs; let_result; attributes }
  | E_constant { cons_name; arguments } ->
    let arguments = List.map ~f:self arguments in
    return @@ E_constant { cons_name; arguments }
  | E_application { lamb; args } ->
    let lamb, args = Simple_utils.Tuple.map2 self (lamb, args) in
    return @@ E_application { lamb; args }
  | E_type_abstraction { type_binder; result } ->
    let result = self result in
    return @@ E_type_abstraction { type_binder; result }
  | E_type_inst { forall; type_ } ->
    let forall = self forall in
    return @@ E_type_inst { forall; type_ }
  | E_constructor { constructor; element } ->
    let element = self element in
    return @@ E_constructor { constructor; element }
  | E_matching { matchee; disc_label; cases } ->
    let matchee = self matchee in
    let cases =
      List.map cases ~f:(fun { pattern; body } ->
          let pattern, body = subst_pattern (pattern, body) ~x ~expr in
          ({ pattern; body } : _ Match_expr.match_case))
    in
    return @@ E_matching { matchee; disc_label; cases }
  | E_literal _ -> return_id
  | E_raw_code { language; code } ->
    let code = self code in
    return @@ E_raw_code { language; code }
  | E_record m ->
    let m = Record.map ~f:self m in
    return @@ E_record m
  | E_accessor { struct_; path } ->
    let struct_ = self struct_ in
    return @@ E_accessor { struct_; path }
  | E_update { struct_; path; update } ->
    let struct_ = self struct_ in
    let update = self update in
    return @@ E_update { struct_; path; update }
  | E_assign { binder; expression } ->
    let expression = self expression in
    return @@ E_assign { binder; expression }
  | E_coerce { anno_expr; type_annotation } ->
    let anno_expr = self anno_expr in
    return @@ E_coerce { anno_expr; type_annotation }
  | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    (* in case of mutable variable, we don't substitute *)
    let rhs = self rhs in
    let let_result = self let_result in
    return @@ E_let_mut_in { let_binder; rhs; let_result; attributes }
  | E_deref _ -> return_id
  | E_for { binder; start; final; incr; f_body } ->
    let start = self start in
    let final = self final in
    let incr = self incr in
    let binder, f_body = subst_binder1 ~body:(binder, f_body) ~x ~expr in
    return @@ E_for { binder; start; final; incr; f_body }
  | E_while while_loop ->
    let while_loop = While_loop.map self while_loop in
    return @@ E_while while_loop
  | E_for_each { fe_binder = binder, None; collection; collection_type; fe_body } ->
    let collection = self collection in
    let binder, fe_body = subst_binder1 ~body:(binder, fe_body) ~x ~expr in
    return
    @@ E_for_each { fe_binder = binder, None; collection; collection_type; fe_body }
  | E_for_each { fe_binder = binder1, Some binder2; collection; collection_type; fe_body }
    ->
    let collection = self collection in
    let binder1, (binder2, fe_body) =
      subst_binder2 ~body:(binder1, (binder2, fe_body)) ~x ~expr
    in
    return
    @@ E_for_each
         { fe_binder = binder1, Some binder2; collection; collection_type; fe_body }
