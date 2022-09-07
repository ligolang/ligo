module Free_variables = Helpers.Free_variables

open Ligo_prim
open Ast_aggregated

(* Reference implementation:
   https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/interp/lambda-subst/main.ml

   ...but, it has at least one bug: in subst,
   `let new_body = replace e' y fresh in ...` should be:
   `let new_body = replace e' fresh y in ...`,
   due to the arg order choice for replace.

   Below, this bug is fixed by adopting the other order choice for
   replace (as well as subst).  *)

let replace_var : Value_var.t -> Value_var.t -> Value_var.t -> Value_var.t =
  fun v x y ->
  if Value_var.equal v x
  then y
  else v

(* replace in `e` the variable `x` with `y`.

   But only replace the _free_ x.
*)
let rec replace : expression -> Value_var.t -> Value_var.t -> expression =
  fun e x y ->
  let replace e = replace e x y in
  let return expression_content = { e with expression_content } in
  let replace_var v = replace_var v x y in
  let (=) = Value_var.equal in
  match e.expression_content with
  | E_variable z ->
    let z = replace_var z in
    return @@ E_variable z
  | E_lambda { binder = { var ; ascr ; attributes } ; output_type ; result } ->
     let result = if var = x then result else replace result in
     return @@ E_lambda { binder = { var ; ascr ; attributes } ; output_type ; result }
  | E_recursive { fun_name ; fun_type ; lambda = { binder = { var ; ascr ; attributes } ; output_type ; result } } ->
     let result = if var = x || fun_name = x then result else replace result in
     return @@ E_recursive { fun_name ; fun_type ; lambda = { binder = { var ; ascr ; attributes } ; output_type ; result } }
  | E_let_in { let_binder = { var ; ascr ; attributes } ; rhs ; let_result ; attr } ->
     let rhs = replace rhs in
     let let_result = if var = x then let_result else replace let_result in
     return @@ E_let_in { let_binder = { var ; ascr ; attributes } ; rhs ; let_result ; attr }
  | E_constant {cons_name; arguments} ->
     let arguments = List.map ~f:replace arguments in
     return @@ E_constant {cons_name; arguments}
  | E_application { lamb ; args } ->
     let lamb, args = Simple_utils.Tuple.map2 replace (lamb, args) in
     return @@ E_application { lamb ; args }
  | E_type_abstraction { type_binder ; result } ->
     let result = replace result in
     return @@ E_type_abstraction { type_binder ; result }
  | E_type_inst { forall ; type_ } ->
     let forall = replace forall in
     return @@ E_type_inst { forall ; type_ }
  | E_constructor { constructor ; element } ->
     let element = replace element in
     return @@ E_constructor { constructor ; element }
  | E_matching { matchee ; cases = Match_variant { cases ; tv } } ->
     let matchee = replace matchee in
     let f ({ constructor ; pattern ; body } : _ matching_content_case) =
       let body = if pattern = x then body else replace body in
       { constructor ; pattern ; body } in
     let cases = List.map ~f cases in
     return @@ E_matching { matchee ; cases = Match_variant { cases ; tv } }
  | E_matching { matchee ; cases = Match_record { fields ; body ; tv } } ->
     let matchee = replace matchee in
     let binders = List.map (Record.LMap.to_kv_list fields) ~f:(fun (_, { var ; _ }) -> replace_var var) in
     let body = if List.mem ~equal:(=) binders x then body else replace body in
     return @@ E_matching { matchee ; cases = Match_record { fields ; body ; tv } }
  | E_literal _ -> e
  | E_raw_code _ -> e
  | E_record m ->
     let m = Record.map (fun x -> replace x) m in
     return @@ E_record m
  | E_accessor { struct_ ; path } ->
     let struct_ = replace struct_ in
     return @@ E_accessor { struct_ ; path }
  | E_update { struct_ ; path ; update } ->
     let struct_ = replace struct_ in
     let update = replace update in
     return @@ E_update { struct_ ; path ; update }
  | E_assign { binder = { var ; ascr ; attributes } ; expression } ->
     let expression = replace expression in
     return @@ E_assign { binder = { var ; ascr ; attributes } ; expression }

(* Given an implementation of substitution on an arbitary type of
   body, implements substitution on a binder (pair of bound variable
   and body) *)
let subst_binder : type body.
  (body:body -> x:Value_var.t -> expr:expression -> body) ->
  (body -> Value_var.t -> Value_var.t -> body) ->
  body:(Value_var.t * body) -> x:Value_var.t -> expr:expression -> (Value_var.t * body) =
  fun subst replace ~body:(y, body) ~x ~expr ->
    (* if x is shadowed, binder doesn't change *)
    if Value_var.equal x y
    then (y, body)
    (* else, if no capture, subst in binder *)
    else if not (List.mem ~equal:Value_var.equal (Free_variables.expression expr) y)
    then (y, subst ~body ~x ~expr)
    (* else, avoid capture and subst in binder *)
    else
      let fresh = Value_var.fresh_like y in
      let body = replace body y fresh in
      (fresh, subst ~body ~x ~expr)

(* Given an implementation of substitution on an arbitary type of
   body, implements substitution on a binder (pair of bound variable
   and body) *)
let subst_binders : type body.
  (body:body -> x:Value_var.t -> expr:expression -> body) ->
  (body -> Value_var.t -> Value_var.t -> body) ->
  body:(Value_var.t list * body) -> x:Value_var.t -> expr:expression -> (Value_var.t list * body) =
  fun subst replace ~body:(ys, body) ~x ~expr ->
    (* if x is shadowed, binder doesn't change *)
    if List.mem ~equal:Value_var.equal ys x
    then (ys, body)
    (* else, if no capture, subst in binder *)
    else
      let fvs = Free_variables.expression expr in
      let f (fs, body) y =
        if not (List.mem ~equal:Value_var.equal fvs y)
        then (y :: fs, body)
         (* else, avoid capture and subst in binder *)
        else
          let fresh = Value_var.fresh_like y in
          let body = replace body y fresh in
          (fresh :: fs, body) in
      let ys, body = List.fold ~f ~init:([], body) ys in
      (List.rev ys, subst ~body ~x ~expr)


(**
   Computes `body[x := expr]`.
**)
let rec subst_expression : body:expression -> x:Value_var.t -> expr:expression -> expression =
  fun ~body ~x ~expr ->
  let self body = subst_expression ~body ~x ~expr in
  let return_id = body in
  let return expression_content = {body with expression_content} in
  let subst_binder1 =
    subst_binder subst_expression replace in
  let subst_binder2 =
    subst_binder
      subst_binder1
      (fun (x, body) y z -> (replace_var x y z, replace body y z)) in
  match body.expression_content with
  | E_variable x' ->
     if Value_var.equal x' x
     then expr
     else return_id
  | E_lambda { binder = { var ; ascr ; attributes } ; output_type ; result } ->
     let var, result = subst_binder1 ~body:(var, result) ~x ~expr in
     return @@ E_lambda { binder = { var ; ascr ; attributes } ; output_type ; result }
  | E_recursive { fun_name ; fun_type ; lambda = { binder = { var ; ascr ; attributes } ; output_type ; result } } ->
     let fun_name, (var, result) = subst_binder2 ~body:(fun_name, (var, result)) ~x ~expr in
     return @@ E_recursive { fun_name ; fun_type ; lambda = { binder = { var ; ascr ; attributes } ; output_type ; result } }
  | E_let_in { let_binder = { var ; ascr ; attributes } ; rhs ; let_result ; attr } ->
     let rhs = self rhs in
     let var, let_result = subst_binder1 ~body:(var, let_result) ~x ~expr in
     return @@ E_let_in { let_binder = { var ; ascr ; attributes } ; rhs ; let_result ; attr }
  | E_constant {cons_name; arguments} ->
     let arguments = List.map ~f:self arguments in
     return @@ E_constant {cons_name; arguments}
  | E_application { lamb ; args } ->
     let lamb, args = Simple_utils.Tuple.map2 self (lamb, args) in
     return @@ E_application { lamb ; args }
  | E_type_abstraction { type_binder ; result } ->
     let result = self result in
     return @@ E_type_abstraction { type_binder ; result }
  | E_type_inst { forall ; type_ } ->
     let forall = self forall in
     return @@ E_type_inst { forall ; type_ }
  | E_constructor { constructor ; element } ->
     let element = self element in
     return @@ E_constructor { constructor ; element }
  | E_matching { matchee ; cases = Match_variant { cases ; tv } } ->
     let matchee = self matchee in
     let f ({ constructor ; pattern ; body } : _ matching_content_case) cs =
       let pattern, body = subst_binder1 ~body:(pattern, body) ~x ~expr in
       { constructor ; pattern ; body } :: cs in
     let cases = List.fold_right cases ~f ~init:[] in
     return @@ E_matching { matchee ; cases = Match_variant { cases ; tv } }
  | E_matching { matchee ; cases = Match_record { fields ; body ; tv } } ->
     let matchee = self matchee in
     let fields = Record.LMap.to_kv_list fields in
     let binders = List.map fields ~f:(fun (_, { var ; _ }) -> var) in
     let binders, body = subst_binders subst_expression replace ~body:(binders, body) ~x ~expr in
     let fields = List.zip_exn fields binders in
     let fields = List.map fields ~f:(fun ((l, { ascr ; attributes ; _ }), var) -> (l, Binder.{ var ; ascr ; attributes })) in
     let fields = Record.LMap.of_list fields in
     return @@ E_matching { matchee ; cases = Match_record { fields ; body ; tv } }
  | E_literal _ | E_raw_code _ ->
     return_id
  | E_record m ->
     let m = Record.map self m in
     return @@ E_record m
  | E_accessor { struct_ ; path } ->
     let struct_ = self struct_ in
     return @@ E_accessor { struct_ ; path }
  | E_update { struct_ ; path ; update } ->
     let struct_ = self struct_ in
     let update = self update in
     return @@ E_update { struct_ ; path ; update }
  | E_assign { binder = { var ; ascr ; attributes } ; expression } ->
     let expression = self expression in
     return @@ E_assign { binder = { var ; ascr ; attributes } ; expression }
