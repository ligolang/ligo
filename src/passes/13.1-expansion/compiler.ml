module Location = Simple_utils.Location
module Ligo_pair = Simple_utils.Ligo_pair
module Trace = Simple_utils.Trace
module I = Ast_aggregated
module O = Ast_expanded
open Errors
open Ligo_prim

(*

Compile patterns (let destructuring, mutable let destructuring and pattern matching) into case expressions.
Mutable let destructuring (E_let_mut_in) is handled in a slightly different way (see destruct_mut_let_in)

*)

let rec compile_expression ~(raise : _ Trace.raise) : I.expression -> O.expression =
 fun exp ->
  let self = compile_expression ~raise in
  let compile_type = compile_type ~raise in
  let compile_matching = compile_matching ~raise in
  let return : O.expression_content -> O.expression =
   fun expression_content ->
    { expression_content
    ; type_expression = compile_type exp.type_expression
    ; location = exp.location
    }
  in
  match exp.expression_content with
  | E_matching { matchee; cases } ->
    let matchee = self matchee in
    compile_matching ~loc:exp.location ~mut:false matchee cases
  | E_let_in
      { let_binder = { wrap_content = P_var let_binder; _ }; rhs; let_result; attributes }
    ->
    (* if lhs is a simple pattern, we do not bother executing Pattern_matching *)
    let let_binder = Binder.map compile_type let_binder in
    let rhs = self rhs in
    let let_result = self let_result in
    return (O.E_let_in { let_binder; rhs; let_result; attributes })
  | E_let_in { let_binder; rhs; let_result; attributes } ->
    let matchee = self rhs in
    compile_matching
      ~loc:exp.location
      ~attributes
      ~mut:false
      matchee
      [ { pattern = let_binder; body = let_result } ]
  | E_let_mut_in
      { let_binder = { wrap_content = P_var let_binder; _ }; rhs; let_result; attributes }
    ->
    (* if lhs is a simple pattern, we do not bother executing Pattern_matching *)
    let let_binder = Binder.map compile_type let_binder in
    let rhs = self rhs in
    let let_result = self let_result in
    return (O.E_let_mut_in { let_binder; rhs; let_result; attributes })
  | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    let matchee = self rhs in
    compile_matching
      ~loc:exp.location
      ~attributes
      ~mut:true
      matchee
      [ { pattern = let_binder; body = let_result } ]
  (* Bellow is no-op *)
  | E_record m ->
    let m = Record.map ~f:self m in
    return (O.E_record m)
  | E_accessor acc ->
    let acc = I.Accessor.map self acc in
    return (O.E_accessor acc)
  | E_update u ->
    let u = I.Update.map self u in
    return (O.E_update u)
  | E_constructor c ->
    let e' = self c.element in
    return (O.E_constructor { c with element = e' })
  | E_application { lamb; args } ->
    let ab = lamb, args in
    let a, b = Ligo_pair.map ~f:self ab in
    return (O.E_application { lamb = a; args = b })
  | E_type_inst { forall; type_ } ->
    let forall = self forall in
    let type_ = compile_type type_ in
    return (O.E_type_inst { forall; type_ })
  | E_lambda l ->
    let l = Lambda.map self compile_type l in
    return (O.E_lambda l)
  | E_type_abstraction ta ->
    let ta = Type_abs.map self ta in
    return (O.E_type_abstraction ta)
  | E_recursive r ->
    let r = Recursive.map self compile_type r in
    return (O.E_recursive r)
  | E_constant c ->
    let c = Constant.map self c in
    return (O.E_constant c)
  | E_raw_code { language; code } ->
    let code = self code in
    return (O.E_raw_code { language; code })
  | E_assign a ->
    let a = Assign.map self compile_type a in
    return (O.E_assign a)
  | E_for f ->
    let f = For_loop.map self f in
    return (O.E_for f)
  | E_for_each fe ->
    let fe = For_each_loop.map self fe in
    return (O.E_for_each fe)
  | E_while w ->
    let w = While_loop.map self w in
    return (O.E_while w)
  | E_deref x -> return (O.E_deref x)
  | E_literal x -> return (O.E_literal x)
  | E_variable x -> return (O.E_variable x)
  | E_coerce { anno_expr; _ } -> self anno_expr


and compile_type ~(raise : _ Trace.raise) : I.type_expression -> O.type_expression =
 fun ty ->
  let self = compile_type ~raise in
  let return type_content : O.type_expression =
    { type_content
    ; orig_var = Option.map ty.abbrev ~f:(fun { orig_var = _, v; _ } -> v)
    ; location = ty.location
    ; source_type = Some ty
    }
  in
  match ty.type_content with
  | T_variable x -> return (T_variable x)
  | T_exists _ -> raise.error @@ cannot_compile_texists ty ty.location
  | T_constant { language; injection; parameters } ->
    return (T_constant { language; injection; parameters = List.map parameters ~f:self })
  | T_sum (r, _) -> return (T_sum (I.Row.map self r))
  | T_record r -> return (T_record (I.Row.map self r))
  | T_arrow x -> return (T_arrow (Arrow.map self x))
  | T_singleton x -> return (T_singleton x)
  | T_abstraction x -> return (T_for_all (Abstraction.map self x))
  | T_for_all x -> return (T_for_all (Abstraction.map self x))


and compile_matching ~(raise : _ Trace.raise)
    :  loc:Location.t -> ?attributes:O.ValueAttr.t -> mut:bool -> O.expression
    -> (I.expression, I.type_expression) I.Match_expr.match_case list -> O.expression
  =
 fun ~loc ?attributes ~mut matchee cases ->
  ignore loc;
  (* TODO? *)
  let matchee_type = matchee.type_expression in
  let var = Value_var.fresh ~loc:Location.generated ~name:"match_" () in
  let match_expr =
    let cases =
      List.map cases ~f:(fun { pattern; body } ->
          let pattern = Linear_pattern.map (compile_type ~raise) pattern in
          let body = compile_expression ~raise body in
          I.Match_expr.{ pattern; body })
    in
    Decision_tree.compile ~raise matchee.type_expression var cases ~mut
  in
  let match_expr = if mut then destruct_mut_let_in ~raise match_expr else match_expr in
  O.e_a_let_in
    ~loc:Location.generated
    { let_binder = Binder.make var matchee_type
    ; rhs = matchee
    ; let_result = { match_expr with location = Location.generated }
    ; attributes = Option.value attributes ~default:O.ValueAttr.default_attributes
    }


(*
destruct_mut_let_in [exp] inspects [exp] for a matching expression, and prepend a mutable binding (E_let_mut_in)
for each binder present in the matched pattern

e.g.
```
var (a,b) = <matchee> ;
a := a + 1 ;
<..>
```

|--compile_matching-->
```
  let mut (a,b) = <matchee> in
  let () = assign a (a + 1) in
  <..>
```
|--destruct_mut_let_in-->
```
  match <matchee> with
  | (a,b) ->
    let mut a = a in
    let mut b = b in
    let () = assign a (a + 1) in
    <...>
```
TODO: warn if tickets in <matchee>
*)

and destruct_mut_let_in ~(raise : _ Trace.raise) : O.expression -> O.expression =
 fun match_expr ->
  let return expression_content = { match_expr with expression_content } in
  let loc = match_expr.location in
  match match_expr.expression_content with
  | O.E_matching ({ cases = O.Match_record case; _ } as prod_case) ->
    let binders = List.map ~f:snd (Record.to_list case.fields) in
    let body =
      List.fold
        binders
        ~f:(fun acc b ->
          O.e_a_let_mut_in
            ~loc:acc.location
            { let_binder = b
            ; rhs = O.e_variable ~loc (Binder.get_var b) (Binder.get_ascr b)
            ; let_result = acc
            ; attributes = O.ValueAttr.default_attributes
            })
        ~init:case.body
    in
    return (O.E_matching { prod_case with cases = O.Match_record { case with body } })
  | O.E_matching ({ cases = O.Match_variant sums; _ } as sum_case) ->
    (* because this would only happens for mutable let-ins, sum.cases should always be of size 1 *)
    let cases =
      List.map sums.cases ~f:(fun (O.{ pattern; body; constructor } as x) ->
          let ty = O.get_sum_type sums.tv constructor in
          let b = Binder.make pattern ty in
          let body =
            O.e_a_let_mut_in
              ~loc:body.location
              { let_binder = b
              ; rhs = O.e_variable ~loc pattern ty
              ; let_result = body
              ; attributes = O.ValueAttr.default_attributes
              }
          in
          { x with body })
    in
    return (O.E_matching { sum_case with cases = O.Match_variant { sums with cases } })
  | _ ->
    raise.error
    @@ corner_case
         ~loc:__LOC__
         "compiling pattern did not result in a matching expression"
