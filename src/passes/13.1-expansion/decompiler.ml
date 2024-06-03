module I = Ast_expanded
module O = Ast_aggregated
module Ligo_pair = Simple_utils.Ligo_pair
open Ligo_prim

(*

very naive decompilation

*)

let rec decompile_expression : I.expression -> O.expression =
 fun exp ->
  let self = decompile_expression in
  let return : O.expression_content -> O.expression =
   fun expression_content ->
    { expression_content
    ; type_expression = decompile_type exp.type_expression
    ; location = exp.location
    }
  in
  let loc = exp.location in
  match exp.expression_content with
  | E_matching { matchee; cases = Match_record { fields; body; tv = _ } } ->
    let rhs = self matchee in
    let let_result = self body in
    let fields = Record.map fields ~f:(O.Pattern.var ~loc) in
    let let_binder =
      O.Pattern.map decompile_type @@ O.Pattern.record_pattern ~loc:exp.location fields
    in
    return
      (O.E_let_in
         { let_binder; rhs; let_result; attributes = Value_attr.default_attributes })
  | E_matching
      { matchee
      ; cases = Match_variant { cases = [ { constructor; pattern; body } ]; tv }
      } ->
    let rhs = self matchee in
    let let_result = self body in
    let let_binder =
      let v = O.Pattern.var ~loc (Binder.make pattern (I.get_sum_type tv constructor)) in
      O.Pattern.map decompile_type
      @@ O.Pattern.variant_pattern ~loc:exp.location (constructor, v)
    in
    return
      (O.E_let_in
         { let_binder; rhs; let_result; attributes = Value_attr.default_attributes })
  | E_matching { matchee; cases = Match_variant { cases; tv } } ->
    let matchee = self matchee in
    let f I.{ constructor; pattern; body } =
      let body = self body in
      let pattern =
        let v =
          O.Pattern.var ~loc (Binder.make pattern (I.get_sum_type tv constructor))
        in
        O.Pattern.map decompile_type
        @@ O.Pattern.variant_pattern ~loc:exp.location (constructor, v)
      in
      O.Match_expr.{ pattern; body }
    in
    let cases = List.map ~f cases in
    return (O.E_matching { matchee; disc_label = None; cases })
  | E_let_in { let_binder; rhs; let_result; attributes } ->
    let rhs = self rhs in
    let let_result = self let_result in
    let let_binder = O.Pattern.map decompile_type @@ O.Pattern.var ~loc let_binder in
    return (O.E_let_in { let_binder; rhs; let_result; attributes })
  | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    let rhs = self rhs in
    let let_result = self let_result in
    let let_binder = O.Pattern.map decompile_type @@ O.Pattern.var ~loc let_binder in
    return (O.E_let_mut_in { let_binder; rhs; let_result; attributes })
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
    let type_ = decompile_type type_ in
    return (O.E_type_inst { forall; type_ })
  | E_lambda l ->
    let l = Lambda.map self decompile_type l in
    return (O.E_lambda l)
  | E_type_abstraction ta ->
    let ta = Type_abs.map self ta in
    return (O.E_type_abstraction ta)
  | E_recursive r ->
    let r = Recursive.map self decompile_type r in
    return (O.E_recursive r)
  | E_constant c ->
    let c = Constant.map self c in
    return (O.E_constant c)
  | E_raw_code { language; code } ->
    let code = self code in
    return (O.E_raw_code { language; code })
  | E_assign a ->
    let a = Assign.map self decompile_type a in
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


and decompile_type : I.type_expression -> O.type_expression =
 fun ty ->
  let return type_content : O.type_expression =
    { type_content
    ; location = ty.location
    ; source_type = ty.source_type
    ; abbrev =
        Option.map ty.orig_var ~f:(fun v -> O.{ orig_var = [], v; applied_types = [] })
    }
  in
  match ty.type_content with
  | T_variable v -> return (O.T_variable v)
  | T_constant { language; injection; parameters } ->
    let parameters = List.map ~f:decompile_type parameters in
    return (O.T_constant { language; injection; parameters })
  | T_sum { fields; layout } ->
    let fields = Record.map ~f:decompile_type fields in
    return (O.T_sum ({ fields; layout }, None))
  | T_record { fields; layout } ->
    let fields = Record.map ~f:decompile_type fields in
    return (O.T_record { fields; layout })
  | T_arrow { type1; type2; param_names } ->
    let type1 = decompile_type type1 in
    let type2 = decompile_type type2 in
    return (O.T_arrow { type1; type2; param_names })
  | T_singleton l -> return (O.T_singleton l)
  | T_for_all { ty_binder; kind; type_ } ->
    let type_ = decompile_type type_ in
    return (O.T_for_all { ty_binder; kind; type_ })
