(*

   This pass removes the counter from variables which were introduced
   using `fresh_like`.

   It should not be normally used. Its introduction is a response to
   fix the problem of mutation saving after deduplication in the AST
   has been applied.

*)

open Ligo_prim
open Ast_aggregated

let rec reduplicate ~raise : expression -> expression =
  fun exp ->
  let self = reduplicate ~raise in
  let remove_counter v = if Value_var.is_generated v then v else Value_var.(of_input_var ~loc:(get_location v) (to_name_exn v)) in
  let return expression_content : expression =
    { exp with expression_content } in
  let binder_remove_counter = fun b -> Binder.set_var b (Binder.apply remove_counter b) in
  let param_remove_counter = fun b -> Param.set_var b (remove_counter @@ Param.get_var b) in
  match exp.expression_content with
    | E_literal l ->
       return (E_literal l)
    | E_constant { cons_name ; arguments } ->
       let arguments = List.map ~f:self arguments in
       return (E_constant { cons_name ; arguments })
    | E_variable v ->
       return (E_variable (remove_counter v))
    | E_application { lamb ; args } ->
       let args = self args in
       let lamb = self lamb in
       return (E_application { lamb ; args })
    | E_lambda { binder ; output_type ; result } ->
       let binder = param_remove_counter binder in
       let result = self result in
       return (E_lambda { binder ; output_type ; result })
    | E_type_abstraction { type_binder ; result } ->
       let result = self result in
       return (E_type_abstraction { type_binder ; result })
    | E_recursive { fun_name ; fun_type ; lambda = { binder ; output_type ; result } } ->
       let result = self result in
       let binder = param_remove_counter binder in
       return (E_recursive { fun_name ; fun_type ; lambda = { binder ; output_type ; result } })
    | E_let_in { let_binder ; rhs ; let_result ; attr } ->
       let rhs = self rhs in
       let let_result = self let_result in
       let let_binder = binder_remove_counter let_binder in
       return (E_let_in { let_binder ; rhs ; let_result ; attr })
    | E_raw_code { language ; code } ->
       let code = self code in
       return (E_raw_code { language ; code })
    | E_type_inst { forall ; type_ } ->
       let forall = self forall in
       return (E_type_inst { forall ; type_ })
    (* Variant *)
    | E_constructor { constructor ; element } ->
       let element = self element in
       return (E_constructor { constructor ; element })
    | E_matching { matchee ; cases = Match_variant { cases ; tv } } ->
       let matchee = self matchee in
       let f { constructor ; pattern ; body } =
         let body = self body in
         let pattern = remove_counter pattern in
         { constructor ; pattern ; body } in
       let cases = List.map ~f cases in
       return (E_matching { matchee ; cases = Match_variant { cases ; tv } })
    | E_matching { matchee ; cases = Match_record { fields ; body ; tv } } ->
       let matchee = self matchee in
       let body = self body in
       let fields = Record.map binder_remove_counter fields in
       return (E_matching { matchee ; cases = Match_record { fields ; body ; tv } })
    (* Record *)
    | E_record map ->
       let map = Record.map (self) map in
       return (E_record map)
    | E_accessor { struct_ ; path } ->
       let struct_ = self struct_ in
       return (E_accessor { struct_ ; path })
    | E_update { struct_ ; path ; update } ->
       let struct_ = self struct_ in
       let update = self update in
       return (E_update { struct_ ; path ; update })
   | E_assign {binder;expression} ->
      let binder = binder_remove_counter binder in
      let expression = self expression in
      return @@ E_assign {binder;expression}
   | E_let_mut_in { let_binder; rhs; let_result; attr } ->
      let let_binder = binder_remove_counter let_binder in
      let rhs = self rhs in
      let let_result = self let_result in
      return @@ E_let_mut_in { let_binder; rhs; let_result; attr }
   | E_for { binder; start; incr; final; f_body } ->
      let binder = remove_counter binder in
      let start = self start in
      let incr = self incr in
      let final = self final in
      let f_body = self f_body in
      return @@ E_for { binder; start; incr; final; f_body }
   | E_while { cond; body } ->
      let cond = self cond in
      let body = self body in
      return @@ E_while { cond; body }
   | E_for_each { fe_binder = binder1, binder2; collection; collection_type; fe_body } ->
      let fe_binder = 
         remove_counter binder1, Option.map ~f:remove_counter binder2
      in
      let collection = self collection in
      let fe_body = self fe_body in
      return @@ E_for_each { fe_binder; collection; collection_type; fe_body }
   | E_deref mut_var -> return @@ E_deref (remove_counter mut_var)
