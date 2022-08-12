open Ast_aggregated

let rec reduplicate ~raise : expression -> expression =
  fun exp ->
  let self = reduplicate ~raise in
  let remove_counter v = if ValueVar.is_generated v then v else ValueVar.(of_input_var ~loc:(get_location v) (to_name_exn v)) in
  let return expression_content : expression =
    { exp with expression_content } in
  let binder_remove_counter = fun {var; ascr; attributes} ->
    let var = remove_counter var in
    {var; ascr; attributes} in
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
    | E_lambda { binder ; result } ->
       let binder = binder_remove_counter binder in
       let result = self result in
       return (E_lambda { binder ; result })
    | E_type_abstraction { type_binder ; result } ->
       let result = self result in
       return (E_type_abstraction { type_binder ; result })
    | E_recursive { fun_name ; fun_type ; lambda = { binder ; result } } ->
       let result = self result in
       let binder = binder_remove_counter binder in
       return (E_recursive { fun_name ; fun_type ; lambda = { binder ; result } })
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
       let fields = LMap.map binder_remove_counter fields in
       return (E_matching { matchee ; cases = Match_record { fields ; body ; tv } })
    (* Record *)
    | E_record map ->
       let map = LMap.map (self) map in
       return (E_record map)
    | E_record_accessor { record ; path } ->
       let record = self record in
       return (E_record_accessor { record ; path })
    | E_record_update { record ; path ; update } ->
       let record = self record in
       let update = self update in
       return (E_record_update { record ; path ; update })
   | E_assign {binder;expression} ->
      let binder = binder_remove_counter binder in
      let expression = self expression in
      return @@ E_assign {binder;expression}
