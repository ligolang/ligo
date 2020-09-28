open Types

let rec expression : environment -> expression -> expression = fun env expr ->
  (* Standard helper functions to help with the fold *)
  let return content = {
      expr with
      expression_content = content ;
    } in
  let return_id = return expr.expression_content in
  let self ?(env' = env) x = expression env' x in
  let self_list lst = List.map self lst in
  let self_2 a b = self a , self b in
  let self_lmap lm = LMap.map self lm in
  let self_cases cs = cases env cs in
  match expr.expression_content with
  | E_lambda c -> (
    let (t_binder , _) = Combinators.get_t_function_exn expr.type_expression in
    let env' = Environment.add_ez_binder c.binder t_binder env in
    let result = self ~env' c.result in
    return @@ E_lambda { c with result }
  )
  | E_let_in c -> (
    let rhs = self c.rhs in
    let env' = Environment.add_ez_declaration c.let_binder rhs env in
    let let_result = self ~env' c.let_result in
    return @@ E_let_in { c with rhs ; let_result }
  )
  (* rec fun_name binder -> result *)
  | E_recursive c -> (
    let env_fun_name = Environment.add_ez_binder c.fun_name c.fun_type env in
    let (t_binder , _) = Combinators.get_t_function_exn c.fun_type in
    let env_binder = Environment.add_ez_binder c.lambda.binder t_binder env_fun_name in
    let result = self ~env':env_binder c.lambda.result in
    let lambda = { c.lambda with result } in
    return @@ E_recursive { c with lambda }
  )
  (* All the following cases are administrative *)
  | E_literal _ -> return_id
  | E_variable _ -> return_id
  | E_constant c -> (
    let arguments = self_list c.arguments in
    return @@ E_constant { c with arguments }
  )
  | E_application c -> (
    let (lamb , args) = self_2 c.lamb c.args in
    return @@ E_application { lamb ; args }
  )
  | E_raw_code _ -> return_id
  | E_constructor c -> (
    let element = self c.element in
    return @@ E_constructor { c with element }
  )
  | E_record c -> (
    let c' = self_lmap c in
    return @@ E_record c'
  )
  | E_record_accessor c -> (
    let record = self c.record in
    return @@ E_record_accessor { c with record }
  )
  | E_record_update c -> (
    let (record , update) = self_2 c.record c.update in
    return @@ E_record_update { c with record ; update }
  )
  | E_matching c -> (
    let matchee = self c.matchee in
    let cases = self_cases c.cases in
    return @@ E_matching { matchee ; cases }
  )

and cases : environment -> matching_expr -> matching_expr = fun env cs ->
  let return x = x in
  let self ?(env' = env) x = expression env' x in
  match cs with
  | Match_list c -> (
    let match_nil = self c.match_nil in
    let match_cons =
      let mc = c.match_cons in
      let env_hd = Environment.add_ez_binder mc.hd mc.tv env in
      let env_tl = Environment.add_ez_binder mc.tl (Combinators.t_list mc.tv) env_hd in
      let body = self ~env':env_tl mc.body in
      { mc with body }
    in
    return @@ Match_list { match_nil ; match_cons }
  )
  | Match_option c -> (
    let match_none = self c.match_none in
    let match_some =
      let ms = c.match_some in
      let env' = Environment.add_ez_binder ms.opt ms.tv env in
      let body = self ~env' ms.body in
      { ms with body }
    in
    return @@ Match_option { match_none ; match_some }
  )
  | Match_variant c -> (
    let variant_type = Combinators.get_t_sum_exn c.tv in
    let cases =
      let aux (c : matching_content_case) =
        let case =
          try (
            LMap.find c.constructor variant_type.content
          ) with _ -> raise (Failure ("Internal error: broken invariant at " ^ __LOC__))
        in
        let env' = Environment.add_ez_binder c.pattern case.associated_type env in
        let body = self ~env' c.body in
        { c with body }
      in
      List.map aux c.cases
    in
    return @@ Match_variant { c with cases }
  )

let program : environment -> program -> environment * program = fun init_env prog ->
  (*
      BAD
      We take the old type environment and add it to the current value environment
      because type declarations are removed in the typer. They should be added back.
   *)
  let aux (pre_env , rev_decls) decl_wrapped =
    match Location.unwrap decl_wrapped with
    | Declaration_constant c -> (
      let expr = expression pre_env c.expr in
      let post_env = Environment.add_ez_declaration c.binder expr pre_env in
      let wrap_content = Declaration_constant { c with expr } in
      let decl_wrapped' = { decl_wrapped with wrap_content } in
      (post_env , decl_wrapped' :: rev_decls)
    )
    | Declaration_type t -> (
      let post_env = Environment.add_type t.type_binder t.type_expr pre_env in      
      let wrap_content = Declaration_type t in
      let decl_wrapped' = { decl_wrapped with wrap_content } in
      (post_env , decl_wrapped' :: rev_decls)
    )
  in
  let (last_env , rev_decls) = List.fold_left aux (init_env , []) prog in
  (last_env , List.rev rev_decls)
