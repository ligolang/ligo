module PP_helpers = Simple_utils.PP_helpers
module AST = Ast_aggregated

let fold_map_expression = Helpers.fold_map_expression
let fvs_expression = Helpers.Free_variables.expression

let create_contract ~raise expr =
  let open AST in
  let open Errors in
  let (), _ = fold_map_expression (fun () expr ->
                  match expr.expression_content with
                  | E_constant { cons_name = C_CREATE_CONTRACT ;
                                 arguments = { expression_content = E_lambda _ ; _ } as lambda :: _ } -> (
                    let fvs = fvs_expression lambda in
                    if Int.equal (List.length fvs) 0 then true, (), expr
                    else raise.Trace.raise @@ fvs_in_create_contract_lambda expr (List.hd_exn fvs)
                  )
                  | E_constant { cons_name = C_CREATE_CONTRACT ;
                                 arguments =  _ } -> (
                    raise.Trace.raise @@ create_contract_lambda C_CREATE_CONTRACT expr
                  )
                  | _ -> true, (), expr) () expr in
  expr
