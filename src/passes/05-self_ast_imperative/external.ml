open Ast_imperative

let destruct_args (m : expression list) : (string * expression list) option =
  match m with
  | { expression_content = E_literal (Literal_string prim) ; _ } :: args ->
     Some (Simple_utils.Ligo_string.extract prim, args)
  | _ -> None

let replace : expression -> expression = fun e ->
   match e.expression_content with
   | E_raw_code { language ; code = { expression_content = E_tuple m ; _ } }
      when String.equal language "external" -> (
      match destruct_args m with
      | Some (code, arguments) -> (
         match Ligo_prim.Constant.read_constant' code with
         | None -> failwith @@ "Constant cannot be externalized: " ^ code
         | Some cons ->
            let expression_content = E_constant { cons_name = Const cons;
                                                  arguments } in
            { e with expression_content }
      )
      | _ -> e
   )
   | E_raw_code { language ; code = { expression_content = E_literal (Literal_string code) ; _ } }
      when String.equal language "external" -> (
      let code = (Simple_utils.Ligo_string.extract code) in
      match Ligo_prim.Constant.read_constant' code with
      | None -> failwith @@ "Constant cannot be externalized: " ^ code
      | Some cons ->
         let expression_content = E_constant { cons_name = Const cons;
                                               arguments = [] } in
         { e with expression_content }
   )
   | _ -> e
