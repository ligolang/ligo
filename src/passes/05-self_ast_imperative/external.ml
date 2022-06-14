open Ast_imperative

(* This function transforms an application expression `l e1 ... en` into the pair `([ e1 ; ... ; en ] , l)` *)
let destruct_applications (e : expression) =
  let rec destruct_applications acc (lamb : expression) =
    match lamb.expression_content with
    | E_application {lamb;args} ->
       destruct_applications (args :: acc) lamb
    | _ ->
       (lamb, acc) in
  destruct_applications [] e

let destruct_args (m : expression list) : (string * expression list) option =
  match m with
  | { expression_content = E_literal (Literal_string prim) ; _ } :: args ->
     Some (Simple_utils.Ligo_string.extract prim, args)
  | _ -> None

let replace : expression -> expression = fun e ->
  match e.expression_content with
  | E_application _ -> (
     let e_, args = destruct_applications e in
     match e_.expression_content with
     | E_raw_code { language ; code = { expression_content = E_literal (Literal_string code) ; _ } }
          when String.equal language "external" -> (
       let code = (Simple_utils.Ligo_string.extract code) in
       match read_constant' code with
       | None -> failwith @@ "Constant cannot be externalized: " ^ code
       | Some cons ->
          let expression_content = E_constant { cons_name = Const cons;
                                                arguments = args } in
          { e with expression_content }
     )
     | _ -> e
  )
  | E_raw_code { language ; code = { expression_content = E_literal (Literal_string code) ; _ } }
       when String.equal language "external" -> (
    let code = (Simple_utils.Ligo_string.extract code) in
    match read_constant' code with
    | None -> failwith @@ "Constant cannot be externalized: " ^ code
    | Some cons ->
       let expression_content = E_constant { cons_name = Const cons;
                                             arguments = [] } in
       { e with expression_content }
  )
  | E_raw_code { language ; code = { expression_content = E_literal (Literal_string code) ; _ } }
       when String.equal language "external" -> (
    let code = (Simple_utils.Ligo_string.extract code) in
    match read_constant' code with
    | None -> failwith @@ "Constant cannot be externalized: " ^ code
    | Some cons ->
       let expression_content = E_constant { cons_name = Const cons;
                                             arguments = [] } in
       { e with expression_content }
  )
  | E_raw_code { language ; code = { expression_content = E_tuple m ; _ } }
       when String.equal language "ext" -> (
    match destruct_args m with
    | Some (code, arguments) -> (
       match read_constant' code with
       | None -> failwith @@ "Constant cannot be externalized: " ^ code
       | Some cons ->
          let expression_content = E_constant { cons_name = Const cons;
                                                arguments } in
          { e with expression_content }
    )
    | _ -> e
  )
  | E_raw_code { language ; code = { expression_content = E_literal (Literal_string code) ; _ } }
       when String.equal language "ext" -> (
    let code = (Simple_utils.Ligo_string.extract code) in
    match read_constant' code with
    | None -> failwith @@ "Constant cannot be externalized: " ^ code
    | Some cons ->
       let expression_content = E_constant { cons_name = Const cons;
                                             arguments = [] } in
       { e with expression_content }
  )
  | _ -> e
