module List = Simple_utils.List
open Errors
open Ast_imperative
open Simple_utils.Trace

let peephole_expression ~raise : expression -> expression = fun e ->
  let return expression_content = { e with expression_content } in
  match e.expression_content with
  | E_literal (Literal_key_hash s) as l -> (
    let open Tezos_crypto in
    match Signature.Public_key_hash.of_b58check_opt s with
    | None -> raise.error (bad_format_literal e)
    | Some _ -> return l
    )
  | E_literal (Literal_address _) as l -> (
    return l
    )
  | E_literal (Literal_signature s) as l -> (
    let open Tezos_crypto in
    match Signature.of_b58check_opt s with
    | None -> raise.error (bad_format_literal e)
    | Some _ -> return l
    )
  | E_literal (Literal_key s) as l -> (
    let open Tezos_crypto in
    match Signature.Public_key.of_b58check_opt s with
    | None -> raise.error (bad_format_literal e)
    | Some _ ->
      return l
    )
  | e -> return e
