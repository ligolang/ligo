open Trace
open Ast_typed
open Tezos_utils

module Errors = struct

  let missing_entry_point name =
    let title () = "missing entry point" in
    let content () = "no entry point with the given name" in
    let data = [
      ("name" , fun () -> name) ;
    ] in
    error ~data title content

  let not_functional_main location =
    let title () = "not functional main" in
    let content () = "main should be a function" in
    let data = [
      ("location" , fun () -> Format.asprintf "%a" Location.pp location) ;
    ] in
    error ~data title content

end

(*
   This converts `expr` in `fun () -> expr`.
*)
let functionalize (body : annotated_expression) : annotated_expression =
  let expression = E_lambda { binder = "_" ; body } in
  let type_annotation = t_function (t_unit ()) body.type_annotation () in
  { body with expression ; type_annotation }

let compile_expression : annotated_expression -> Michelson.t result = fun e ->
  let%bind mini_c_expression = Transpiler.transpile_annotated_expression e in
  Of_mini_c.compile_expression mini_c_expression

(*
   val compile_value : annotated_expression -> Michelson.t result
   This requires writing a function
   `transpile_expression_as_value : annotated_expression -> Mini_c.value result`
 *)

let compile_function expr =
  let%bind l = get_lambda expr.expression in
  let%bind io = get_t_function expr.type_annotation in
  let%bind mini_c = Transpiler.transpile_lambda Mini_c.Environment.empty l io in
  let%bind (f , (in_ty , out_ty)) =
    match (mini_c.content , mini_c.type_value) with
    | E_literal (D_function f) , T_function ty -> ok (f , ty)
    | _ -> fail @@ Errors.not_functional_main expr.location
  in
  Of_mini_c.compile_function f in_ty out_ty

(*
   Assume the following code:
   ```
     const x = 42
     const y = 120
     const z = 423
     const f = () -> x + y
   ```
   It is transformed in:
   ```
     const f = () ->
       let x = 42 in
       let y = 120 in
       let z = 423 in
       x + y
   ```

   To do so, each declaration `const variable = expr` is translated in
   a function `body -> let variable = expr in body`. Those functions are
   then applied in order, which yields `let x = 42 in let y = 120 in ...`.

   The entry-point can be an expression, which is then functionalized if
   `to_functionalize` is set to true.
*)
let aggregate_declarations_for_entry (lst : program) (name : string) (to_functionalize : bool) : annotated_expression result =
  let rec aux acc (lst : program) =
    let%bind acc = acc in
    match lst with
    | [] -> fail @@ Errors.missing_entry_point name
    | hd :: tl -> (
        let (Declaration_constant (an , (pre_env , _))) = Location.unwrap hd in
        if (an.name <> name) then (
          let next = fun expr ->
            let cur = e_a_let_in an.name an.annotated_expression expr pre_env in
            acc cur in
          aux (ok next) tl
        ) else (
          match (an.annotated_expression.expression , to_functionalize) with
          | (E_lambda l , false) -> (
            let l' = { l with body = acc l.body } in
            let e' = { an.annotated_expression with expression = E_lambda l' } in
            ok e'
          )
          | (_ , true) -> (
              ok @@ functionalize @@ acc an.annotated_expression
            )
          | _ -> fail @@ Errors.not_functional_main an.annotated_expression.location
        )
      )
  in
  let%bind l = aux (ok (fun x -> x)) lst in
  ok l

let compile_function_entry : program -> string -> _ = fun p entry ->
  let%bind expr = aggregate_declarations_for_entry p entry false in
  compile_function expr

let compile_expression_entry : program -> string -> _ = fun p entry ->
  let%bind expr = aggregate_declarations_for_entry p entry true in
  compile_function expr

let compile_expression_as_function : annotated_expression -> Compiler.Program.compiled_program result = fun e ->
  let expr = functionalize e in
  compile_function expr
