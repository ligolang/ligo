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


let get_entry (lst : program) (name : string) : (annotated_expression * int) result =
  let%bind entry_expression =
    trace_option (Errors.missing_entry_point name) @@
    let aux x =
      let (Declaration_constant (an , _)) = Location.unwrap x in
      if (an.name = name)
      then Some an.annotated_expression
      else None
    in
    List.find_map aux lst
  in
  let entry_index =
    let aux x =
      let (Declaration_constant (an , _)) = Location.unwrap x in
      an.name = name
    in
    List.find_index aux lst
  in
  ok (entry_expression , entry_index)

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

   The entry-point can be an expression, which is then functionalized if
   `to_functionalize` is set to true.
*)
let get_aggregated_entry (lst : program) (name : string) (to_functionalize : bool) : annotated_expression result =
  let%bind (entry_expression , entry_index) = get_entry lst name in
  let pre_declarations =
    let sub_program = List.until entry_index lst in
    let aux x = Location.unwrap x in
    List.map aux sub_program
  in
  let wrapper =
    let aux prec cur =
      let (Declaration_constant (an , (pre_env , _))) = cur in
      e_a_let_in an.name an.annotated_expression prec pre_env
    in
    fun expr -> List.fold_right' aux expr pre_declarations
  in
  match (entry_expression.expression , to_functionalize) with
  | (E_lambda l , false) -> (
      let l' = { l with body = wrapper l.body } in
      let e' = { entry_expression with expression = E_lambda l' } in
      ok e'
    )
  | (_ , true) -> (
      ok @@ functionalize @@ wrapper entry_expression
    )
  | _ -> fail @@ Errors.not_functional_main entry_expression.location

let compile_function_entry : program -> string -> _ = fun p entry ->
  let%bind expr = get_aggregated_entry p entry false in
  compile_function expr

let compile_expression_entry : program -> string -> _ = fun p entry ->
  let%bind expr = get_aggregated_entry p entry true in
  compile_function expr

let compile_expression_as_function : annotated_expression -> Compiler.Program.compiled_program result = fun e ->
  let expr = functionalize e in
  compile_function expr

let uncompile_value : _ -> _ -> annotated_expression result = fun x ty ->
  let%bind mini_c = Of_mini_c.uncompile_value x in
  Transpiler.untranspile mini_c ty

let uncompile_entry_function_result = fun program entry ex_ty_value ->
  let%bind output_type =
    let%bind (entry_expression , _ ) = get_entry program entry in
    let%bind (_ , output_type) = get_t_function entry_expression.type_annotation in
    ok output_type
  in
  uncompile_value ex_ty_value output_type

let uncompile_entry_expression_result = fun program entry ex_ty_value ->
  let%bind output_type =
    let%bind (entry_expression , _ ) = get_entry program entry in
    ok entry_expression.type_annotation
  in
  uncompile_value ex_ty_value output_type
