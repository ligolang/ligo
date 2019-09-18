open Types
open Combinators
open Trace

module Errors = struct

  let missing_entry_point name =
    let title () = "missing entry point" in
    let content () = "no entry point with the given name" in
    let data = [
      ("name" , fun () -> name) ;
    ] in
    error ~data title content

  let not_functional_main name =
    let title () = "not functional main" in
    let content () = "main should be a function" in
    let data = [
      ("name" , fun () -> Format.asprintf "%s" name) ;
    ] in
    error ~data title content

end

(*
   Converts `expr` in `fun () -> expr`.
*)
let functionalize (body : expression) : expression =
  let content = E_literal (D_function { binder = "_" ; body }) in
  let type_value = t_function t_unit body.type_value in
  { content ; type_value }

let get_entry (lst : program) (name : string) : (expression * int) result =
  let%bind entry_expression =
    trace_option (Errors.missing_entry_point name) @@
    let aux x =
      let (((decl_name , decl_expr) , _)) = x in
      if (decl_name = name)
      then Some decl_expr
      else None
    in
    List.find_map aux lst
  in
  let entry_index =
    let aux x =
      let (((decl_name , _) , _)) = x in
      decl_name = name
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
let aggregate_entry (lst : program) (name : string) (to_functionalize : bool) : expression result =
  let%bind (entry_expression , entry_index) = get_entry lst name in
  let pre_declarations = List.until entry_index lst in
  let wrapper =
    let aux prec cur =
      let (((name , expr) , _)) = cur in
      e_let_in name expr.type_value expr prec
    in
    fun expr -> List.fold_right' aux expr pre_declarations
  in
  match (entry_expression.content , to_functionalize) with
  | (E_literal (D_function l) , false) -> (
      let l' = { l with body = wrapper l.body } in
      let e' = { entry_expression with content = E_literal (D_function l') } in
      ok e'
    )
  | (_ , true) -> (
      ok @@ functionalize @@ wrapper entry_expression
    )
  | _ -> fail @@ Errors.not_functional_main name
