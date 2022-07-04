open Ast_typed
open Errors
open Simple_utils.Trace

let assert_type_expression_eq ~raise (loc:Location.t) ((tv',tv):type_expression * type_expression) : unit =
  trace_option ~raise (assert_equal loc tv' tv) @@
    assert_type_expression_eq (tv' , tv)

type typer = type_expression list -> type_expression option -> type_expression

let typer_2 ~raise : Location.t -> string -> (type_expression -> type_expression -> type_expression) -> typer = fun l s f lst _ ->
  match lst with
  | [ a ; b ] -> f a b
  | _ -> raise.error @@ wrong_param_number l s 2 lst

let eq_1 a cst = type_expression_eq (a , cst)
let eq_2 (a , b) cst = type_expression_eq (a , cst) && type_expression_eq (b , cst)

(*
  [first_success] ~raise [f] [lst]:
    Applies [f] to all elements of [lst] (from left to right) and returns the first call that succeeded.
    If all calls failed, raises the first encountered error
*)
let first_success ~raise (f: raise:_ raise -> _ -> _) (lst : 'a List.Ne.t) =
  let f a ~raise = f ~raise a in (* weird switch for compatibility *)
  let funcs = List.Ne.map f @@ lst in
  bind_exists ~raise funcs

