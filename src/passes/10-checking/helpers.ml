open Ast_typed
open Errors
open Trace

let assert_type_expression_eq (loc:Location.t) ((tv',tv):type_expression * type_expression) : (unit,typer_error) result = 
  trace_option (assert_equal loc tv' tv) @@
    assert_type_expression_eq (tv' , tv)

type typer = type_expression list -> type_expression option -> (type_expression, typer_error) result

let typer_0 : Location.t -> string -> (type_expression option -> (type_expression, typer_error) result) -> typer = fun l s f lst tv_opt ->
  match lst with
  | [] -> (
    let* tv' = f tv_opt in
    ok (tv')
  )
  | _ -> fail @@ wrong_param_number l s 0 lst

let typer_1 : Location.t -> string -> (type_expression -> (type_expression, typer_error) result) -> typer = fun l s f lst _ ->
  match lst with
  | [ a ] -> (
      let* tv' = f a in
      ok (tv')
    )
  | _ -> fail @@ wrong_param_number l s 1 lst

let typer_1_opt : Location.t -> string -> (type_expression -> type_expression option -> (type_expression , typer_error) result) -> typer = fun l s f lst tv_opt ->
  match lst with
  | [ a ] -> (
      let* tv' = f a tv_opt in
      ok (tv')
    )
  | _ -> fail @@ wrong_param_number l s 1 lst

let typer_2 : Location.t -> string -> (type_expression -> type_expression -> (type_expression, typer_error) result) -> typer = fun l s f lst _ ->
  match lst with
  | [ a ; b ] -> (
      let* tv' = f a b in
      ok (tv')
    )
  | _ -> fail @@ wrong_param_number l s 2 lst

let typer_2_opt : Location.t -> string -> (type_expression -> type_expression -> type_expression option -> (type_expression, typer_error) result) -> typer = fun l s f lst tv_opt ->
  match lst with
  | [ a ; b ] -> (
      let* tv' = f a b tv_opt in
      ok (tv')
    )
  | _ -> fail @@ wrong_param_number l s 2 lst

let typer_3 : Location.t -> string -> (type_expression -> type_expression -> type_expression -> (type_expression, typer_error) result) -> typer = fun l s f lst _ ->
  match lst with
  | [ a ; b ; c ] -> (
      let* tv' = f a b c in
      ok (tv')
    )
  | _ -> fail @@ wrong_param_number l s 3 lst

let typer_4 : Location.t -> string -> (type_expression -> type_expression -> type_expression -> type_expression -> (type_expression , typer_error) result) -> typer = fun l s f lst _ ->
  match lst with
  | [ a ; b ; c ; d ] -> (
      let* tv' = f a b c d in
      ok (tv')
    )
  | _ -> fail @@ wrong_param_number l s 4 lst

let typer_5 : Location.t -> string -> (type_expression -> type_expression -> type_expression -> type_expression -> type_expression -> (type_expression, typer_error) result) -> typer = fun l s f lst _ ->
  match lst with
  | [ a ; b ; c ; d ; e ] -> (
      let* tv' = f a b c d e in
      ok (tv')
    )
  | _ -> fail @@ wrong_param_number l s 5 lst

let typer_6 : Location.t -> string
  -> (type_expression -> type_expression -> type_expression -> type_expression -> type_expression -> type_expression -> (type_expression, typer_error) result) -> typer = fun l s f lst _ ->
  match lst with
  | [ a ; b ; c ; d ; e ; f_ ] -> (
      let* tv' = f a b c d e f_ in
      ok (tv')
    )
  | _ -> fail @@ wrong_param_number l s 6 lst

let constant' loc name cst = typer_0 loc name (fun _ -> ok cst)
let eq_1 a cst = type_expression_eq (a , cst)
let eq_2 (a , b) cst = type_expression_eq (a , cst) && type_expression_eq (b , cst)

let assert_eq_1 a b = if eq_1 a b then Some () else None
let assert_eq l a b = trace_option (not_matching l a b) @@ assert_eq_1 a b
