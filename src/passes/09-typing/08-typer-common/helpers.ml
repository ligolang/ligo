open Errors
open Ast_typed
open Trace

let assert_type_expression_eq ((tv',tv):type_expression * type_expression) : (unit,typer_error) result = 
  trace_option (assert_equal tv' tv) @@
    assert_type_expression_eq (tv' , tv)

type typer = type_expression list -> type_expression -> (type_expression, typer_error) result

let typer_0 : string -> (type_expression -> Location.t -> (type_expression, typer_error) result) -> Location.t -> typer = fun s f l lst tv_opt ->
  match lst with
  | [] -> (
    let%bind tv' = f tv_opt l in
    ok (tv')
  )
  | _ -> fail @@ wrong_param_number s 0 lst

let typer_1 : string -> (type_expression -> (type_expression, typer_error) result) -> typer = fun s f lst _ ->
  match lst with
  | [ a ] -> (
      let%bind tv' = f a in
      ok (tv')
    )
  | _ -> fail @@ wrong_param_number s 1 lst

let typer_1_opt : string -> (type_expression -> type_expression -> (type_expression , typer_error) result) -> typer = fun s f lst tv_opt ->
  match lst with
  | [ a ] -> (
      let%bind tv' = f a tv_opt in
      ok (tv')
    )
  | _ -> fail @@ wrong_param_number s 1 lst

let typer_2 : string -> (type_expression -> type_expression -> (type_expression, typer_error) result) -> typer = fun s f lst _ ->
  match lst with
  | [ a ; b ] -> (
      let%bind tv' = f a b in
      ok (tv')
    )
  | _ -> fail @@ wrong_param_number s 2 lst

let typer_2_opt : string -> (type_expression -> type_expression -> type_expression -> (type_expression, typer_error) result) -> typer = fun s f lst tv_opt ->
  match lst with
  | [ a ; b ] -> (
      let%bind tv' = f a b tv_opt in
      ok (tv')
    )
  | _ -> fail @@ wrong_param_number s 2 lst

let typer_3 : string -> (type_expression -> type_expression -> type_expression -> (type_expression, typer_error) result) -> typer = fun s f lst _ ->
  match lst with
  | [ a ; b ; c ] -> (
      let%bind tv' = f a b c in
      ok (tv')
    )
  | _ -> fail @@ wrong_param_number s 3 lst

let typer_4 : string -> (type_expression -> type_expression -> type_expression -> type_expression -> (type_expression , typer_error) result) -> typer = fun s f lst _ ->
  match lst with
  | [ a ; b ; c ; d ] -> (
      let%bind tv' = f a b c d in
      ok (tv')
    )
  | _ -> fail @@ wrong_param_number s 4 lst

let typer_5 : string -> (type_expression -> type_expression -> type_expression -> type_expression -> type_expression -> (type_expression, typer_error) result) -> typer = fun s f lst _ ->
  match lst with
  | [ a ; b ; c ; d ; e ] -> (
      let%bind tv' = f a b c d e in
      ok (tv')
    )
  | _ -> fail @@ wrong_param_number s 5 lst

let typer_6 : string
  -> (type_expression -> type_expression -> type_expression -> type_expression -> type_expression -> type_expression -> (type_expression, typer_error) result) -> typer = fun s f lst _ ->
  match lst with
  | [ a ; b ; c ; d ; e ; f_ ] -> (
      let%bind tv' = f a b c d e f_ in
      ok (tv')
    )
  | _ -> fail @@ wrong_param_number s 6 lst

let constant' name cst = typer_0 name (fun _ _ -> ok cst)
let eq_1 a cst = type_expression_eq (a , cst)
let eq_2 (a , b) cst = type_expression_eq (a , cst) && type_expression_eq (b , cst)

let assert_eq_1 a b = if eq_1 a b then Some () else None
let assert_eq a b = trace_option (not_matching a b) @@ assert_eq_1 a b
