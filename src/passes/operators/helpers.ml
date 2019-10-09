module Typer = struct

  open Trace
  open Ast_typed

  module Errors = struct
    let wrong_param_number = fun name expected got ->
      let title () = "wrong number of params" in
      let full () = Format.asprintf "constant name: %s\nexpected: %d\ngot: %d\n"
          name expected (List.length got) in
      error title full

    let error_uncomparable_types a b () =
      let title () = "these types are not comparable" in
      let message () = "" in
      let data = [
        ("a" , fun () -> Format.asprintf "%a" PP.type_value a) ;
        ("b" , fun () -> Format.asprintf "%a" PP.type_value b )
      ] in
      error ~data title message ()
  end
  open Errors

  type type_result = string * type_value
  type typer' = type_value list -> type_value option -> type_result result
  type typer = string * typer'

  let typer'_0 : name -> (type_value option -> type_value result) -> typer' = fun s f lst tv_opt ->
    match lst with
    | [] -> (
      let%bind tv' = f tv_opt in
      ok (s , tv')
    )
    | _ -> fail @@ wrong_param_number s 0 lst
  let typer_0 name f : typer = (name , typer'_0 name f)

  let typer'_1 : name -> (type_value -> type_value result) -> typer' = fun s f lst _ ->
    match lst with
    | [ a ] -> (
        let%bind tv' = f a in
        ok (s , tv')
      )
    | _ -> fail @@ wrong_param_number s 1 lst
  let typer_1 name f : typer = (name , typer'_1 name f)

  let typer'_1_opt : name -> (type_value -> type_value option -> type_value result) -> typer' = fun s f lst tv_opt ->
    match lst with
    | [ a ] -> (
        let%bind tv' = f a tv_opt in
        ok (s , tv')
      )
    | _ -> fail @@ wrong_param_number s 1 lst
  let typer_1_opt name f : typer = (name , typer'_1_opt name f)

  let typer'_2 : name -> (type_value -> type_value -> type_value result) -> typer' = fun s f lst _ ->
    match lst with
    | [ a ; b ] -> (
        let%bind tv' = f a b in
        ok (s , tv')
      )
    | _ -> fail @@ wrong_param_number s 2 lst
  let typer_2 name f : typer = (name , typer'_2 name f)

  let typer'_3 : name -> (type_value -> type_value -> type_value -> type_value result) -> typer' = fun s f lst _ ->
    match lst with
    | [ a ; b ; c ] -> (
        let%bind tv' = f a b c in
        ok (s , tv')
      )
    | _ -> fail @@ wrong_param_number s 3 lst
  let typer_3 name f : typer = (name , typer'_3 name f)

  let typer'_4 : name -> (type_value -> type_value -> type_value -> type_value -> type_value result) -> typer' = fun s f lst _ ->
    match lst with
    | [ a ; b ; c ; d ] -> (
        let%bind tv' = f a b c d in
        ok (s , tv')
      )
    | _ -> fail @@ wrong_param_number s 4 lst
  let typer_4 name f : typer = (name , typer'_4 name f)

  let typer'_5 : name -> (type_value -> type_value -> type_value -> type_value -> type_value -> type_value result) -> typer' = fun s f lst _ ->
    match lst with
    | [ a ; b ; c ; d ; e ] -> (
        let%bind tv' = f a b c d e in
        ok (s , tv')
      )
    | _ -> fail @@ wrong_param_number s 5 lst
  let typer_5 name f : typer = (name , typer'_5 name f)

  let typer'_6 : name -> (type_value -> type_value -> type_value -> type_value -> type_value -> type_value -> type_value result) -> typer' = fun s f lst _ ->
    match lst with
    | [ a ; b ; c ; d ; e ; f_ ] -> (
        let%bind tv' = f a b c d e f_ in
        ok (s , tv')
      )
    | _ -> fail @@ wrong_param_number s 6 lst
  let typer_6 name f : typer = (name , typer'_6 name f)

  let constant name cst = typer_0 name (fun _ -> ok cst)

  open Combinators

  let eq_1 a cst = type_value_eq (a , cst)
  let eq_2 (a , b) cst = type_value_eq (a , cst) && type_value_eq (b , cst)

  let assert_eq_1 ?msg a b = Assert.assert_true ?msg (eq_1 a b)

  let comparator : string -> typer = fun s -> typer_2 s @@ fun a b ->
    let%bind () =
      trace_strong (error_uncomparable_types a b) @@
      Assert.assert_true @@
      List.exists (eq_2 (a , b)) [
        t_int () ;
        t_nat () ;
        t_mutez () ;
        t_string () ;
        t_bytes () ;
        t_address () ;
        t_timestamp () ;
      ] in
    ok @@ t_bool ()

  let boolean_operator_2 : string -> typer = fun s -> typer_2 s @@ fun a b ->
    let%bind () =
      trace_strong (simple_error "A isn't of type bool") @@
      Assert.assert_true @@
      type_value_eq (t_bool () , a) in
    let%bind () =
      trace_strong (simple_error "B isn't of type bool") @@
      Assert.assert_true @@
      type_value_eq (t_bool () , b) in
    ok @@ t_bool ()

end

module Compiler = struct

  open Tezos_utils.Michelson

  type predicate =
    | Constant of michelson
    | Unary of michelson
    | Binary of michelson
    | Ternary of michelson
    | Tetrary of michelson
    | Pentary of michelson
    | Hexary of michelson
  let simple_constant c = Constant c
  let simple_unary c = Unary c
  let simple_binary c = Binary c
  let simple_ternary c = Ternary c
  let simple_tetrary c = Tetrary c
  let simple_pentary c = Pentary c
  let simple_hexary c = Hexary c
end
