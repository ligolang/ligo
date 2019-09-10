open Trace
open Ligo.Run
open Test_helpers

let type_file = type_file Pascaligo

let get_program =
  let s = ref None in
  fun () -> match !s with
    | Some s -> ok s
    | None -> (
        let%bind program = type_file "./contracts/heap-instance.ligo" in
        s := Some program ;
        ok program
      )

let a_heap_ez ?value_type (content:(int * Ast_typed.ae) list) =
  let open Ast_typed.Combinators in
  let content =
    let aux = fun (x, y) -> e_a_empty_nat x, y in
    List.map aux content in
  let value_type = match value_type, content with
    | None, hd :: _ -> (snd hd).type_annotation
    | Some s, _ -> s
    | _ -> raise (Failure "no value type and heap empty when building heap") in
  e_a_empty_map content (t_nat ()) value_type

let ez lst =
  let open Ast_typed.Combinators in
  let value_type = t_pair
      (t_int ())
      (t_string ())
      ()
  in
  let lst' =
    let aux (i, (j, s)) =
      (i, e_a_empty_pair (e_a_empty_int j) (e_a_empty_string s)) in
    List.map aux lst in
  a_heap_ez ~value_type lst'

let dummy n =
  ez List.(
    map (fun n -> (n, (n, string_of_int n)))
    @@ tl
    @@ range (n + 1)
  )

let is_empty () : unit result =
  let%bind program = get_program () in
  let aux n =
    let open Ast_typed.Combinators in
    let input = dummy n in
    let%bind result = run_typed "is_empty" program input in
    let expected = e_a_empty_bool (n = 0) in
    Ast_typed.assert_value_eq (expected, result)
  in
  let%bind _ = bind_list
    @@ List.map aux
    @@ [0 ; 2 ; 7 ; 12] in
  ok ()

let get_top () : unit result =
  let%bind program = get_program () in
  let aux n =
    let open Ast_typed.Combinators in
    let input = dummy n in
    match n, run_typed "get_top" program input with
    | 0, Trace.Ok _ -> simple_fail "unexpected success"
    | 0, _ -> ok ()
    | _, result ->
        let%bind result' = result in
        let expected = e_a_empty_pair (e_a_empty_int 1) (e_a_empty_string "1") in
        Ast_typed.assert_value_eq (expected, result')
  in
  let%bind _ = bind_list
    @@ List.map aux
    @@ [0 ; 2 ; 7 ; 12] in
  ok ()

let pop_switch () : unit result =
  let%bind program = get_program () in
  let aux n =
    let input = dummy n in
    match n, run_typed "pop_switch" program input with
    | 0, Trace.Ok _ -> simple_fail "unexpected success"
    | 0, _ -> ok ()
    | _, result ->
        let%bind result' = result in
        let expected = ez List.(
            map (fun i -> if i = 1 then (1, (n, string_of_int n)) else (i, (i, string_of_int i)))
            @@ tl
            @@ range (n + 1)
          ) in
        Ast_typed.assert_value_eq (expected, result')
  in
  let%bind _ = bind_list
    @@ List.map aux
    @@ [0 ; 2 ; 7 ; 12] in
  ok ()

let pop () : unit result =
  let%bind program = get_program () in
  let aux n =
    let input = dummy n in
    (match run_typed "pop" program input with
    | Trace.Ok (output , _) -> (
        Format.printf "\nPop output on %d : %a\n" n Ast_typed.PP.annotated_expression output ;
      )
    | Trace.Error err -> (
        Format.printf "\nPop output on %d : error\n" n) ;
        Format.printf "Errors : {\n%a}\n%!" error_pp (err ()) ;
    ) ;
    ok ()
  in
  let%bind _ = bind_list
    @@ List.map aux
    @@ [2 ; 7 ; 12] in
  simple_fail "display"
  (* ok () *)

let main = test_suite "Heap (End to End)" [
    test "is_empty" is_empty ;
    test "get_top" get_top ;
    test "pop_switch" pop_switch ;
    (* test "pop" pop ; *)
  ]
