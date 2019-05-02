(* Copyright Coase, Inc 2019 *)

open Trace
open Ligo
open Test_helpers

let get_program =
  let s = ref None in
  fun () -> match !s with
    | Some s -> ok s
    | None -> (
        let%bind program = type_file "./contracts/coase.ligo" in
        s := Some program ;
        ok program
      )

let a_heap_ez ?value_type (content:(int * AST_Typed.ae) list) =
  let open AST_Typed.Combinators in
  let content =
    let aux = fun (x, y) -> e_a_empty_nat x, y in
    List.map aux content in
  let value_type = match value_type, content with
    | None, hd :: _ -> (snd hd).type_annotation
    | Some s, _ -> s
    | _ -> raise (Failure "no value type and heap empty when building heap") in
  e_a_empty_map content (t_nat ()) value_type

let ez lst =
  let open AST_Typed.Combinators in
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

let buy () =
  let%bind program = get_program () in
  let aux n =
    let open AST_Typed.Combinators in
    let input = dummy n in
    let%bind result = easy_run_typed "is_empty" program input in
    let expected = e_a_empty_bool (n = 0) in
    AST_Typed.assert_value_eq (expected, result)
  in
  let%bind _ = bind_list
    @@ List.map aux
    @@ [0 ; 2 ; 7 ; 12] in
  ok ()

let main = "Coase (End to End)", [
    test "buy" buy ;
  ]
