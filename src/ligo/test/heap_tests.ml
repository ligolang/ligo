open Trace
open Ligo
open Test_helpers

let get_program =
  let s = ref None in
  fun () -> match !s with
    | Some s -> ok s
    | None -> (
        let%bind program = type_file "./contracts/heap.ligo" in
        s := Some program ;
        ok program
      )


let a_heap_ez ?value_type (content:(int * AST_Typed.ae) list) =
  let open AST_Typed.Combinators in
  let content =
    let aux = fun (x, y) -> e_a_int x, y in
    List.map aux content in
  let value_type = match value_type, content with
    | None, hd :: _ -> (snd hd).type_annotation
    | Some s, _ -> s
    | _ -> raise (Failure "no value type and heap empty when building heap") in
  e_a_map content (t_int ()) value_type

let ez lst =
  let open AST_Typed.Combinators in
  let value_type = t_pair
      (t_int ())
      (t_string ())
      ()
  in
  let lst' =
    let aux (i, (j, s)) =
      (i, e_a_pair (e_a_int j) (e_a_string s)) in
    List.map aux lst in
  a_heap_ez ~value_type lst'

let dummy n =
  ez List.(
    map (fun n -> (n, (n, string_of_int n))) @@
    range n
  )

let is_empty () : unit result =
  let%bind program = get_program () in
  let aux n =
    let open AST_Typed.Combinators in
    let input = dummy n in
    let%bind result = easy_run_typed "is_empty" program input in
    let expected = e_a_bool (n = 0) in
    AST_Typed.assert_value_eq (expected, result)
  in
  let%bind _ = bind_list
    @@ List.map aux
    @@ [0 ; 2 ; 7 ; 12] in
  ok ()


let main = "Heap (End to End)", [
    test "is_empty" is_empty ;
  ]
