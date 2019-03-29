open Ligo_helpers.Trace
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

let a_heap content size =
  let open AST_Typed.Combinators in
  a_record_ez [
    ("content", content) ;
    ("size", size) ;
  ]

let a_heap_ez ?value_type (content:(int * AST_Typed.ae) list) =
  let open AST_Typed.Combinators in
  let content =
    let aux = fun (x, y) -> a_int x, y in
    List.map aux content in
  let value_type = match value_type, content with
    | None, hd :: _ -> (snd hd).type_annotation
    | Some s, _ -> s
    | _ -> raise (Failure "no value type and heap empty when building heap") in
  a_map content make_t_int value_type

let is_empty () : unit result =
  let%bind program = get_program () in
  let aux n =
    let open AST_Typed.Combinators in
    let input = a_int n in
    let%bind result = easy_run_main_typed program input in
    let%bind result' =
      trace (simple_error "bad result") @@
      get_a_int result in
    Assert.assert_equal_int (3 * n + 2) result'
  in
  let%bind _ = bind_list
    @@ List.map aux
    @@ [0 ; 2 ; 42 ; 163 ; -1] in
  ok ()


let main = "Heap (End to End)", [
    test "is_empty" is_empty ;
  ]
