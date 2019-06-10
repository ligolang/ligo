open Trace
open Ligo.Run
open Test_helpers

let get_program =
  let s = ref None in
  fun () -> match !s with
    | Some s -> ok s
    | None -> (
        let%bind program = type_file "cameligo" "./contracts/vote.mligo" in
        s := Some program ;
        ok program
      )

open Ast_simplified

let dummy_storage = ez_e_record [
    ("title" , e_string "dummy") ;
    ("candidates" , e_typed_map [] t_string t_int) ;
    ("voters" , e_typed_set [] t_address) ;
    ("beginning_time" , e_timestamp 0) ;
    ("finish_time" , e_timestamp 0) ;
  ]

let init_vote () =
  let%bind _program = get_program () in
  ok ()

let main = test_suite "Vote" [
    (* test "type" init_vote ; *)
  ]
