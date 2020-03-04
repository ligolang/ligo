open Trace
open Test_helpers

let type_file f =
  let%bind simplified  = Ligo.Compile.Of_source.compile f (Syntax_name "cameligo") in
  let%bind typed,state = Ligo.Compile.Of_simplified.compile simplified in
  ok @@ (typed,state)

let get_program =
  let s = ref None in
  fun () -> match !s with
    | Some s -> ok s
    | None -> (
        let%bind (program , state) = type_file "./contracts/vote.mligo" in
        s := Some (program , state) ;
        ok (program , state)
      )

open Ast_simplified

let init_storage name = e_record_ez [
    ("title" , e_string name) ;
    ("yea", e_nat 0) ;
    ("nay", e_nat 0) ;
    ("voters" , e_typed_set [] t_address) ;
    ("start_time" , e_timestamp 0) ;
    ("finish_time" , e_timestamp 1000000000) ;
  ]

let reset title start_time finish_time =
  let reset_action = e_record_ez [
      ("title" , e_string title) ;
      ("start_time" , e_timestamp start_time) ;
      ("finish_time" , e_timestamp finish_time)]
  in e_constructor "Reset" reset_action

let yea = e_constructor "Vote" (e_constructor "Yea" (e_unit ()))

let init_vote () =
  let%bind (program , _) = get_program () in
  let%bind result =
    Test_helpers.run_typed_program_with_simplified_input
      program "main" (e_pair yea (init_storage "basic")) in
  let%bind (_, storage) = extract_pair result in
  let%bind storage' = extract_record storage in
(*  let votes = List.assoc (Label "voters") storage' in
  let%bind votes' = extract_map votes in *)
  let yea = List.assoc (Label "yea") storage' in
  let%bind () = Ast_simplified.Misc.assert_value_eq (yea, e_nat 1) in
  ok ()

let main = test_suite "Vote" [
    test "type" init_vote ;
  ]
