open Simple_utils.Trace
open Test_helpers
open Main_errors

let get_program = get_program "./contracts/vote.mligo"

open Ast_unified

let init_storage name =
  e_record_ez
    ~loc
    [ "title", e_string ~loc name
    ; "yea", e_nat ~loc 0
    ; "nay", e_nat ~loc 0
    ; "voters", e_set ~loc []
    ; "start_time", e_timestamp ~loc 0
    ; "finish_time", e_timestamp ~loc 1000000000
    ]


let yea =
  let element =
    e_applied_constructor
      ~loc
      { constructor = Label.of_string "Yea"; element = e_unit ~loc }
  in
  e_applied_constructor ~loc { constructor = Label.of_string "Vote"; element }


let init_vote ~raise () =
  let program = get_program ~raise () in
  let result =
    Test_helpers.run_typed_program_with_imperative_input
      ~raise
      program
      "main"
      (e_pair ~loc yea (init_storage "basic"))
  in
  let _, storage =
    trace_option ~raise (test_internal __LOC__) @@ Ast_core.extract_pair result
  in
  let storage' =
    trace_option ~raise (test_internal __LOC__) @@ Ast_core.extract_record storage
  in
  (*  let votes = List.assoc (Label "voters") storage' in
  let votes' = extract_map votes in *)
  let yea = List.Assoc.find_exn ~equal:Caml.( = ) storage' (Label "yea") in
  let () =
    trace_option ~raise (test_internal __LOC__)
    @@ Ast_core.Misc.assert_value_eq (yea, Ast_core.e_nat ~loc Z.one)
  in
  ()


let main = test_suite "Vote" [ test_w "type" init_vote ]
