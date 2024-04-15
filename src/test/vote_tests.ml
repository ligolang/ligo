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


(* Same as get_e_pair *)
let extract_pair (e : Ast_core.expression) =
  match e.expression_content with
  | E_record record ->
    (match Record.to_list record with
    (* TODO: why 1, 0 *)
    | [ (Label ("0", _), a); (Label ("1", _), b) ]
    | [ (Label ("1", _), b); (Label ("0", _), a) ] -> Some (a, b)
    | _ -> None)
  | _ -> None


let extract_record (e : Ast_core.expression) =
  match e.expression_content with
  | E_record lst -> Some (Record.to_list lst)
  | _ -> None


let init_vote ~raise () =
  Lwt_main.run
  @@
  let open Lwt.Let_syntax in
  let program = get_program ~raise () in
  let%map result =
    Test_helpers.run_typed_program_with_imperative_input
      ~raise
      program
      "main"
      (e_pair ~loc yea (init_storage "basic"))
  in
  let _, storage = trace_option ~raise (test_internal __LOC__) @@ extract_pair result in
  let storage' = trace_option ~raise (test_internal __LOC__) @@ extract_record storage in
  (*  let votes = List.assoc (Label "voters") storage' in
  let votes' = extract_map votes in *)
  let yea = List.Assoc.find_exn ~equal:Label.equal storage' (Label.create "yea") in
  let () =
    trace_option ~raise (test_internal __LOC__)
    @@ Ast_core.Misc.assert_value_eq (yea, Ast_core.e_nat ~loc Z.one)
  in
  ()


let main = test_suite "Vote" [ test_w "type" init_vote ]
