open Trace
open Test_helpers

let type_file = Ligo.Compile.Of_source.type_file (Syntax_name "cameligo")

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

let init_storage name = ez_e_record [
    ("title" , e_string name) ;
    ("candidates" , e_map [
        (e_string "Yes" , e_int 0) ;
        (e_string "No" , e_int 0) ;
      ]) ;
    ("voters" , e_typed_set [] t_address) ;
    ("beginning_time" , e_timestamp 0) ;
    ("finish_time" , e_timestamp 1000000000) ;
  ]

let init title beginning_time finish_time =
  let init_action = ez_e_record [
      ("title" , e_string title) ;
      ("beginning_time" , e_timestamp beginning_time) ;
      ("finish_time" , e_timestamp finish_time) ;
    ] in
  e_constructor "Init" init_action

let vote str =
  let vote = e_string str in
  e_constructor "Vote" vote

let init_vote () =
  let%bind (program , state) = get_program () in
  let%bind result = Ligo.Run.Of_simplified.run_typed_program program state "main" (e_pair (vote "Yes") (init_storage "basic")) in
  let%bind (_ , storage) = extract_pair result in
  let%bind storage' = extract_record storage in
  let votes = List.assoc "candidates" storage' in
  let%bind votes' = extract_map votes in
  let%bind (_ , yess) =
    trace_option (simple_error "") @@
    List.find_opt (fun (k , _) -> Ast_simplified.Misc.is_value_eq (k , e_string "Yes")) votes' in
  let%bind () = Ast_simplified.Misc.assert_value_eq (yess , e_int 1) in
  ok ()

let main = test_suite "Vote" [
    test "type" init_vote ;
  ]
