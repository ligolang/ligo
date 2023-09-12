module Requests = Ligo_lsp.Server.Requests
open Alcotest_extras
open Lsp_helpers
open Cst_shared.Fold
open Cst_cameligo.Fold
open Simple_utils.Utils

type ('a, 'b) cst_fold_test =
  { file_path : string
  ; expected : 'b
  ; accumulator : 'b
  ; fold_function : 'b -> 'a -> 'b
  ; map_function : some_node -> 'a fold_control
  ; testable : 'b Alcotest.testable
  }

let get_cst_fold
    (type a b)
    ({ file_path; expected; accumulator; fold_function; map_function; testable } :
      (a, b) cst_fold_test)
    : unit Alcotest.test_case
  =
  Alcotest.test_case file_path `Quick
  @@ fun () ->
  let contents = In_channel.read_all file_path in
  let cst =
    Dialect_cst.get_cst
      ~strict:false
      ~file:(Path.from_relative file_path)
      Syntax_types.CameLIGO
      contents
  in
  match cst with
  | Ok (CameLIGO cst) ->
    let result = Cst_cameligo.Fold.fold_cst accumulator fold_function map_function cst in
    check testable "List of list sizes is incorrect" expected result
  | Error e -> fail e
  | _ -> fail "FATAL ERROR: can't get CST"


let length_of_lists_but_not_in_modules : some_node -> int fold_control =
 fun (Some_node (x, b)) ->
  match b with
  | S_list_ S_expr -> Continue (List.length @@ sepseq_to_list x.value.inside)
  | S_module_decl -> Stop
  | _ -> Skip


let tokens_with_zero_size : some_node -> string option fold_control =
 fun (Some_node (x, b)) ->
  match b with
  | S_eof -> Stop
  | S_wrap S_lexeme ->
    if x#region#start#point_num = x#region#stop#point_num
    then Continue (Some x#payload)
    else Skip
  | S_wrap (S_tuple_2 (S_lexeme, _)) ->
    if x#region#start#point_num = x#region#stop#point_num
    then Continue (Some (fst x#payload))
    else Skip
  | S_wrap _ -> Last None
  | _ -> Skip


let test1 =
  { file_path = "contracts/lsp/fold_lists.mligo"
  ; expected = [ 0; 5; 3; 0; 3; 3; 5 ] (* Reversed because of fold *)
  ; accumulator = []
  ; fold_function = Fun.flip List.cons
  ; map_function = length_of_lists_but_not_in_modules
  ; testable = Alcotest.list Alcotest.int
  }


let test2 =
  { file_path = "contracts/lsp/missing_a_lot.mligo"
  ; expected =
      Some [ "ghost_ident"; "in"; "ghost_ident"; "="; "=" ] (* Reversed because of fold *)
  ; accumulator = Some []
  ; fold_function =
      (fun l x -> Option.bind l ~f:(fun l -> Option.bind x ~f:(fun x -> Some (x :: l))))
  ; map_function = tokens_with_zero_size
  ; testable = Alcotest.option @@ Alcotest.list Alcotest.string
  }


let tests = "cst fold", [ get_cst_fold test1; get_cst_fold test2 ]
