module Requests = Ligo_lsp.Server.Requests
open Lsp_test_helpers.Common
open Cst_shared.Fold
open Simple_utils.Utils

type 'node node_witness =
  | CameLIGONode : Cst_cameligo.Fold.some_node node_witness
  | JsLIGONode : Cst_jsligo.Fold.some_node node_witness

type ('a, 'b, 'node) cst_fold_test =
  { file_path : string
  ; accumulator : 'b
  ; fold_function : 'b -> 'a -> 'b
  ; witness : 'node node_witness
  ; map_function : 'node -> 'a fold_control
  ; pp_result : 'b Fmt.t
  }

let get_cst_fold
    (type a b node)
    ({ file_path; accumulator; fold_function; witness; map_function; pp_result } :
      (a, b, node) cst_fold_test)
    : unit
  =
  let file_path = resolve file_path in
  let contents = In_channel.read_all file_path in
  let cst =
    Ligo_api.Dialect_cst.get_cst
      ~strict:false
      ~file:file_path
      ~preprocess_define:[]
      Syntax_types.CameLIGO
      (Caml.Buffer.of_seq (Caml.String.to_seq contents))
  in
  match cst, witness with
  | Ok (CameLIGO cst), CameLIGONode ->
    let result = Cst_cameligo.Fold.fold_cst accumulator fold_function map_function cst in
    Format.printf "%a" pp_result result
  | Ok (JsLIGO cst), JsLIGONode ->
    let result = Cst_jsligo.Fold.fold_cst accumulator fold_function map_function cst in
    Format.printf "%a" pp_result result
  | Error e, _ -> failwith e
  | _ -> failwith "FATAL ERROR: can't get CST"


let length_of_lists_but_not_in_modules : Cst_cameligo.Fold.some_node -> int fold_control =
 fun (Some_node (x, b)) ->
  match b with
  | S_list_ S_expr -> Continue (List.length @@ sepseq_to_list x.value.inside)
  | S_module_decl -> Stop
  | _ -> Skip


let tokens_with_zero_size : Cst_cameligo.Fold.some_node -> string option fold_control =
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


let%expect_test _ =
  get_cst_fold
    { file_path = "contracts/lsp/fold_lists.mligo"
    ; accumulator = []
    ; fold_function = Fun.flip List.cons
    ; witness = CameLIGONode
    ; map_function = length_of_lists_but_not_in_modules
    ; pp_result = Fmt.Dump.list Format.pp_print_int
    };
  [%expect {| [0; 5; 3; 0; 3; 3; 5] |}]

let%expect_test _ =
  get_cst_fold
    { file_path = "contracts/lsp/missing_a_lot.mligo"
    ; accumulator = Some []
    ; fold_function =
        (fun l x -> Option.bind l ~f:(fun l -> Option.bind x ~f:(fun x -> Some (x :: l))))
    ; witness = CameLIGONode
    ; map_function = tokens_with_zero_size
    ; pp_result = Fmt.Dump.option @@ Fmt.Dump.list String.pp
    };
  [%expect
    {|
    Some
      ["\226\154\160ghost_ident\226\152\160"; "in";
       "\226\154\160ghost_ident\226\152\160"; "="; "="] |}]
