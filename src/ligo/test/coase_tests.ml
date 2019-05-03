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

open Ast_typed

let card owner =
  ez_e_a_empty_record [
    ("card_owner" , owner) ;
  ]

let card_ez owner = card (e_a_empty_address owner)

let make_cards lst =
  let card_id_ty = t_nat () in
  let card_ty =
    ez_t_record [
      ("card_owner" , t_address ()) ;
    ] () in
  let assoc_lst = List.mapi (fun i x -> (e_a_empty_nat i , x)) lst in
  e_a_empty_map assoc_lst card_id_ty card_ty

let card_pattern (coeff , qtt , last) =
    ez_e_a_empty_record [
    ("coefficient" , coeff) ;
    ("quantity" , qtt) ;
    ("last" , last) ;
  ]

let card_pattern_ez (coeff , qtt , last) =
  card_pattern (e_a_empty_tez coeff , e_a_empty_nat qtt , e_a_empty_nat last)

let make_card_patterns lst =
  let card_pattern_id_ty = t_nat () in
  let card_pattern_ty =
    ez_t_record [
      ("coefficient" , t_tez ()) ;
      ("quantity" , t_nat ()) ;
      ("last_id" , t_nat ()) ;
    ] () in
  let assoc_lst = List.mapi (fun i x -> (e_a_empty_nat i , x)) lst in
  e_a_empty_map assoc_lst card_pattern_id_ty card_pattern_ty

let storage cards_patterns cards =
  ez_e_a_empty_record [
    ("cards" , cards) ;
    ("card_patterns" , cards_patterns) ;
  ]

let storage_ez cps cs =
  storage (make_card_patterns cps) (make_cards cs)

let basic n =
  let card_patterns = List.map card_pattern_ez [
    (100 , 100 , 150) ;
    (20 , 1000 , 2000) ;
  ] in
  let owner =
    let open Tezos_utils.Memory_proto_alpha in
    let id = List.hd dummy_environment.identities in
    let kt = id.implicit_contract in
    Alpha_context.Contract.to_b58check kt in
  let cards =
    List.map card_ez
    @@ List.map (Function.constant owner)
    @@ List.range n in
  storage (make_card_patterns card_patterns) (make_cards cards)

let buy () =
  let%bind program = get_program () in
  let aux n =
    let open AST_Typed.Combinators in
    let input =
      let card_pattern_id = ez_e_a_empty_record [
          ("card_to_buy" , e_a_empty_nat 0) ;
        ] in
      let storage = basic n in
      e_a_empty_pair card_pattern_id storage
    in
    let%bind amount =
      trace_option (simple_error "getting amount for run") @@
      Tezos_utils.Memory_proto_alpha.Alpha_context.Tez.of_mutez @@ Int64.of_int 10000000000 in
    let%bind result = easy_run_typed ~amount "buy_single" program input in
    Format.printf "\nResult : %a\n" Ast_typed.PP.value result ;
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
