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

let card_ty = ez_t_record [
    ("card_owner" , t_address ()) ;
  ] ()

let card_ez owner = card (e_a_empty_address owner)

let make_cards assoc_lst =
  let card_id_ty = t_nat () in
  e_a_empty_map assoc_lst card_id_ty card_ty

let card_pattern (coeff , qtt) =
  ez_e_a_empty_record [
    ("coefficient" , coeff) ;
    ("quantity" , qtt) ;
  ]

let card_pattern_ty =
  ez_t_record [
    ("coefficient" , t_tez ()) ;
    ("quantity" , t_nat ()) ;
  ] ()

let card_pattern_ez (coeff , qtt) =
  card_pattern (e_a_empty_tez coeff , e_a_empty_nat qtt)

let make_card_patterns lst =
  let card_pattern_id_ty = t_nat () in
  let assoc_lst = List.mapi (fun i x -> (e_a_empty_nat i , x)) lst in
  e_a_empty_map assoc_lst card_pattern_id_ty card_pattern_ty

let storage cards_patterns cards next_id =
  ez_e_a_empty_record [
    ("cards" , cards) ;
    ("card_patterns" , cards_patterns) ;
    ("next_id" , next_id) ;
  ]

let storage_ez cps cs next_id =
  storage (make_card_patterns cps) (make_cards cs) (e_a_empty_nat next_id)

let cards_ez owner n =
  List.mapi (fun i x -> (e_a_empty_nat i , x))
  @@ List.map card_ez
  @@ List.map (Function.constant owner)
  @@ List.range n

let first_owner =
  let open Tezos_utils.Memory_proto_alpha in
  let id = List.nth dummy_environment.identities 0 in
  let kt = id.implicit_contract in
  Alpha_context.Contract.to_b58check kt

let second_owner =
  let open Tezos_utils.Memory_proto_alpha in
  let id = List.nth dummy_environment.identities 1 in
  let kt = id.implicit_contract in
  Alpha_context.Contract.to_b58check kt

let basic a b cards next_id =
  let card_patterns = List.map card_pattern_ez [
    (100 , a) ;
    (20 , b) ;
  ] in
  storage_ez card_patterns cards next_id

let buy () =
  let%bind program = get_program () in
  let aux n =
    let open AST_Typed.Combinators in
    let input =
      let buy_action = ez_e_a_empty_record [
          ("card_to_buy" , e_a_empty_nat 0) ;
        ] in
      let storage = basic 100 1000 (cards_ez first_owner n) (2 * n) in
      e_a_empty_pair buy_action storage
    in
    let expected =
      let ops = e_a_empty_list [] (t_operation ()) in
      let storage =
        let cards =
          cards_ez first_owner n @
          [(e_a_empty_nat (2 * n) , card (e_a_empty_address second_owner))]
        in
        basic 101 1000 cards ((2 * n) + 1) in
      e_a_empty_pair ops storage
    in
    let%bind amount =
      trace_option (simple_error "getting amount for run") @@
      Tezos_utils.Memory_proto_alpha.Alpha_context.Tez.of_mutez @@ Int64.of_int 10000000000 in
    let%bind result = easy_run_typed ~amount "buy_single" program input in
    Format.printf "\nResult : %a\n" Ast_typed.PP.value result ;
    AST_Typed.assert_value_eq (expected, result)
  in
  let%bind _ = bind_list
    @@ List.map aux
    @@ [2 ; (* 0 ; 7 ; 12 *)] in
  ok ()

let main = "Coase (End to End)", [
    test "buy" buy ;
  ]
