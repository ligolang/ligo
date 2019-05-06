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

open Ast_simplified

let card owner =
  ez_e_a_record [
    ("card_owner" , owner) ;
    ("card_pattern" , e_a_nat 0) ;
  ]

let card_ty = t_record_ez [
    ("card_owner" , t_address) ;
    ("card_pattern" , t_nat) ;
  ]

let card_ez owner = card (e_a_address owner)

let make_cards assoc_lst =
  let card_id_ty = t_nat in
  e_a_map assoc_lst card_id_ty card_ty

let card_pattern (coeff , qtt) =
  ez_e_a_record [
    ("coefficient" , coeff) ;
    ("quantity" , qtt) ;
  ]

let card_pattern_ty =
  t_record_ez [
    ("coefficient" , t_tez) ;
    ("quantity" , t_nat) ;
  ]

let card_pattern_ez (coeff , qtt) =
  card_pattern (e_a_tez coeff , e_a_nat qtt)

let make_card_patterns lst =
  let card_pattern_id_ty = t_nat  in
  let assoc_lst = List.mapi (fun i x -> (e_a_nat i , x)) lst in
  e_a_map assoc_lst card_pattern_id_ty card_pattern_ty

let storage cards_patterns cards next_id =
  ez_e_a_record [
    ("cards" , cards) ;
    ("card_patterns" , cards_patterns) ;
    ("next_id" , next_id) ;
  ]

let storage_ez cps cs next_id =
  storage (make_card_patterns cps) (make_cards cs) (e_a_nat next_id)

let cards_ez owner n =
  List.mapi (fun i x -> (e_a_nat i , x))
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
  let%bind () =
    let make_input = fun n ->
      let buy_action = ez_e_a_record [
          ("card_to_buy" , e_a_nat 0) ;
        ] in
      let storage = basic 100 1000 (cards_ez first_owner n) (2 * n) in
      e_a_pair buy_action storage
    in
    let make_expected = fun n ->
      let ops = e_a_list [] t_operation in
      let storage =
        let cards =
          cards_ez first_owner n @
          [(e_a_nat (2 * n) , card (e_a_address second_owner))]
        in
        basic 101 1000 cards ((2 * n) + 1) in
      e_a_pair ops storage
    in
    let%bind () =
      let%bind amount =
        trace_option (simple_error "getting amount for run") @@
        Tezos_utils.Memory_proto_alpha.Alpha_context.Tez.of_mutez @@ Int64.of_int 10000000000 in
      let options = make_options ~amount () in
      expect_eq_n_pos_small ~options program "buy_single" make_input make_expected in
    let%bind () =
      let%bind amount =
        trace_option (simple_error "getting amount for run") @@
        Tezos_utils.Memory_proto_alpha.Alpha_context.Tez.of_mutez @@ Int64.of_int 0 in
      let options = make_options ~amount () in
      trace_strong (simple_error "could buy without money") @@
      Assert.assert_fail
      @@ expect_eq_n_pos_small ~options program "buy_single" make_input make_expected in
    ok ()
  in
  ok ()

let sell () =
  let%bind program = get_program () in
  let%bind () =
    let make_input = fun n ->
      let sell_action = ez_e_a_record [
          ("card_to_sell" , e_a_nat (n - 1)) ;
        ] in
      let storage = basic 100 1000 (cards_ez first_owner n) (2 * n) in
      e_a_pair sell_action storage
    in
    let make_expected = fun n ->
      let ops = e_a_list [] t_operation in
      let storage =
        let cards =
          cards_ez first_owner n @
          [(e_a_nat (2 * n) , card (e_a_address second_owner))]
        in
        basic 101 1000 cards ((2 * n) + 1) in
      e_a_pair ops storage
    in
    let%bind () =
      let amount = Memory_proto_alpha.Alpha_context.Tez.zero in
      let options = make_options ~amount () in
      expect_eq_n_pos_small ~options program "sell_single" make_input make_expected in
    ok ()
  in
  ok ()


let main = "Coase (End to End)", [
    test "buy" buy ;
    (* test "sell" sell ; *)
  ]
