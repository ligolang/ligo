#import "../../lib/fa2/asset/single_asset.mligo" "FA2_single_asset"
#import "./balance_of_callback_contract.mligo" "Callback"

(* Tests for FA2 single asset contract *)

module List_helper = struct 

  let nth_exn (type a) (i: int) (a: a list) : a =
    let rec aux (remaining: a list) (cur: int) : a =
      match remaining with 
       [] -> 
        failwith "Not found in list"
      | hd :: tl -> 
          if cur = i then 
            hd 
          else aux tl (cur + 1)
    in
    aux a 0  

end

let get_initial_storage (a, b, c : nat * nat * nat) = 
  let () = Test.reset_state 6n ([] : tez list) in

  let owner1 = Test.nth_bootstrap_account 0 in 
  let owner2 = Test.nth_bootstrap_account 1 in 
  let owner3 = Test.nth_bootstrap_account 2 in 

  let owners = [owner1; owner2; owner3] in

  let op1 = Test.nth_bootstrap_account 3 in
  let op2 = Test.nth_bootstrap_account 4 in
  let op3 = Test.nth_bootstrap_account 5 in

  let ops = [op1; op2; op3] in

  let ledger = Big_map.literal ([
      (owner1, a);
      (owner2, b);
      (owner3, c);
    ])
  in

  let operators  = Big_map.literal ([
      (owner1, Set.literal [op1]);
      (owner2, Set.literal [op1;op2]);
      (owner3, Set.literal [op1;op3]);
      (op3   , Set.literal [op1;op2]);
    ])
  in
  
  let token_info = (Map.empty: (string, bytes) map) in
  let token_metadata = {
    token_id   = 0n;
    token_info = token_info;
  } in

  let initial_storage = {
      ledger         = ledger;
      token_metadata = token_metadata;
      operators      = operators;
  } in

  initial_storage, owners, ops

let assert_balances 
  (contract_address : (FA2_single_asset.parameter, FA2_single_asset.storage) typed_address ) 
  (a, b, c : (address * nat) * (address * nat) * (address * nat)) = 
  let (owner1, balance1) = a in
  let (owner2, balance2) = b in
  let (owner3, balance3) = c in
  let storage = Test.get_storage contract_address in
  let ledger = storage.ledger in
  let () = match (Big_map.find_opt owner1 ledger) with
    Some amt -> assert (amt = balance1)
  | None -> failwith "incorret address" 
  in
  let () = match (Big_map.find_opt owner2 ledger) with
    Some amt ->  assert (amt = balance2)
  | None -> failwith "incorret address" 
  in
  let () = match (Big_map.find_opt owner3 ledger) with
    Some amt -> assert (amt = balance3)
  | None -> failwith "incorret address" 
  in
  ()

(* Transfer *)

(* 1. transfer successful *)
let test_atomic_tansfer_success =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; tx=([{to_=owner2;amount=2n};{to_=owner3;amount=3n}] : FA2_single_asset.atomic_trans list)});
    ({from_=owner2; tx=([{to_=owner3;amount=2n};{to_=owner1;amount=3n}] : FA2_single_asset.atomic_trans list)});
  ] : FA2_single_asset.transfer)
  in
  let () = Test.set_source op1 in 
  let (t_addr,_,_) = Test.originate FA2_single_asset.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let _ = Test.transfer_to_contract_exn contr (Transfer transfer_requests) 0tez in
  let () = assert_balances t_addr ((owner1, 8n), (owner2, 7n), (owner3, 15n)) in
  ()

(* 2. transfer failure incorrect operator *)
let test_atomic_transfer_failure_not_operator = 
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let transfer_requests = ([
    ({from_=owner1; tx=([{to_=owner2;amount=2n};{to_=owner3;amount=3n}] : FA2_single_asset.atomic_trans list)});
    ({from_=owner2; tx=([{to_=owner3;amount=2n};{to_=owner1;amount=3n}] : FA2_single_asset.atomic_trans list)});
  ] : FA2_single_asset.transfer)
  in
  let () = Test.set_source op3 in 
  let (t_addr,_,_) = Test.originate FA2_single_asset.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let result = Test.transfer_to_contract contr (Transfer transfer_requests) 0tez in
  match result with
    Success _ -> failwith "This test should fail"
  | Fail (Rejected (err, _))  -> assert (Test.michelson_equal err (Test.eval FA2_single_asset.Errors.not_operator))
  | Fail _ -> failwith "invalid test failure"

(* 3. transfer failure insuffient balance *)
let test_atomic_transfer_failure_not_suffient_balance = 
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; tx=([{to_=owner2;amount=20n};{to_=owner3;amount=3n}] : FA2_single_asset.atomic_trans list)});
    ({from_=owner2; tx=([{to_=owner3;amount=2n};{to_=owner1;amount=3n}] : FA2_single_asset.atomic_trans list)});
  ] : FA2_single_asset.transfer)
  in
  let () = Test.set_source op1 in 
  let (t_addr,_,_) = Test.originate FA2_single_asset.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let result = Test.transfer_to_contract contr (Transfer transfer_requests) 0tez in
  match result with
    Success _ -> failwith "This test should fail"
  | Fail (Rejected (err, _))  -> assert (Test.michelson_equal err (Test.eval FA2_single_asset.Errors.ins_balance))
  | Fail _ -> failwith "invalid test failure"

(* 4. transfer successful 0 amount & self transfer *)
let test_atomic_tansfer_success_zero_amount_and_self_transfer =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; tx=([{to_=owner2;amount=0n};{to_=owner3;amount=0n}] : FA2_single_asset.atomic_trans list)});
    ({from_=owner2; tx=([{to_=owner2;amount=2n};] : FA2_single_asset.atomic_trans list)});
  ] : FA2_single_asset.transfer)
  in
  let () = Test.set_source op1 in 
  let (t_addr,_,_) = Test.originate FA2_single_asset.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let _ = Test.transfer_to_contract_exn contr (Transfer transfer_requests) 0tez in
  let () = assert_balances t_addr ((owner1, 10n), (owner2, 10n), (owner3, 10n)) in
  ()

(* 5. transfer failure transitive operators *)
let test_transfer_failure_transitive_operators = 
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op2    = List_helper.nth_exn 1 operators in
  let transfer_requests = ([
      ({from_=owner3; tx=([{to_=owner2;amount=2n};] : FA2_single_asset.atomic_trans list)});
  ] : FA2_single_asset.transfer)
  in
  let () = Test.set_source op2 in 
  let (t_addr,_,_) = Test.originate FA2_single_asset.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let result = Test.transfer_to_contract contr (Transfer transfer_requests) 0tez in
  match result with
    Success _ -> failwith "This test should fail"
  | Fail (Rejected (err, _))  -> assert (Test.michelson_equal err (Test.eval FA2_single_asset.Errors.not_operator))
  | Fail _ -> failwith "invalid test failure"

(* Balance of *)

(* 6. empty balance of + callback with empty response *)
let test_empty_transfer_and_balance_of = 
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let (callback_addr,_,_) = Test.originate Callback.main ([] : nat list) 0tez in
  let callback_contract = Test.to_contract callback_addr in

  let balance_of_requests = ({
    requests = ([] : FA2_single_asset.request list);
    callback = callback_contract;
  } : FA2_single_asset.balance_of) in

  let (t_addr,_,_) = Test.originate FA2_single_asset.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let _ = Test.transfer_to_contract_exn contr (Balance_of balance_of_requests) 0tez in
  
  let callback_storage = Test.get_storage callback_addr in
  assert (callback_storage = ([] : nat list))

(* 7. duplicate balance_of requests *)
let test_balance_of_requests_with_duplicates = 
  let initial_storage, owners, operators = get_initial_storage (10n, 5n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let (callback_addr,_,_) = Test.originate Callback.main ([] : nat list) 0tez in
  let callback_contract = Test.to_contract callback_addr in

  let balance_of_requests = ({
    requests = ([
      {owner=owner1;token_id=0n};
      {owner=owner2;token_id=0n};
      {owner=owner1;token_id=0n};
    ] : FA2_single_asset.request list);
    callback = callback_contract;
  } : FA2_single_asset.balance_of) in

  let (t_addr,_,_) = Test.originate FA2_single_asset.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let _ = Test.transfer_to_contract_exn contr (Balance_of balance_of_requests) 0tez in

  let callback_storage = Test.get_storage callback_addr in
  assert (callback_storage = ([10n; 5n; 10n]))

(* 8. 0 balance if does not hold any tokens (not in ledger) *)
let test_balance_of_0_balance_if_address_does_not_hold_tokens = 
  let initial_storage, owners, operators = get_initial_storage (10n, 5n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let (callback_addr,_,_) = Test.originate Callback.main ([] : nat list) 0tez in
  let callback_contract = Test.to_contract callback_addr in

  let balance_of_requests = ({
    requests = ([
      {owner=owner1;token_id=0n};
      {owner=owner2;token_id=0n};
      {owner=op1;token_id=0n};
    ] : FA2_single_asset.request list);
    callback = callback_contract;
  } : FA2_single_asset.balance_of) in

  let (t_addr,_,_) = Test.originate FA2_single_asset.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let _ = Test.transfer_to_contract_exn contr (Balance_of balance_of_requests) 0tez in

  let callback_storage = Test.get_storage callback_addr in
  assert (callback_storage = ([10n; 5n; 0n]))

(* Update operators *)

(* 9. Remove operator & do transfer - failure *)
let test_update_operator_remove_operator_and_transfer = 
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let (t_addr,_,_) = Test.originate FA2_single_asset.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in

  let () = Test.set_source owner1 in 
  let _ = Test.transfer_to_contract_exn contr 
    (Update_operators ([
      (Remove_operator ({
        owner    = owner1;
        operator = op1;
        token_id = 0n;
      } : FA2_single_asset.operator) : FA2_single_asset.unit_update)
    ] : FA2_single_asset.update_operators)) 0tez in

  let () = Test.set_source op1 in
  let transfer_requests = ([
    ({from_=owner1; tx=([{to_=owner2;amount=0n};{to_=owner3;amount=0n}] : FA2_single_asset.atomic_trans list)});
    ({from_=owner2; tx=([{to_=owner2;amount=2n};] : FA2_single_asset.atomic_trans list)});
  ] : FA2_single_asset.transfer)
  in
  let result = Test.transfer_to_contract contr (Transfer transfer_requests) 0tez in
  match result with
    Success _ -> failwith "This test should fail"
  | Fail (Rejected (err, _))  -> assert (Test.michelson_equal err (Test.eval FA2_single_asset.Errors.not_operator))
  | Fail _ -> failwith "invalid test failure"

(* 10. Add operator & do transfer - success *)
let test_update_operator_add_operator_and_transfer = 
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let (t_addr,_,_) = Test.originate FA2_single_asset.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in

  let () = Test.set_source owner1 in 
  let _ = Test.transfer_to_contract_exn contr 
    (Update_operators ([
      (Add_operator ({
        owner    = owner1;
        operator = op3;
        token_id = 0n;
      } : FA2_single_asset.operator) : FA2_single_asset.unit_update);
    ] : FA2_single_asset.update_operators)) 0tez in

  let () = Test.set_source owner2 in 
  let _ = Test.transfer_to_contract_exn contr 
    (Update_operators ([
      (Add_operator ({
        owner    = owner2;
        operator = op3;
        token_id = 0n;
      } : FA2_single_asset.operator) : FA2_single_asset.unit_update);
    ] : FA2_single_asset.update_operators)) 0tez in

  let () = Test.set_source op3 in
  let transfer_requests = ([
    ({from_=owner1; tx=([{to_=owner2;amount=0n};{to_=owner3;amount=0n}] : FA2_single_asset.atomic_trans list)});
    ({from_=owner2; tx=([{to_=owner2;amount=2n};] : FA2_single_asset.atomic_trans list)});
  ] : FA2_single_asset.transfer)
  in
  let _ = Test.transfer_to_contract_exn contr (Transfer transfer_requests) 0tez in
  ()