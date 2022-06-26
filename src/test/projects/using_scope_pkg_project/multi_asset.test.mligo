#import "@stevenj/generic-fa2/multi_asset.instance.mligo" "MultiAsset"
#import "./helpers/list.mligo" "List_helper"

type ext = MultiAsset.extension
type storage = MultiAsset.storage
type extended_storage = ext storage

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
    ((owner1, 1n), a);
    ((owner2, 2n), b);
    ((owner3, 3n), c);
    ((owner1, 2n), a);
  ])
  in

  let operators  = Big_map.literal ([
    ((owner1, op1), Set.literal [1n; 2n]);
    ((owner2, op1), Set.literal [2n]);
    ((owner3, op1), Set.literal [3n]);
    ((op1   , op3), Set.literal [2n]);
  ])
  in

  let token_metadata = (Big_map.literal [
    (1n, ({token_id=1n;token_info=(Map.empty : (string, bytes) map);} : MultiAsset.TokenMetadata.data));
    (2n, ({token_id=2n;token_info=(Map.empty : (string, bytes) map);} : MultiAsset.TokenMetadata.data));
    (3n, ({token_id=3n;token_info=(Map.empty : (string, bytes) map);} : MultiAsset.TokenMetadata.data));
  ] : MultiAsset.TokenMetadata.t) in

  let initial_storage = {
    metadata = Big_map.literal [
        ("", Bytes.pack("tezos-storage:contents"));
        ("contents", ("": bytes))
    ];
    ledger         = ledger;
    token_metadata = token_metadata;
    operators      = operators;
    extension      = "foo";
  } in

  initial_storage, owners, ops

let assert_balances
  (contract_address : (MultiAsset.parameter, extended_storage) typed_address )
  (a, b, c : (address * nat * nat) * (address * nat * nat) * (address * nat * nat)) =
  let (owner1, token_id_1, balance1) = a in
  let (owner2, token_id_2, balance2) = b in
  let (owner3, token_id_3, balance3) = c in
  let storage = Test.get_storage contract_address in
  let ledger = storage.ledger in
  let () = match (Big_map.find_opt (owner1, token_id_1) ledger) with
    Some amt -> assert (amt = balance1)
  | None -> failwith "incorret address"
  in
  let () = match (Big_map.find_opt (owner2, token_id_2) ledger) with
    Some amt -> assert (amt = balance2)
  | None -> failwith "incorret address"
  in
  let () = match (Big_map.find_opt (owner3, token_id_3) ledger) with
    Some amt -> assert (amt = balance3)
  | None -> failwith "incorret address"
  in
  ()

let test_atomic_tansfer_success =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; tx=([{to_=owner2;amount=2n;token_id=2n};] : MultiAsset.atomic_trans list)});
  ] : MultiAsset.transfer)
  in
  let () = Test.set_source op1 in
  let (t_addr,_,_) = Test.originate MultiAsset.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let _ = Test.transfer_to_contract_exn contr (Transfer transfer_requests) 0tez in
  let () = assert_balances t_addr ((owner1, 2n, 8n), (owner2, 2n, 12n), (owner3, 3n, 10n)) in
  ()

let test_atomic_tansfer_originate_from_file_success =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; tx=([{to_=owner2;amount=2n;token_id=2n};] : MultiAsset.atomic_trans list)});
  ] : MultiAsset.transfer)
  in
  let () = Test.set_source op1 in
  let f = "@stevenj/generic-fa2/multi_asset.instance.mligo" in
  let v_mich = Test.run (fun (x : extended_storage) -> x) initial_storage in
  let (addr,_,_) = Test.originate_from_file f "main" ([]: string list) v_mich 0tez in
  let t_addr : (MultiAsset.parameter, extended_storage) typed_address = Test.cast_address addr in
  let contr = Test.to_contract t_addr in
  let _ = Test.transfer_to_contract_exn contr (Transfer transfer_requests) 0tez in
  let () = assert_balances t_addr ((owner1, 2n, 8n), (owner2, 2n, 12n), (owner3, 3n, 10n)) in
  ()
