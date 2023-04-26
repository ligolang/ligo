#import "../hashlock.mligo" "Hashlock"


let originate storage balance =
  let storage = Test.eval storage in
  let addr, ctr, size = Test.originate_from_file "../hashlock.mligo" "main" [] storage balance in
  ((Test.cast_address addr : (Hashlock.parameter, Hashlock.storage) typed_address), ctr, size)

let test_commit =
  let () = Test.reset_state_at (0 : timestamp) 10n ([] : tez list) in
  let first_committer = Test.nth_bootstrap_account 0 in
  let hashable = [%bytes "hello world"] in
  let packed_sender = Bytes.pack first_committer in
  let salted_hash = Crypto.sha256 (Bytes.concat hashable packed_sender) in
  let pre_commits = (Big_map.empty : Hashlock.commit_set) in
  let init_storage = { hashed = Crypto.sha256 hashable ; unused = true ; commits = pre_commits } in
  let (typed_addr, _, _) = originate init_storage 0tez in
  let contr = Test.to_contract typed_addr in
  let parameter = Commit salted_hash in
  let () = Test.set_source first_committer in
  let lock_time = Tezos.get_now () + 15 + 86_400 in
  let _ = Test.transfer_to_contract_exn contr parameter 0tez in
  let new_storage = Test.get_storage typed_addr in
  let commit = { date = lock_time ; salted_hash = salted_hash } in
  let post_commits = Big_map.add first_committer commit (Big_map.empty : Hashlock.commit_set) in
  let post_storage = { hashed = Crypto.sha256 hashable ; unused = true ; commits = post_commits } in
  Test.assert (new_storage = post_storage)

let test_reveal_no_commit =
  let () = Test.reset_state_at (0 : timestamp) 10n ([] : tez list) in
  let first_committer = Test.nth_bootstrap_account 0 in
  let empty_message = fun (_ : unit) -> ([] : operation list) in
  let reveal = { hashable = [%bytes "hello world"] ; message = empty_message } in
  let hashable = [%bytes "hello world"] in
  let pre_commits = (Big_map.empty : Hashlock.commit_set) in
  let init_storage = { hashed = Crypto.sha256 hashable ; unused = true ; commits = pre_commits } in
  let (typed_addr, _, _) = originate init_storage 0tez in
  let contr = Test.to_contract typed_addr in
  let parameter = Reveal reveal in
  let () = Test.set_source first_committer in
  match Test.transfer_to_contract contr parameter 0tez with
  | Success _ -> failwith "Transaction should fail"
  | Fail (Rejected (a, _)) -> Test.assert (Test.michelson_equal a (Test.eval "You have not made a commitment to hash against yet."))
  | Fail _ -> failwith "Transaction should fail with rejection"
  
let test_reveal_young_commit =
  let () = Test.reset_state_at (3_600 : timestamp) 10n ([] : tez list) in
  let first_committer = Test.nth_bootstrap_account 0 in
  let packed_sender = Bytes.pack first_committer in
  let hashable = [%bytes "hello world"] in
  let salted_hash = Crypto.sha256 (Bytes.concat hashable packed_sender) in
  let lock_time = (86_400 : timestamp) in
  let commit = { date = lock_time ; salted_hash = salted_hash } in
  let commits = Big_map.add first_committer commit (Big_map.empty : Hashlock.commit_set) in
  let init_storage = { hashed = Crypto.sha256 hashable ; unused = true ; commits = commits } in
  let (typed_addr, _, _) = originate init_storage 0tez in
  let contr = Test.to_contract typed_addr in
  let empty_message = fun (_ : unit) -> ([] : operation list) in
  let reveal = { hashable = hashable ; message = empty_message } in
  let parameter = Reveal reveal in
  let () = Test.set_source first_committer in
  match Test.transfer_to_contract contr parameter 0tez with
  | Success _ -> failwith "Transaction should fail"
  | Fail (Rejected (a, _)) -> Test.assert (Test.michelson_equal a (Test.eval "It has not been 24 hours since your commit yet."))
  | Fail _ -> failwith "Transaction should fail with rejection"

let test_reveal_breaks_commit =
  let () = Test.reset_state_at (86_400 : timestamp) 10n ([] : tez list) in
  let first_committer = Test.nth_bootstrap_account 0 in
  let packed_sender = Bytes.pack first_committer in
  let hashable = [%bytes "hello world"] in
  let bad_hashable = [%bytes "hello"] in
  let salted_hash = Crypto.sha256 (Bytes.concat bad_hashable packed_sender) in
  let lock_time = (86_400 : timestamp) in
  let commit = { date = lock_time ; salted_hash = salted_hash } in
  let commits = Big_map.add first_committer commit (Big_map.empty : Hashlock.commit_set) in
  let init_storage = { hashed = Crypto.sha256 hashable ; unused = true ; commits = commits } in
  let (typed_addr, _, _) = originate init_storage 0tez in
  let contr = Test.to_contract typed_addr in
  let empty_message = fun (_ : unit) -> ([] : operation list) in
  let reveal = { hashable = hashable ; message = empty_message } in
  let parameter = Reveal reveal in
  let () = Test.set_source first_committer in
  match Test.transfer_to_contract contr parameter 0tez with
  | Success _ -> failwith "Transaction should fail"
  | Fail (Rejected (a, _)) -> Test.assert (Test.michelson_equal a (Test.eval "This reveal does not match your commitment."))
  | Fail _ -> failwith "Transaction should fail with rejection"

let test_reveal_wrong_commit =
  let () = Test.reset_state_at (86_400 : timestamp) 10n ([] : tez list) in
  let first_committer = Test.nth_bootstrap_account 0 in
  let packed_sender = Bytes.pack first_committer in
  let bad_hashable = [%bytes "hello"] in
  let hashable = [%bytes "hello world"] in
  let salted_hash = Crypto.sha256 (Bytes.concat bad_hashable packed_sender) in
  let lock_time = (86_400 : timestamp) in
  let commit = { date = lock_time ; salted_hash = salted_hash } in
  let commits = Big_map.add first_committer commit (Big_map.empty : Hashlock.commit_set) in
  let init_storage = { hashed = Crypto.sha256 hashable ; unused = true ; commits = commits } in
  let (typed_addr, _, _) = originate init_storage 0tez in
  let contr = Test.to_contract typed_addr in
  let empty_message = fun (_ : unit) -> ([] : operation list) in
  let reveal = { hashable = bad_hashable ; message = empty_message } in
  let parameter = Reveal reveal in
  let () = Test.set_source first_committer in
  match Test.transfer_to_contract contr parameter 0tez with
  | Success _ -> failwith "Transaction should fail"
  | Fail (Rejected (a, _)) -> Test.assert (Test.michelson_equal a (Test.eval "Your commitment did not match the storage hash."))
  | Fail _ -> failwith "Transaction should fail with rejection"

let test_reveal_no_reuse =
  let () = Test.reset_state_at (86_400 : timestamp) 10n ([] : tez list) in
  let first_committer = Test.nth_bootstrap_account 0 in
  let packed_sender = Bytes.pack first_committer in
  let bad_hashable = [%bytes "hello"] in
  let hashable = [%bytes "hello world"] in
  let salted_hash = Crypto.sha256 (Bytes.concat bad_hashable packed_sender) in
  let lock_time = (86_400 : timestamp) in
  let commit = { date = lock_time ; salted_hash = salted_hash } in
  let commits = Big_map.add first_committer commit (Big_map.empty : Hashlock.commit_set) in
  let init_storage = { hashed = Crypto.sha256 hashable ; unused = false ; commits = commits } in
  let (typed_addr, _, _) = originate init_storage 0tez in
  let contr = Test.to_contract typed_addr in
  let empty_message = fun (_ : unit) -> ([] : operation list) in
  let reveal = { hashable = bad_hashable ; message = empty_message } in
  let parameter = Reveal reveal in
  let () = Test.set_source first_committer in
  match Test.transfer_to_contract contr parameter 0tez with
  | Success _ -> failwith "Transaction should fail"
  | Fail (Rejected (a, _)) -> Test.assert (Test.michelson_equal a (Test.eval "This contract has already been used."))
  | Fail _ -> failwith "Transaction should fail with rejection"

let test_reveal =
  let () = Test.reset_state_at (86_400 : timestamp) 10n ([] : tez list) in
  let first_committer = Test.nth_bootstrap_account 0 in
  let packed_sender = Bytes.pack first_committer in
  let hashable = [%bytes "hello world"] in
  let salted_hash = Crypto.sha256 (Bytes.concat hashable packed_sender) in
  let lock_time = (86_400 : timestamp) in
  let commit = { date = lock_time ; salted_hash = salted_hash } in
  let commits = Big_map.add first_committer commit (Big_map.empty : Hashlock.commit_set) in
  let init_storage = { hashed = Crypto.sha256 hashable ; unused = true ; commits = commits } in
  let (typed_addr, _, _) = originate init_storage 0tez in
  let contr = Test.to_contract typed_addr in
  let empty_message = fun (_ : unit) -> ([] : operation list) in
  let reveal = { hashable = hashable ; message = empty_message } in
  let parameter = Reveal reveal in
  let () = Test.set_source first_committer in
  let _ = Test.transfer_to_contract_exn contr parameter 0tez in
  let new_storage = Test.get_storage typed_addr in
  let commit = { date = lock_time ; salted_hash = salted_hash } in
  let post_commits = Big_map.add first_committer commit (Big_map.empty : Hashlock.commit_set) in
  let post_storage = { hashed = Crypto.sha256 hashable ; unused = false ; commits = post_commits } in
  Test.assert (new_storage = post_storage)