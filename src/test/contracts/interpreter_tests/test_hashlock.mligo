#import "../hashlock.mligo" "Hashlock"

module Test = Test.Next
module State = Test.State
module Typed_address = Test.Typed_address
module Account = Test.Account
module Contract = Test.Contract
module Michelson = Test.Michelson
module Originate = Test.Originate

let assert = Test.Assert.assert
let failwith = Test.Assert.failwith
let michelson_eq = Test.Compare.eq

let test_commit =
  let () = State.reset_at (0 : timestamp) 10n ([] : tez list) in
  let first_committer = Account.address 0 in
  let hashable = [%bytes "hello world"] in
  let packed_sender = Bytes.pack first_committer in
  let salted_hash = Crypto.sha256 (Bytes.concat hashable packed_sender) in
  let pre_commits = (Big_map.empty : Hashlock.commit_set) in
  let init_storage = { hashed = Crypto.sha256 hashable ; unused = true ; commits = pre_commits } in
  // Bake in order to get the address, so time has passed by 10 seconds (1 block)
  let {taddr = typed_addr; code = _; size = _} = Originate.from_file "../hashlock.mligo" init_storage 0tez in
  let contr = Typed_address.to_contract typed_addr in
  let parameter = Commit salted_hash in
  let () = State.set_source first_committer in
  // 86_400 is added to the date for commits in hashlock.mligo (1 day in seconds)
  (* Blocks time have been reduced from 10s to 8s:
https://gitlab.com/tezos/tezos/-/commit/5a3caeda3b165e39d5c942dc9c63d86cff00a9c1#12306a7cbb207399c3bdb11ffc2b9451996a9961
https://gitlab.com/tezos/tezos/-/merge_requests/12716
  *)
  let lock_time = Tezos.get_now () + 8 + 86_400 in
  let _ = Contract.transfer_exn contr parameter 0tez in
  let new_storage = Typed_address.get_storage typed_addr in
  let commit = { date = lock_time ; salted_hash = salted_hash } in
  let post_commits = Big_map.add first_committer commit (Big_map.empty : Hashlock.commit_set) in
  let post_storage = { hashed = Crypto.sha256 hashable ; unused = true ; commits = post_commits } in
  assert (new_storage = post_storage)

let test_reveal_no_commit =
  let () = State.reset_at (0 : timestamp) 10n ([] : tez list) in
  let first_committer = Account.address 0 in
  let empty_message = fun (_ : unit) -> ([] : operation list) in
  let reveal = { hashable = [%bytes "hello world"] ; message = empty_message } in
  let hashable = [%bytes "hello world"] in
  let pre_commits = (Big_map.empty : Hashlock.commit_set) in
  let init_storage = { hashed = Crypto.sha256 hashable ; unused = true ; commits = pre_commits } in
  let {taddr = typed_addr; code = _; size = _} = Originate.from_file "../hashlock.mligo" init_storage 0tez in
  let contr = Typed_address.to_contract typed_addr in
  let parameter = Reveal reveal in
  let () = State.set_source first_committer in
  match Contract.transfer contr parameter 0tez with
  | Success _ -> failwith "Transaction should fail"
  | Fail (Rejected (a, _)) -> assert (michelson_eq a (Michelson.eval "You have not made a commitment to hash against yet."))
  | Fail _ -> failwith "Transaction should fail with rejection"

let test_reveal_young_commit =
  let () = State.reset_at (3_600 : timestamp) 10n ([] : tez list) in
  let first_committer = Account.address 0 in
  let packed_sender = Bytes.pack first_committer in
  let hashable = [%bytes "hello world"] in
  let salted_hash = Crypto.sha256 (Bytes.concat hashable packed_sender) in
  let lock_time = (86_400 : timestamp) in
  let commit = { date = lock_time ; salted_hash = salted_hash } in
  let commits = Big_map.add first_committer commit (Big_map.empty : Hashlock.commit_set) in
  let init_storage = { hashed = Crypto.sha256 hashable ; unused = true ; commits = commits } in
  let {taddr = typed_addr; code = _; size = _} = Originate.from_file "../hashlock.mligo" init_storage 0tez in
  let contr = Typed_address.to_contract typed_addr in
  let empty_message = fun (_ : unit) -> ([] : operation list) in
  let reveal = { hashable = hashable ; message = empty_message } in
  let parameter = Reveal reveal in
  let () = State.set_source first_committer in
  match Contract.transfer contr parameter 0tez with
  | Success _ -> failwith "Transaction should fail"
  | Fail (Rejected (a, _)) -> assert (michelson_eq a (Michelson.eval "It has not been 24 hours since your commit yet."))
  | Fail _ -> failwith "Transaction should fail with rejection"

let test_reveal_breaks_commit =
  let () = State.reset_at (86_400 : timestamp) 10n ([] : tez list) in
  let first_committer = Account.address 0 in
  let packed_sender = Bytes.pack first_committer in
  let hashable = [%bytes "hello world"] in
  let bad_hashable = [%bytes "hello"] in
  let salted_hash = Crypto.sha256 (Bytes.concat bad_hashable packed_sender) in
  let lock_time = (86_400 : timestamp) in
  let commit = { date = lock_time ; salted_hash = salted_hash } in
  let commits = Big_map.add first_committer commit (Big_map.empty : Hashlock.commit_set) in
  let init_storage = { hashed = Crypto.sha256 hashable ; unused = true ; commits = commits } in
  let {taddr = typed_addr; code = _; size = _} = Originate.from_file "../hashlock.mligo" init_storage 0tez in
  let contr = Typed_address.to_contract typed_addr in
  let empty_message = fun (_ : unit) -> ([] : operation list) in
  let reveal = { hashable = hashable ; message = empty_message } in
  let parameter = Reveal reveal in
  let () = State.set_source first_committer in
  match Contract.transfer contr parameter 0tez with
  | Success _ -> failwith "Transaction should fail"
  | Fail (Rejected (a, _)) -> assert (michelson_eq a (Michelson.eval "This reveal does not match your commitment."))
  | Fail _ -> failwith "Transaction should fail with rejection"

let test_reveal_wrong_commit =
  let () = State.reset_at (86_400 : timestamp) 10n ([] : tez list) in
  let first_committer = Account.address 0 in
  let packed_sender = Bytes.pack first_committer in
  let bad_hashable = [%bytes "hello"] in
  let hashable = [%bytes "hello world"] in
  let salted_hash = Crypto.sha256 (Bytes.concat bad_hashable packed_sender) in
  let lock_time = (86_400 : timestamp) in
  let commit = { date = lock_time ; salted_hash = salted_hash } in
  let commits = Big_map.add first_committer commit (Big_map.empty : Hashlock.commit_set) in
  let init_storage = { hashed = Crypto.sha256 hashable ; unused = true ; commits = commits } in
  let {taddr = typed_addr; code = _; size = _} = Originate.from_file "../hashlock.mligo" init_storage 0tez in
  let contr = Typed_address.to_contract typed_addr in
  let empty_message = fun (_ : unit) -> ([] : operation list) in
  let reveal = { hashable = bad_hashable ; message = empty_message } in
  let parameter = Reveal reveal in
  let () = State.set_source first_committer in
  match Contract.transfer contr parameter 0tez with
  | Success _ -> failwith "Transaction should fail"
  | Fail (Rejected (a, _)) -> assert (michelson_eq a (Michelson.eval "Your commitment did not match the storage hash."))
  | Fail _ -> failwith "Transaction should fail with rejection"

let test_reveal_no_reuse =
  let () = State.reset_at (86_400 : timestamp) 10n ([] : tez list) in
  let first_committer = Account.address 0 in
  let packed_sender = Bytes.pack first_committer in
  let bad_hashable = [%bytes "hello"] in
  let hashable = [%bytes "hello world"] in
  let salted_hash = Crypto.sha256 (Bytes.concat bad_hashable packed_sender) in
  let lock_time = (86_400 : timestamp) in
  let commit = { date = lock_time ; salted_hash = salted_hash } in
  let commits = Big_map.add first_committer commit (Big_map.empty : Hashlock.commit_set) in
  let init_storage = { hashed = Crypto.sha256 hashable ; unused = false ; commits = commits } in
  let {taddr = typed_addr; code = _; size = _} = Originate.from_file "../hashlock.mligo" init_storage 0tez in
  let contr = Typed_address.to_contract typed_addr in
  let empty_message = fun (_ : unit) -> ([] : operation list) in
  let reveal = { hashable = bad_hashable ; message = empty_message } in
  let parameter = Reveal reveal in
  let () = State.set_source first_committer in
  match Contract.transfer contr parameter 0tez with
  | Success _ -> failwith "Transaction should fail"
  | Fail (Rejected (a, _)) -> assert (michelson_eq a (Michelson.eval "This contract has already been used."))
  | Fail _ -> failwith "Transaction should fail with rejection"

let test_reveal =
  let () = State.reset_at (86_400 : timestamp) 10n ([] : tez list) in
  let first_committer = Account.address 0 in
  let packed_sender = Bytes.pack first_committer in
  let hashable = [%bytes "hello world"] in
  let salted_hash = Crypto.sha256 (Bytes.concat hashable packed_sender) in
  let lock_time = (86_400 : timestamp) in
  let commit = { date = lock_time ; salted_hash = salted_hash } in
  let commits = Big_map.add first_committer commit (Big_map.empty : Hashlock.commit_set) in
  let init_storage = { hashed = Crypto.sha256 hashable ; unused = true ; commits = commits } in
  let {taddr = typed_addr; code = _; size = _} = Originate.from_file "../hashlock.mligo" init_storage 0tez in
  let contr = Typed_address.to_contract typed_addr in
  let empty_message = fun (_ : unit) -> ([] : operation list) in
  let reveal = { hashable = hashable ; message = empty_message } in
  let parameter = Reveal reveal in
  let () = State.set_source first_committer in
  let _ = Contract.transfer_exn contr parameter 0tez in
  let new_storage = Typed_address.get_storage typed_addr in
  let commit = { date = lock_time ; salted_hash = salted_hash } in
  let post_commits = Big_map.add first_committer commit (Big_map.empty : Hashlock.commit_set) in
  let post_storage = { hashed = Crypto.sha256 hashable ; unused = false ; commits = post_commits } in
  assert (new_storage = post_storage)
