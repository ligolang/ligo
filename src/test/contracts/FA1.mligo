(**
   This file implements the TZIP-5 protocol (a.k.a FA1)
   copyright Wulfman Corporation 2022
*)

module Errors = struct
   let notEnoughBalance = "NotEnoughBalance"
end


module Ledger = struct
   type owner  = address
   type amount_ = nat
   type t = (owner, amount_) big_map

   let get_for_user    (ledger:t) (owner: owner) : amount_ =
      match Big_map.find_opt owner ledger with
         Some (tokens) -> tokens
      |  None          -> 0n

   let update_for_user (ledger:t) (owner: owner) (amount_ : amount_) : t =
      Big_map.update owner (Some amount_) ledger

   let decrease_token_amount_for_user (ledger : t) (from_ : owner) (amount_ : amount_) : t =
      let tokens = get_for_user ledger from_ in
      let () = assert_with_error (tokens >= amount_) Errors.notEnoughBalance in
      let tokens = abs(tokens - amount_) in
      let ledger = update_for_user ledger from_ tokens in
      ledger

   let increase_token_amount_for_user (ledger : t) (to_   : owner) (amount_ : amount_) : t =
      let tokens = get_for_user ledger to_ in
      let tokens = tokens + amount_ in
      let ledger = update_for_user ledger to_ tokens in
      ledger
end

module Storage = struct
   type t = {
      ledger : Ledger.t;
      totalSupply : nat;
      (* Note: memoizing the sum of all participant balance reduce the cost of getTotalSupply entrypoint.
         However, with this pattern the value has to be manually set at origination which can lead to consistency issues.
      *)
   }

   let get_amount_for_owner (s:t) (owner : address) =
      Ledger.get_for_user s.ledger owner

   let get_ledger (s:t) = s.ledger
   let set_ledger (s:t) (ledger:Ledger.t) = {s with ledger = ledger}
end


type storage = Storage.t


(** transfer entrypoint *)
type transfer = address * (address * nat)
let transfer (from_,(to_,value):transfer) (s:storage) =
   let ledger = Storage.get_ledger s in
   let ledger = Ledger.decrease_token_amount_for_user ledger from_ value in
   let ledger = Ledger.increase_token_amount_for_user ledger to_   value in
   let s = Storage.set_ledger s ledger in
   ([]: operation list),s

(** getBalance entrypoint *)
type getBalance = address * nat contract
let getBalance ((owner,callback): getBalance) (s: storage) =
   let balance_ = Storage.get_amount_for_owner s owner in
   let operation = Tezos.transaction balance_ 0tez callback in
   ([operation]: operation list),s


(** getTotalSupply entrypoint *)
type getTotalSupply = unit * nat contract
let getTotalSupply ((),callback : getTotalSupply) (s:storage) =
   let operation = Tezos.transaction s.totalSupply 0tez callback in
   ([operation]: operation list),s


type parameter = Transfer of transfer | GetBalance of getBalance | GetTotalSupply of getTotalSupply
let main (p : parameter) (s : storage) = match p with
   Transfer       p -> transfer       p s
|  GetBalance     p -> getBalance     p s
|  GetTotalSupply p -> getTotalSupply p s
