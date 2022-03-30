(** 
   This file implement the TZIP-12 protocol (a.k.a FA2) for single asset on Tezos
   copyright Wulfman Corporation 2021
*)

#import "../common/errors.mligo" "Errors"

module Operators = struct
   type owner    = address
   type operator = address
   type token_id = nat
   type t = (owner, operator set) big_map

(** if transfer policy is Owner_or_operator_transfer *)
   let assert_authorisation (operators : t) (from_ : address) : unit = 
      let sender_ = Tezos.sender in
      if (sender_ = from_) then ()
      else 
      let authorized = match Big_map.find_opt from_ operators with
         Some (a) -> a | None -> Set.empty
      in if Set.mem sender_ authorized then ()
      else failwith Errors.not_operator
(** if transfer policy is Owner_transfer
   let assert_authorisation (operators : t) (from_ : address) : unit = 
      let sender_ = Tezos.sender in
      if (sender_ = from_) then ()
      else failwith Errors.not_owner
*)

(** if transfer policy is No_transfer
   let assert_authorisation (operators : t) (from_ : address) : unit = 
      failwith Errors.no_owner
*)

   let assert_update_permission (owner : owner) : unit =
      assert_with_error (owner = Tezos.sender) "The sender can only manage operators for his own token"
   (** For an administator
      let admin = tz1.... in
      assert_with_error (Tezos.sender = admiin) "Only administrator can manage operators"
   *)

   let add_operator (operators : t) (owner : owner) (operator : operator) : t =
      if owner = operator then operators (* assert_authorisation always allow the owner so this case is not relevant *)
      else
         let () = assert_update_permission owner in
         let auths = match Big_map.find_opt owner operators with
            Some (os) -> os | None -> Set.empty in
         let auths  = Set.add operator auths in
         Big_map.update owner (Some auths) operators
         
   let remove_operator (operators : t) (owner : owner) (operator : operator) : t =
      if owner = operator then operators (* assert_authorisation always allow the owner so this case is not relevant *)
      else
         let () = assert_update_permission owner in
         let auths = match Big_map.find_opt owner operators with
         None -> None | Some (os) ->
            let os = Set.remove operator os in
            if (Set.size os = 0n) then None else Some (os)
         in
         Big_map.update owner auths operators
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
      let () = assert_with_error (tokens >= amount_) Errors.ins_balance in
      let tokens = abs(tokens - amount_) in
      let ledger = update_for_user ledger from_ tokens in
      ledger 

   let increase_token_amount_for_user (ledger : t) (to_   : owner) (amount_ : amount_) : t = 
      let tokens = get_for_user ledger to_ in
      let tokens = tokens + amount_ in
      let ledger = update_for_user ledger to_ tokens in
      ledger 
end

module TokenMetadata = struct
   (**
      This should be initialized at origination, conforming to either 
      TZIP-12 : https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-12/tzip-12.md#token-metadata
      or TZIP-16 : https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-12/tzip-12.md#contract-metadata-tzip-016 
   *)
   type data = {token_id:nat;token_info:(string,bytes)map}
   type t = data
end

module Storage = struct
   type token_id = nat
   type t = {
      ledger : Ledger.t;
      token_metadata : TokenMetadata.t;
      operators : Operators.t;
   }

   let get_amount_for_owner (s:t) (owner : address) = 
      Ledger.get_for_user s.ledger owner

   let set_ledger (s:t) (ledger:Ledger.t) = {s with ledger = ledger}

   let get_operators (s:t) = s.operators
   let set_operators (s:t) (operators:Operators.t) = {s with operators = operators}
end


type storage = Storage.t


(** transfert entrypoint 
*)
type atomic_trans = [@layout:comb] {
   to_      : address;
   amount   : nat;
}

type transfer_from = {
   from_ : address;
   tx    : atomic_trans list
}
type transfer = transfer_from list

let transfer : transfer -> storage -> operation list * storage = 
   fun (t:transfer) (s:storage) -> 
   (* This function process the "tx" list. Since all transfer share the same "from_" address, we use a se *)
   let process_atomic_transfer (from_:address) (ledger, t:Ledger.t * atomic_trans) =
      let {to_;amount=amount_} = t in
      let ()     = Operators.assert_authorisation s.operators from_ in
      let ledger = Ledger.decrease_token_amount_for_user ledger from_ amount_ in
      let ledger = Ledger.increase_token_amount_for_user ledger to_   amount_ in
      ledger
   in
   let process_single_transfer (ledger, t:Ledger.t * transfer_from ) =
      let {from_;tx} = t in
      let ledger     = List.fold_left (process_atomic_transfer from_) ledger tx in
      ledger
   in
   let ledger = List.fold_left process_single_transfer s.ledger t in
   let s = Storage.set_ledger s ledger in
   ([]: operation list),s

(** balance_of entrypoint 
*)
type request = {
   owner    : address;
   token_id : nat;
}

type callback = [@layout:comb] {
   request : request;
   balance : nat;
}

type balance_of = [@layout:comb] {
   requests : request list;
   callback : callback list contract;
}

let balance_of : balance_of -> storage -> operation list * storage = 
   fun (b: balance_of) (s: storage) -> 
   let {requests;callback} = b in
   let get_balance_info (request : request) : callback =
      let {owner;token_id=_token_id} = request in
      let balance_ = Storage.get_amount_for_owner s owner    in
      {request=request;balance=balance_}
   in
   let callback_param = List.map get_balance_info requests in
   let operation = Tezos.transaction callback_param 0tez callback in
   ([operation]: operation list),s

(** update operators entrypoint *)
(**
(list %update_operators
  (or
    (pair %add_operator
      (address %owner)
      (pair
        (address %operator)
        (nat %token_id)
      )
    )
    (pair %remove_operator
      (address %owner)
      (pair
        (address %operator)
        (nat %token_id)
      )
    )
  )
)
*)
type operator = [@layout:comb] {
   owner    : address;
   operator : address;
   token_id : nat; 
}

type unit_update      = Add_operator of operator | Remove_operator of operator
type update_operators = unit_update list
(**
Add or Remove token operators for the specified token owners and token IDs.


The entrypoint accepts a list of update_operator commands. If two different
commands in the list add and remove an operator for the same token owner and
token ID, the last command in the list MUST take effect.


It is possible to update operators for a token owner that does not hold any token
balances yet.


Operator relation is not transitive. If C is an operator of B and if B is an
operator of A, C cannot transfer tokens that are owned by A, on behalf of B.


*)
let update_ops : update_operators -> storage -> operation list * storage = 
   fun (updates: update_operators) (s: storage) -> 
   let update_operator (operators,update : Operators.t * unit_update) = match update with 
      Add_operator    {owner=owner;operator=operator;token_id=_token_id} -> Operators.add_operator    operators owner operator
   |  Remove_operator {owner=owner;operator=operator;token_id=_token_id} -> Operators.remove_operator operators owner operator
   in
   let operators = Storage.get_operators s in
   let operators = List.fold_left update_operator operators updates in
   let s = Storage.set_operators s operators in
   ([]: operation list),s

(** If transfer_policy is  No_transfer or Owner_transfer
let update_ops : update_operators -> storage -> operation list * storage = 
   fun (updates: update_operators) (s: storage) -> 
   let () = failwith Errors.not_supported in
   ([]: operation list),s
*)


type parameter = [@layout:comb] | Transfer of transfer | Balance_of of balance_of | Update_operators of update_operators
let main ((p,s):(parameter * storage)) = match p with
   Transfer         p -> transfer   p s
|  Balance_of       p -> balance_of p s
|  Update_operators p -> update_ops p s
