(**
This file implement the TZIP-12 protocol (a.k.a FA2) for NFT on Tezos
copyright Wulfman Corporation 2021
*)

#import "errors.mligo" "Errors"

type owner    = address
type operator = address
type token_id = nat
type t = ((owner * operator), token_id set) big_map

(** if transfer policy is Owner_or_operator_transfer *)
let assert_authorisation (operators : t) (from_ : address) (token_id : nat) : unit =
  let sender_ = (Tezos.get_sender ()) in
  if (sender_ = from_) then ()
  else
  let authorized = match Big_map.find_opt (from_,sender_) operators with
     Some (a) -> a | None -> Set.empty
  in if Set.mem token_id authorized then ()
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
  assert_with_error (owner = (Tezos.get_sender ())) "The sender can only manage operators for his own token"
(** For an administator
  let admin = tz1.... in
  assert_with_error (Tezos.sender = admiin) "Only administrator can manage operators"
*)

let add_operator (operators : t) (owner : owner) (operator : operator) (token_id : token_id) : t =
  if owner = operator then operators (* assert_authorisation always allow the owner so this case is not relevant *)
  else
     let () = assert_update_permission owner in
     let auth_tokens = match Big_map.find_opt (owner,operator) operators with
        Some (ts) -> ts | None -> Set.empty in
     let auth_tokens  = Set.add token_id auth_tokens in
     Big_map.update (owner,operator) (Some auth_tokens) operators

let remove_operator (operators : t) (owner : owner) (operator : operator) (token_id : token_id) : t =
  if owner = operator then operators (* assert_authorisation always allow the owner so this case is not relevant *)
  else
     let () = assert_update_permission owner in
     let auth_tokens = match Big_map.find_opt (owner,operator) operators with
     None -> None | Some (ts) ->
        let ts = Set.remove token_id ts in
        if (Set.size ts = 0n) then None else Some (ts)
     in
     Big_map.update (owner,operator) auth_tokens operators
