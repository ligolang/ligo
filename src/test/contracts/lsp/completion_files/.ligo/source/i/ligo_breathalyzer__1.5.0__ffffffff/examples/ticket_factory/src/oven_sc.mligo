(* MIT License

   Copyright (c) 2022 Marigold <contact@marigold.dev>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal in
   the Software without restriction, including without limitation the rights to
   use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
   the Software, and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

#import "mint_sc.mligo" "Mint"

type storage = {
    stored_ticket : bytes ticket option
  ; qty_ticket : nat
  ; owner_address : address
  ; mint_address : address
}
type applied = operation list * storage

let request_mint (mint_address: address) (qty: tez) : operation =
  if qty <= 0tez then failwith "oven_sc: amount should not null"
  else
    let callback : bytes ticket contract = Tezos.self "%oven_retrieve_ticket" in
    let mint_sc : (Mint parameter_of) contract =
      Tezos.get_contract_with_error
        mint_address
        "oven_sc: unable to find mint contract"
    in
    Tezos.transaction (Mint_process_mint callback: Mint parameter_of) qty mint_sc

let retrieve_ticket
    (counter: nat)
    (stored_ticket : bytes ticket option)
    (minted_ticket: bytes ticket) : bytes ticket option * nat =
  match stored_ticket with
  | None ->
     let (_, (_, n)), fresh_ticket = Tezos.read_ticket minted_ticket in
     (Some fresh_ticket, n)
  | Some ticket ->
      let (_, (_, n)), fresh_ticket = Tezos.read_ticket minted_ticket in
      (Tezos.join_tickets (ticket, fresh_ticket), counter + n)

let request_redeem (mint_address: address) (stored_ticket : bytes ticket option) : operation =
  match stored_ticket with
  | None -> failwith "oven_sc: no stored ticket"
  | Some ticket ->
    let (_, (_, qty)), ticket = Tezos.read_ticket ticket in
    if qty <= 0n then failwith "oven_sc: quantity is null"
    else
      let callback : unit contract = Tezos.self "%oven_retrieve_tez" in
      let mint_sc : (Mint parameter_of) contract =
      Tezos.get_contract_with_error
        mint_address
        "oven_sc: unable to find mint contract"
      in
      Tezos.transaction ((Mint_process_redeem (ticket, callback)): Mint parameter_of) 0tez mint_sc

let retrieve_tez (owner_address : address) (retribution: tez) : operation =
  let beneficiary : unit contract =
     Tezos.get_contract_with_error owner_address "oven_sc: unable to find owner"
  in
  Tezos.transaction unit retribution beneficiary

[@entry]
let oven_request_mint
  (_: unit)
  ({owner_address; mint_address; stored_ticket; qty_ticket}: storage) : applied =
  let _ = assert_with_error (Tezos.get_source () = owner_address) "oven_sc: not owner" in
  let quantity = Tezos.get_amount () in
  let operation = request_mint mint_address quantity in
  ([operation], {
      owner_address = owner_address
    ; mint_address = mint_address
    ; stored_ticket = stored_ticket
    ; qty_ticket = qty_ticket} )

[@entry]
let oven_retrieve_ticket
  (minted_ticket: bytes ticket)
  ({owner_address; mint_address; stored_ticket; qty_ticket}: storage) : applied =
  let _ = assert_with_error (Tezos.get_source () = owner_address) "oven_sc: not owner" in
  let (new_ticket, counter) = retrieve_ticket qty_ticket stored_ticket minted_ticket in
  ([], {
    owner_address = owner_address
  ; mint_address = mint_address
  ; stored_ticket = new_ticket
  ; qty_ticket = counter} )

[@entry]
let oven_request_redeem
  (_: unit)
  ({owner_address; mint_address; stored_ticket; qty_ticket = _}: storage) : applied =
  let _ = assert_with_error (Tezos.get_source () = owner_address) "oven_sc: not owner" in
  let operation = request_redeem mint_address stored_ticket in
  ([operation], {
    owner_address = owner_address
  ; mint_address = mint_address
  ; stored_ticket = (None : bytes ticket option)
  ; qty_ticket = 0n })

[@entry]
let oven_retrieve_tez
  (_: unit)
  ({owner_address; mint_address; stored_ticket = _; qty_ticket = _}: storage) : applied =
  let _ = assert_with_error (Tezos.get_source () = owner_address) "oven_sc: not owner" in
  let quantity = Tezos.get_amount () in
  let operation = retrieve_tez owner_address quantity in
  ([operation], {
    owner_address = owner_address
  ; mint_address = mint_address
  ; stored_ticket = (None : bytes ticket option)
  ; qty_ticket = 0n} )
