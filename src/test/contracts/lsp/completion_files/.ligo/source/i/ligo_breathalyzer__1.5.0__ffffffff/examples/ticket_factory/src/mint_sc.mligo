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

type storage = {
    fixed_payload : bytes
  ; minimal_amount : tez
}
type applied = operation list * storage

let tez_to_nat (xtz: tez) : nat = xtz / 1mutez
let nat_to_tez (x: nat) : tez = x * 1mutez

let create_new_ticket (storage: storage) (qty: tez) : bytes ticket option =
  if qty < storage.minimal_amount then failwith "mint_sc: amount too low"
  else
    let qty_nat = tez_to_nat qty in
    let payload = storage.fixed_payload in
    Tezos.create_ticket payload qty_nat

let process_mint
    (storage: storage)
    (callback: bytes ticket contract)
    (qty: tez) : operation =
  match create_new_ticket storage qty with
  | Some fresh_ticket -> Tezos.transaction fresh_ticket 0tez callback
  | None -> failwith "Ticket creation failure"

let process_redeem
    (storage: storage)
    (self_address: address)
    (ticket: bytes ticket)
    (callback: unit contract) : operation =
  let (addr, (payload, qty)), _burned_ticket = Tezos.read_ticket ticket in
  let () = if qty <= 0n then failwith "mint_sc: invalid amount" in
  let () = if addr <> self_address then failwith "mint_sc: invalid ticketer" in
  let () = if payload <> storage.fixed_payload then failwith "mint_sc: invalid payload" in
  let retribution = nat_to_tez qty in
  Tezos.transaction unit retribution callback

[@entry]
let mint_process_mint (callback: bytes ticket contract) (storage: storage): applied =
  let quantity = Tezos.get_amount () in
  let operation = process_mint storage callback quantity in
  ([operation], storage)

[@entry]
let mint_process_redeem
  (ticket, callback: bytes ticket * unit contract)
  (storage: storage): applied =
  let self_address = Tezos.get_self_address () in
  let operation = process_redeem storage self_address ticket callback in
  ([operation], storage)
