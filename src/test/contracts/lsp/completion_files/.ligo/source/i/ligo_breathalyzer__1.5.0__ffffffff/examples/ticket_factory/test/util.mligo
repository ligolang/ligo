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

#import "../src/mint_sc.mligo" "Mint"
#import "../src/oven_sc.mligo" "Oven"
#import "../../../lib/lib.mligo" "Breath"

type originated = Breath.Contract.originated

let originate_mint (level: Breath.Logger.level) (pl: bytes) (min_amount: tez) () =
  Breath.Contract.originate
    level
    "mint_sc"
    (contract_of Mint)
    { fixed_payload = pl; minimal_amount = min_amount }
    0tez

let originate_oven_with
    (level: Breath.Logger.level)
    (ticket: bytes ticket option)
    (actor: Breath.Context.actor)
    (mint: (Mint parameter_of, Mint.storage) originated)
    () =
  let (fresh_ticket, counter) = match ticket with
    | None -> (None : bytes ticket option), 0n
    | Some t ->
      let (_, (_, qty)), fresh = Tezos.read_ticket t in
      (Some fresh, qty)
  in
  Breath.Contract.originate
    level
    ("oven_sc_" ^ actor.name)
    (contract_of Oven)
    { stored_ticket = fresh_ticket
    ; owner_address = actor.address
    ; mint_address = mint.originated_address
    ; qty_ticket = counter}
    0tez

let originate_oven_with_ticket
    (level: Breath.Logger.level)
    (ticket: bytes ticket)
    (actor: Breath.Context.actor)
    (mint: (Mint parameter_of, Mint.storage) originated)
    () =
  originate_oven_with level (Some ticket) actor mint ()

let originate_oven
    (level: Breath.Logger.level)
    (actor: Breath.Context.actor)
    (mint: (Mint parameter_of, Mint.storage) originated)
    () =
  originate_oven_with level (None: bytes ticket option) actor mint ()

let request_mint (contract: (Oven parameter_of, Oven.storage) originated) (qty: tez) () =
  Breath.Contract.transfer_to contract Oven_request_mint qty

let request_redeem (contract: (Oven parameter_of, Oven.storage) originated) () =
  Breath.Contract.transfer_to contract Oven_request_redeem 0tez

let expected_mint_state
    (contract: (Mint parameter_of, Mint.storage) originated)
    (pl: bytes)
    (ma: tez)
    (current_balance: tez) : Breath.Result.result =
  let storage = Breath.Contract.storage_of contract in
  let balance = Breath.Contract.balance_of contract in
  let pl_expectation = Breath.Assert.is_equal "fixed payload" storage.fixed_payload pl in
  let ma_expectation = Breath.Assert.is_equal "minimal amount" storage.minimal_amount ma in
  let ba_expectation = Breath.Assert.is_equal "balance" balance current_balance in
  Breath.Result.reduce [pl_expectation; ma_expectation; ba_expectation]

let expected_oven_state
    (contract : (Oven parameter_of, Oven.storage) originated)
    (actor : Breath.Context.actor)
    (mint : (Mint parameter_of, Mint.storage) originated)
    (qty : nat) : Breath.Result.result =
  let { stored_ticket = _; owner_address; mint_address; qty_ticket} =
    Breath.Contract.storage_of contract
  in
  let owner_expectation =
    Breath.Assert.is_equal "owner address" owner_address actor.address
  in
  let mint_expectation =
    Breath.Assert.is_equal "mint address" mint_address mint.originated_address
  in
  let ticket_expectation =
    Breath.Assert.is_equal "ticket quantity" qty_ticket qty
  in
  Breath.Result.reduce [
    owner_expectation
  ; mint_expectation
  ; ticket_expectation ]
