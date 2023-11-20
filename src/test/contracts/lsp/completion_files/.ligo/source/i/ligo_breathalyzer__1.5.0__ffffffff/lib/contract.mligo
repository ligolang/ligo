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

#import "logger.mligo" "Logger"
#import "result.mligo" "Result"

type ('a, 'b) originated = {
  originated_typed_address : ('a, 'b) typed_address
; originated_contract : 'a contract
; originated_address : address
}

(** [originate_module level name module storage quantity] originates the
    smart-contract from the module [module] and provisions it using [quantity]
    and with [storage] as a default storage value. Use the [contract_of] keyword
    to get a module_contract out of a module. Use this function to originate a
    contract with views. *)
let originate
  (type a b)
  (level: Logger.level)
  (name: string)
  (contract: (a, b) module_contract)
  (storage: b)
  (quantity: tez) : (a, b) originated =
  let {addr=typed_address; code=_; size=_} = Test.originate contract storage quantity in
  let contract = Test.to_contract typed_address in
  let address = Tezos.address contract in

  let () =
    Logger.log level ("originated smart contract", name, address, storage, quantity)
  in
  { originated_typed_address = typed_address
  ; originated_contract = contract
  ; originated_address = address }

(** [transfer_to contract parameter amount] transfers amount to an
    originated SC. *)
let transfer_to
    (type a b)
    (originated: (a, b) originated)
    (parameter: a)
    (fund: tez) : Result.result =
  let contract = originated.originated_contract in
  Result.try_with
    (fun () -> Test.transfer_to_contract contract parameter fund)

(** [transfer_with_entrypoint_to contract entrypoint parameter amount] transfers
    [amount] to an originated SC. *)
let transfer_with_entrypoint_to
    (type a b c)
    (originated: (a, b) originated)
    (entrypoint: string)
    (parameter: c)
    (fund: tez) : Result.result =
  let contract = Test.to_entrypoint entrypoint originated.originated_typed_address in
  Result.try_with
    (fun () -> Test.transfer_to_contract contract parameter fund)

(** [call contract parameter] calls [contract] with [parameter] without
    transferring any tez. *)
let call
  (type a b)
  (originated: (a, b) originated)
  (parameter: a) : Result.result =
  transfer_to originated parameter 0tez

(** [storage_of originated_contract] retreives the storage of an originated
    smart-contract. *)
let storage_of
    (type a b)
    (originated: (a, b) originated) : b =
  let typed_address = originated.originated_typed_address in
  Test.get_storage typed_address

(** [balance_of originated_contract] returns the current balance of a
    smart-contract. *)
let balance_of
    (type a b)
    (originated: (a, b) originated) : tez =
  let addr = originated.originated_typed_address in
  Test.get_balance addr
