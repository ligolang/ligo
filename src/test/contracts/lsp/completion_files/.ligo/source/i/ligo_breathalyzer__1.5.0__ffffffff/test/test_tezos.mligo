(* MIT License

   Copyright (c) 2023 Marigold <contact@marigold.dev>

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

#import "../lib/lib.mligo" "B"
#import "./simple_contract.jsligo" "Simple"

module Other = struct
  type storage = {
    simple: address;
    x: int
  }

  [@entry]
  let default () (storage: storage): operation list * storage =
    match (Tezos.call_view "get_value" () storage.simple: int option) with
      | None -> failwith "Should not happen"
      | Some x -> [], { storage with x = x }

  [@view]
  let view () (storage: storage): int =
    storage.x
end

let (_, (alice, _, _)) = B.Context.init_default ()

let originated level =
  B.Contract.originate level "Simple" (contract_of Simple) 0 0tez

let case_views_1 =
  B.Model.case
    "call_view"
    "succeeds on a contract originated with originate_module"
    (fun (level: B.Logger.level) ->
      let contract = originated level in
      B.Result.reduce [
        B.Context.call_as alice
          contract (Decrement 5);
        B.Context.call_as alice
          contract (Increment 3);
        B.Assert.is_equal
          "should be -2"
          (Tezos.call_view "get_value" () contract.originated_address)
          (Some (-2))
      ])

let case_views_2 =
  B.Model.case
    "call_view"
    "succeeds when a contract uses it on another contract, both originated with originate_module"
    (fun (level: B.Logger.level) ->
      let contract = originated level in
      let initial_storage = { simple = contract.originated_address; x = 0 } in
      let other =
        B.Contract.originate level "Other" (contract_of Other) initial_storage 0tez
      in
      B.Result.reduce [
        B.Context.call_as alice
          contract (Increment 10);
        B.Context.call_as alice
          other (Default ());
        B.Assert.is_equal
          "should be 10"
          (Tezos.call_view "view" () other.originated_address)
          (Some 10)
      ])

let suite =
  B.Model.suite
    "Test suite for Tezos operations"
    [ case_views_1
    ; case_views_2
    ]
