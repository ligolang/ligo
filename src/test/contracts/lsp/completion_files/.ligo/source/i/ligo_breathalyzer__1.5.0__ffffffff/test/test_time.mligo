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

(* Simple contract that expects you to wait one minute before two pings *)
module Ping = struct
  type storage = timestamp

  [@entry]
  let ping () (last: storage) : operation list * storage =
    let now = Tezos.get_now () in
    if now > last + 60 then
      [], now
    else
      failwith "come back later"
end

module Exact = struct
  type storage = timestamp

  [@entry]
  let exact () (last: storage) : operation list * storage =
    let now = Tezos.get_now () in
    if last > (0: timestamp) && now <> last + 30 then
      failwith "wrong time"
    else
      [], now
end

let (_, (alice, _, _)) = B.Context.init_default ()

let ping_contract level initial_time =
  B.Contract.originate level "ping" (contract_of Ping) initial_time 0tez

let exact_contract level initial_time =
  B.Contract.originate level "exact" (contract_of Exact) initial_time 0tez

let suite =
  B.Model.suite
    "Test suite for time-related operations"
    [
      B.Model.case
        "wait_for"
        "waits for at least the expected time"
        (fun (level: B.Logger.level) ->
          let contract = ping_contract level (0: timestamp) in

          B.Result.reduce [
            B.Context.call_as alice contract Ping;
            B.Context.wait_for 60n;
            B.Context.call_as alice contract Ping;
            B.Context.wait_for 60n;
            B.Context.call_as alice contract Ping;
          ]);

      B.Model.case
        "ping"
        "does not wait for an arbitrary long time"
        (fun (level: B.Logger.level) ->
          let contract = ping_contract level (0: timestamp) in

          B.Result.reduce [
            B.Context.call_as alice contract Ping;
            B.Context.wait_for 30n;
            B.Expect.fail_with_message
              "come back later"
              (B.Context.call_as alice contract Ping);
          ]);

      B.Model.case
        "wait_for"
        "waiting for the block time makes you miss 1 block, not more"
        (fun (level: B.Logger.level) ->
          let exact_contract = exact_contract level (0: timestamp) in

          (* We cannot just repeat the list [call_as; wait_for] because these
           functions are impure *)
          B.Result.reduce [
            B.Context.call_as alice exact_contract Exact;
            B.Context.wait_for 15n;
            B.Context.call_as alice exact_contract Exact;
            B.Context.wait_for 15n;
            B.Context.call_as alice exact_contract Exact;
            B.Context.wait_for 15n;
            B.Context.call_as alice exact_contract Exact;
            B.Context.wait_for 15n;
            B.Context.call_as alice exact_contract Exact;
            B.Context.wait_for 15n;
            B.Context.call_as alice exact_contract Exact;
            B.Context.wait_for 15n;
            B.Context.call_as alice exact_contract Exact;
          ]);

      B.Model.case
        "wait_for"
        "waiting for less than one block time does not make you miss one block"
        (fun (level: B.Logger.level) ->
          let exact_contract = exact_contract level (0: timestamp) in

          (* We cannot just repeat the list [call_as; wait_for] because these
           functions are impure *)
          B.Result.reduce [
            B.Context.call_as alice exact_contract Exact;
            B.Context.wait_for 1n;
            B.Expect.fail_with_message
              "wrong time"
              (B.Context.call_as alice exact_contract Exact);
          ]);

    ]
