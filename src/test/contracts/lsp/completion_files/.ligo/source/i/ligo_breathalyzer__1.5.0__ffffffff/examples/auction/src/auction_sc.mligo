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

type current_leader = {
  current_leader_address: address;
  current_leader_amount: tez;
  start_time: timestamp
}

type storage = current_leader option

let bid (storage: storage) (quantity: tez) (user_address: address) (time: timestamp) : storage =
  if quantity = 0tez then failwith "Amount cannot be null"
  else match storage with
  | None ->
    Some {
      current_leader_address = user_address;
      current_leader_amount = quantity;
      start_time = time
    }
  | Some { current_leader_address; current_leader_amount; start_time } ->
    let () = if current_leader_address = user_address then failwith "Same leader" in
    let () = if current_leader_amount >= quantity then failwith "Amount should be greater" in
    Some {
      current_leader_address = user_address;
      current_leader_amount = quantity;
      start_time = start_time
    }

let claim (storage: storage) (user_address: address) (time: timestamp) : operation * storage =
  let one_day = 86_400 in
  let current_leader = Option.unopt_with_error storage "No bid received" in
  let () = if current_leader.start_time + one_day >= time then
    failwith "Auction is not complete"
  in
  let () = if current_leader.current_leader_address <> user_address then
    failwith "Not the leader"
  in
  (* We don't have any asset to send to the winning bidder, so we just emit an
  event instead. *)
  let op = Tezos.emit "%claim" user_address in
  let new_storage = None in
  (op, new_storage)

[@entry]
let bid () (storage: storage) : operation list * storage =
  let quantity = Tezos.get_amount () in
  let user_address = Tezos.get_sender () in
  let current_time = Tezos.get_now () in
  ([], bid storage quantity user_address current_time)

[@entry]
let claim () (storage: storage) : operation list * storage =
  let user_address = Tezos.get_sender () in
  let current_time = Tezos.get_now () in
  let operation, storage = claim storage user_address current_time in
  ([operation], storage)

[@view]
let is_claimable () (storage: storage) : bool =
  match storage with
    | None -> false
    | Some current_leader ->
      current_leader.start_time + 86_400 < Tezos.get_now ()
