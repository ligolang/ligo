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

(** The core of the test framework. *)

#import "util.mligo" "Util"

(** An alias for returing the gas consumption of a function *)
type gas = nat

(** A reason can be :
    - a Message, it is an [user-error]
    - an Execution error (ie: the balance is too low).
*)
type reason =
  | Message of string
  | Execution of test_exec_error

(** A test can be succeed, so the result is [Passed n] where [n] is the gas
   consumption of the test. Or [Failed] with a reason (see: [reason]). *)
type result =
  | Passed of gas
  | Failed of reason list

(** [is_succeed result] return [true] if the result is passed, [false] otherwise. *)
let is_succeed (result : result) : bool =
  match result with
  | Passed _ -> true
  | Failed _ -> false

(** Build a [passed] result. *)
let succeed_with (n: gas)  = Passed n

(** Build a [passed] result with a null gas consumption. *)
let succeed = succeed_with 0n

(** Build a [failed] with a given message. *)
let fail_with (m: string) = Failed ([Message m])

(** [and_then_lazy a f] will collapse a and the [f] performing if [a] is [passed].
    If both are passed, it will returns passed with the sum of the gas consumption,
    if there is a failure, it will returns the first one. *)
let and_then_lazy (left: result) (right: unit -> result) : result =
  match left with
  | Failed x -> Failed x
  | Passed x -> begin
    match right () with
    | Passed y -> Passed (x + y)
    | failure -> failure
  end

(** [and_then a b] will collapse two results. It produce an "early-return".
    If [a] and [b] are both succeed, the result will be the sum of the
    gas consumption of [a] and [b], otherwise, it will returns the concat of
    failing result. *)
let and_then (left: result) (right: result) =
  match (left, right) with
  | Passed x, Passed y -> Passed (x + y)
  | Failed x, Failed y -> Failed (Util.concat x y)
  | _, Failed x -> Failed x
  | Failed x, _ -> Failed x

(** [reduce list] will apply [and_then] on each element of a list of result. *)
let reduce (list: result list) : result =
  List.fold_left
    (fun (x, y : result * result) -> and_then x y)
    succeed
    list

(** [try_catch f s e] perform [f ()] and if the execution is succeed, it perform
    [s gas] and if it fails it perform [e error]. *)
let try_catch
    (type a)
    (callback: unit -> test_exec_result)
    (succeed_callback: nat -> a)
    (failed_callback: test_exec_error -> a) : a =
  match callback () with
  | Success gas_value -> succeed_callback gas_value
  | Fail err -> failed_callback err


(** [try_with f] will perform [f] and wrap it into our test result. *)
let try_with (callback: unit -> test_exec_result) : result =
  let execution_failure (err: test_exec_error) =
    Failed [Execution err]
  in
  try_catch callback succeed_with execution_failure

(* Try to render an execution error as a string. *)
let pp_test_exec_error (err: test_exec_error) : string =
  match err with
  | Other reason -> "Other: `" ^ reason ^ "`"
  | Rejected reason -> Test.to_string reason
  | Balance_too_low r ->
    let contract_balance = r.contract_balance in
    let spend_request = r.spend_request in
    let balance = Util.tez_to_string contract_balance in
    let request = Util.tez_to_string spend_request in
    "Balance_too_low {contract_balance = "
     ^ balance
     ^ "; spend_request = "
     ^ request
     ^ "; _}"

(** Try to transform a failure reason into a string. *)
let pp_reason (reason: reason) : string =
  match reason with
  | Message msg -> "Message: `" ^ msg ^ "`"
  | Execution err ->
    let exec_error = pp_test_exec_error err in
    "Execution: `" ^ exec_error ^ "`"

(** Convert a Result into a string in order to be printed. *)
let pp (result: result) : string =
  match result with
  | Passed x ->
    let gas_str = Util.nat_to_string x in
    "Passed with [" ^ gas_str ^ "] of gas consumption"
  | Failed reasons ->
    let list =
      List.fold_left (fun (acc, err : string * reason) ->
        let str = pp_reason err in
        acc ^ " " ^ str ^ ";"
      ) "" reasons
    in
    "Failed with errors [" ^ list ^ "]"
