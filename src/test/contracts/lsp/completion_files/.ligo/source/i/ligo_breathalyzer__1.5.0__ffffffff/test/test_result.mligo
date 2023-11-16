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

#import "../lib/lib.mligo" "B"

let case_result_and_then_1 =
  B.Model.case
    "and_then"
    "on two passed values, it should passed with the sum of gas consumption"
    (fun (_: B.Logger.level) ->
      let expected = B.Result.succeed_with 10n in
      let computed =
        B.Result.and_then
          (B.Result.succeed_with 3n)
          (B.Result.succeed_with 7n) in
          B.Assert.is_true "should be equal" (expected = computed))

let case_result_and_then_2 =
  B.Model.case
    "and_then"
    "when there is one failure, it should return it"
    (fun (_: B.Logger.level) ->
      let expected = B.Result.fail_with "kaboom" in
      let computed = B.Result.and_then (B.Result.succeed_with 3n) (expected) in
      B.Assert.is_true "should be equal" (expected = computed))

let case_result_and_then_3 =
  B.Model.case
    "and_then"
    "when there is one failure, it should return it"
    (fun (_: B.Logger.level) ->
      let expected = B.Result.fail_with "kaboom" in
      let computed = B.Result.and_then (expected) (B.Result.succeed_with 3n) in
      B.Assert.is_true "should be equal" (expected = computed))

let case_result_and_then_4 =
  B.Model.case
    "and_then"
    "when there are two failures, it should merge them"
    (fun (_: B.Logger.level) ->
      let expected = Failed [Message "1"; Message "2"] in
      let computed = B.Result.and_then (B.Result.fail_with "1") (B.Result.fail_with "2") in
      B.Assert.is_true "should be equal" (expected = computed))

let case_result_and_then_lazy_1 =
  B.Model.case
    "and_then_lazy"
    "when the first is failure, it should not compute the second one"
    (fun (_: B.Logger.level) ->
      let expected = B.Result.fail_with "kaboom" in
      let computed = B.Result.and_then_lazy
        (expected)
        (fun () ->
          let () = failwith "boum" in
          B.Result.succeed_with 3n)
      in
      B.Assert.is_true "should be equal" (expected = computed))

let case_result_and_then_lazy_2 =
  B.Model.case
    "and_then_lazy"
    "when there are two failures, it should compute the second one and merge them"
    (fun (_: B.Logger.level) ->
       let expected = Failed [Message "1"] in
       let computed = B.Result.and_then_lazy
          (B.Result.fail_with "1")
          (fun () -> (B.Result.fail_with "2"))
        in
        B.Assert.is_true "should be equal" (expected = computed))


let case_result_reduce_1 =
  B.Model.case
    "reduce"
    "reducing an empty list should produce a succeed without gas consumption"
    (fun (_: B.Logger.level) ->
       let expected = B.Result.succeed in
       let computed = B.Result.reduce ([] : B.Result.result list) in
       B.Assert.is_true "should be equal" (expected = computed))

let case_result_reduce_2 =
  B.Model.case
    "reduce"
    "reducing a list with only succeed values should produce a succeed with the sum of gas consumptions"
    (fun (_: B.Logger.level) ->
      let expected = B.Result.succeed_with 10n in
      let computed =
      B.Result.reduce [
        B.Result.succeed; B.Result.succeed; B.Result.succeed;
        B.Result.succeed_with 6n; B.Result.succeed;
        B.Result.succeed_with 4n]
      in
      B.Assert.is_true "should be equal" (expected = computed))

let case_result_reduce_3 =
  B.Model.case
    "reduce"
    "when there are some errors it should merge them"
    (fun (_: B.Logger.level) ->
      let expected = Failed [Message "kabooum"; Message "aha"] in
      let computed =
        B.Result.reduce [
          B.Result.succeed; B.Result.succeed; B.Result.fail_with "kabooum";
          B.Result.succeed_with 6n; B.Result.succeed;
          B.Result.fail_with "aha"]
      in
       B.Assert.is_true "should be equal" (expected = computed))

let suite =
  B.Model.suite
    "Test suite for the Result Module"
    [ case_result_and_then_1 ;
      case_result_and_then_2 ;
      case_result_and_then_3 ;
      case_result_and_then_4 ;
      case_result_and_then_lazy_1 ;
      case_result_and_then_lazy_2 ;
      case_result_reduce_1 ;
      case_result_reduce_2 ;
      case_result_reduce_3 ]
