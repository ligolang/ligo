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

#import "result.mligo" "Result"

let fail_with_message (message: string) (result: Result.result) : Result.result =
  match result with
  | Failed [Execution (Rejected (mp, _))] ->
    let message_mp = Test.compile_value message in
    if Test.michelson_equal mp message_mp then Result.succeed
    else
      let full_message =
        "Expected failure: `"
        ^ message ^ "` but: `"
        ^ (Test.to_string mp)
        ^ "` given"
      in
      Result.fail_with full_message
  | _ -> Result.fail_with ("Expected failure: `" ^ message ^ "`")

let fail_with_value (type a) (value: a) (result: Result.result) : Result.result =
  match result with
  | Failed [Execution (Rejected (mp, _))] ->
    let value_mp = Test.compile_value value in
    if Test.michelson_equal mp value_mp then Result.succeed
    else
      let full_value =
        "Expected failure: `"
        ^ (Test.to_string value) ^ "` but: `"
        ^ (Test.to_string mp)
        ^ "` given"
      in
      Result.fail_with full_value
  | _ -> Result.fail_with ("Expected failure: `" ^ (Test.to_string value) ^ "`")
