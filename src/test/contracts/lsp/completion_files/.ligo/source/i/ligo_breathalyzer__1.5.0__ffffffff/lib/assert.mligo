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

let fail_with (message: string) =
  Result.fail_with ("Assertion failed: " ^ message)

(** Some common assertions. *)

let is_true (message: string) (value: bool) : Result.result =
  if value then Result.succeed
  else fail_with message

let is_false (message: string) (value: bool) : Result.result =
  is_true message (not value)

let is_equal (type a) (message: string) (left: a) (right: a) =
  let michelson_left = Test.compile_value left in
  let michelson_right = Test.compile_value right in
  if Test.michelson_equal michelson_left michelson_right then Result.succeed
  else
    let full_message =
      message
      ^ ", "
      ^ Test.to_string left
      ^ " is not equal to "
      ^ Test.to_string right
    in
    fail_with full_message

let is_none (type a) (message: string) (value: a option) : Result.result =
  match value with
  | None -> Result.succeed
  | Some _ -> fail_with ("is not [None], " ^ message)

let is_some_and
    (type a)
    (message: string)
    (predicate : a -> Result.result)
    (value: a option) : Result.result =
  match value with
  | None -> fail_with ("is not [Some], " ^ message)
  | Some x -> predicate x

let is_some (type a) (message: string) = is_some_and message (fun (_: a) -> Result.succeed)

let succeeds : Result.result =
  Result.succeed
