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

#import "list_ext.mligo" "L"
#import "../../lib/lib.mligo" "Breath"

let empty : int list = []

let case_cons_1 =
  Breath.Model.case
    "cons"
    "cons of a value to an empty list should produce a list of one element"
    (fun (_: Breath.Logger.level) ->
       let expected = [1] in
       let computed = L.cons 1 empty in
       Breath.Assert.is_true "should be equal"(expected = computed))

let case_cons_2 =
  Breath.Model.case
    "cons"
    "cons of a value to a filled list should produce a new list"
    (fun (_: Breath.Logger.level) ->
      let expected = [1; 2; 3; 4] in
      let computed = L.cons 1 [2; 3; 4] in
      Breath.Assert.is_true "should be equal"(expected = computed))

let case_rev_1 =
  Breath.Model.case
    "rev"
    "rev an empty list produce an empty list"
    (fun (_: Breath.Logger.level) ->
      let expected = empty in
      let computed = L.rev expected in
      Breath.Assert.is_true "should be equal"(expected = computed))

let case_rev_2 =
  Breath.Model.case
    "rev"
    "rev a filled list should produce the reversed list"
    (fun (_: Breath.Logger.level) ->
      let expected = [1; 2; 3] in
      let computed = L.rev [3; 2; 1] in
      Breath.Assert.is_true "should be equal"(expected = computed))

let case_concat_1 =
  Breath.Model.case
    "concat"
    "concat two empty list should produce an empty list"
    (fun (_: Breath.Logger.level) ->
      let expected = empty in
      let computed = L.concat expected expected in
      Breath.Assert.is_true "should be equal"(expected = computed))

let case_concat_2 =
  Breath.Model.case
    "concat"
    "concat an empty list to a regular list should produce the same regular list"
    (fun (_: Breath.Logger.level) ->
      let expected = [1] in
      let computed = L.concat empty [1] in
      Breath.Assert.is_true "should be equal"(expected = computed))

let case_concat_3 =
  Breath.Model.case
    "concat"
    "concat a regular list to an empty list should produce the same regular list"
    (fun (_: Breath.Logger.level) ->
      let expected = [1] in
      let computed = L.concat [1] empty in
      Breath.Assert.is_true "should be equal"(expected = computed))

let case_concat_4 =
  Breath.Model.case
    "concat"
    "concat two regular list should produce a new list"
    (fun (_: Breath.Logger.level) ->
      let expected = [1; 2; 3; 4; 5; 6] in
      let computed = L.concat [1; 2; 3] [4; 5; 6] in
      Breath.Assert.is_true "should be equal"(expected = computed))

let () =
  Breath.Model.run_suites Trace [
    Breath.Model.suite "A simple list extension"
    [ case_cons_1; case_cons_2; case_rev_1; case_rev_2;
      case_concat_1; case_concat_2; case_concat_3;
      case_concat_4 ]
  ]
