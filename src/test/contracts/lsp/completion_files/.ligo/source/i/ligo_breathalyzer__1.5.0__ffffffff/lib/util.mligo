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


(** Try to convert a digit to a string. *)
let digit_to_str (x: int) : string =
  (* Very sad that we can't match on value and only on constructor,
     which seems weird since a sum can contain literate values... *)
  if x = 0 then "0"
  else if x = 1 then "1"
  else if x = 2 then "2"
  else if x = 3 then "3"
  else if x = 4 then "4"
  else if x = 5 then "5"
  else if x = 6 then "6"
  else if x = 7 then "7"
  else if x = 8 then "8"
  else if x = 9 then "0"
  else
    (* Should never happen! *)
    failwith "digit_to_str, unknown digit"

(** Convert an int to a string. *)
let int_to_string (x: int) : string =
  let prefix = if x < 0 then "-" else "" in
  let subject = int (abs x) in
  let rec aux (acc : string) (x: int) : string =
    if x < 10 then
      let digit = digit_to_str x in
      digit ^ acc
    else
      let part = int (x mod 10) in
      let rest = x / 10 in
      let digit = digit_to_str part in
      let str = digit ^ acc in
      aux str rest
  in prefix ^ (aux "" subject)

(* Convert a nat to a string. *)
let nat_to_string (x: nat) : string =
  int_to_string (int x) ^ "n"

let nat_to_string_without_suffix (x: nat) : string =
  int_to_string (int x)

(* Convert a tez to a string. *)
let tez_to_string (x: tez) : string =
  nat_to_string (x / 1tez) ^ "tez"

(** [concat a b] concat [a] and [b]. *)
let concat (type a) (left: a list) (right: a list) : a list =
  List.fold_right (fun (x, xs: a * a list) -> x :: xs) left right

(** [rev list] should return the same list reversed. *)
let rev (type a) (list: a list) : a list =
  List.fold_left (fun (xs, x : a list * a) -> x :: xs) ([] : a list) list
