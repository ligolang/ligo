(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Micheline

include Michelson_primitives

type error += Unknown_primitive_name of string (* `Permanent *)
type error += Invalid_case of string (* `Permanent *)
type error += Invalid_primitive_name of string Micheline.canonical * Micheline.canonical_location (* `Permanent *)

let prim_of_string x = match prim_of_string x with
  | Ok x -> ok x
  | Error (Unknown_primitive_name x) -> error (Unknown_primitive_name x)
  | Error (Invalid_case x) -> error (Invalid_case x)
  | Error (Invalid_primitive_name (a , b)) -> error (Invalid_primitive_name (a , b))

let prims_of_strings x = match prims_of_strings x with
  | Ok x -> ok x
  | Error (Unknown_primitive_name x) -> error (Unknown_primitive_name x)
  | Error (Invalid_case x) -> error (Invalid_case x)
  | Error (Invalid_primitive_name (a , b)) -> error (Invalid_primitive_name (a , b))
