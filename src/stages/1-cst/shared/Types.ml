(* Vendor dependencies *)

module Directive = Preprocessor.Directive
module Utils     = Simple_utils.Utils
module Region    = Simple_utils.Region

(* Local dependencies *)

module Wrap = Lexing_shared.Wrap
module Attr = Lexing_shared.Attr

(* Utilities *)

type 'a reg = 'a Region.reg
type 'payload wrap = 'payload Wrap.t

open Utils

(* Lexemes *)

type lexeme = string

(* Yojson utilities *)

let yojson_of_reg f Region.{region; value} =
  `Assoc [("region", Region.to_yojson region); ("value", f value)]

let yojson_of_wrap f (wrapped : 'a wrap) =
  let open Wrap in
  `Assoc [("payload", f wrapped#payload);
          ("region", Region.to_yojson wrapped#region)]

let yojson_of_lexeme s = `String s


let yojson_of_nseq f (s : 'a nseq) = `List (nseq_to_list @@ nseq_map f s)
let yojson_of_nsepseq f _ (s : ('a, 'sep) nsepseq) = `List (nsepseq_to_list @@ nsepseq_map f s)
let yojson_of_sepseq f _ (s : ('a, 'sep) sepseq) = `List (sepseq_to_list @@ sepseq_map f s)

let yojson_of_sep_or_term f _ (s : ('a, 'sep) sep_or_term) =
  `List (sep_or_term_to_list @@ sep_or_term_map f s)

let yojson_of_nsep_or_term f _ (s : ('a, 'sep) nsep_or_term) =
  `List (nsep_or_term_to_list @@ nsep_or_term_map f s)

let yojson_of_nsep_or_pref f _ (s : ('a, 'sep) nsep_or_pref) =
  `List (nsep_or_pref_to_list @@ nsep_or_pref_map f s)

type z = Z.t

type hex = Hex.t

type int64 = Int64.t
