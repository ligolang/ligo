(* Utility types and functions *)

(* Parametric rules for sequences

   sepseq:  (possibly empty) sequence of separated items;
   nsepseq: non-empty sequence of separated items.
*)

open Core

type ('a,'sep) nsepseq = 'a * ('sep * 'a) list
type ('a,'sep)  sepseq = ('a,'sep) nsepseq option

type ('a,'sep) nsep_or_term = [
  `Sep  of ('a,'sep) nsepseq
| `Term of ('a * 'sep) Nonempty_list.t
]

type ('a,'sep) sep_or_term = ('a,'sep) nsep_or_term option

type ('a,'sep) nsep_or_pref = [
  `Sep  of ('a,'sep) nsepseq
| `Pref of ('sep * 'a) Nonempty_list.t
]

(* Consing *)

val nsepseq_cons : 'a -> 'sep -> ('a,'sep) nsepseq -> ('a,'sep) nsepseq
val sepseq_cons  : 'a -> 'sep -> ('a,'sep)  sepseq -> ('a,'sep) nsepseq

val nsep_or_term_cons :
  'a -> 'sep -> ('a,'sep) nsep_or_term -> ('a,'sep) nsep_or_term

val sep_or_term_cons :
  'a -> 'sep -> ('a,'sep) sep_or_term -> ('a,'sep) nsep_or_term

val nsep_or_pref_cons :
  'a -> 'sep -> ('a,'sep) nsep_or_pref -> ('a,'sep) nsep_or_pref

(* Reversing *)

val nsepseq_rev : ('a,'sep) nsepseq -> ('a,'sep) nsepseq
val sepseq_rev  : ('a,'sep)  sepseq -> ('a,'sep)  sepseq

val nsep_or_term_rev : ('a,'sep) nsep_or_term -> ('a,'sep) nsep_or_term
val sep_or_term_rev  : ('a,'sep)  sep_or_term -> ('a,'sep)  sep_or_term
val nsep_or_pref_rev : ('a,'sep) nsep_or_pref -> ('a,'sep) nsep_or_pref

(* Rightwards iterators *)

val nsepseq_foldl : ('a -> 'b -> 'a) -> 'a -> ('b,'c) nsepseq -> 'a
val sepseq_foldl  : ('a -> 'b -> 'a) -> 'a -> ('b,'c)  sepseq -> 'a

val nsep_or_term_foldl :
  ('a -> 'b -> 'a) -> 'a -> ('b,'sep) nsep_or_term -> 'a

val sep_or_term_foldl :
  ('a -> 'b -> 'a) -> 'a -> ('b,'sep) sep_or_term -> 'a

val nsep_or_pref_foldl :
  ('a -> 'b -> 'a) -> 'a -> ('b,'sep) nsep_or_pref -> 'a

val nsepseq_iter : ('a -> unit) -> ('a,'b) nsepseq -> unit
val sepseq_iter  : ('a -> unit) -> ('a,'b)  sepseq -> unit

val nsep_or_term_iter : ('a -> unit) -> ('a,'b) nsep_or_term -> unit
val sep_or_term_iter  : ('a -> unit) -> ('a,'b)  sep_or_term -> unit
val nsep_or_pref_iter : ('a -> unit) -> ('a,'b) nsep_or_pref -> unit

(* Leftwards iterators *)

val nsepseq_foldr : ('a -> 'b -> 'b) -> ('a,'c) nsepseq -> 'b -> 'b
val sepseq_foldr  : ('a -> 'b -> 'b) -> ('a,'c)  sepseq -> 'b -> 'b

val nsep_or_term_foldr : ('a -> 'b -> 'b) -> ('a,'c) nsep_or_term -> 'b -> 'b
val sep_or_term_foldr  : ('a -> 'b -> 'b) -> ('a,'c)  sep_or_term -> 'b -> 'b
val nsep_or_pref_foldr : ('a -> 'b -> 'b) -> ('a,'c) nsep_or_pref -> 'b -> 'b

(* Maps *)

val nsepseq_map : ('a -> 'b) -> ('a,'c) nsepseq -> ('b,'c) nsepseq
val sepseq_map  : ('a -> 'b) -> ('a,'c)  sepseq -> ('b,'c)  sepseq

val nsep_or_term_map : ('a -> 'b) -> ('a,'c) nsep_or_term -> ('b,'c) nsep_or_term
val sep_or_term_map  : ('a -> 'b) -> ('a,'c)  sep_or_term -> ('b,'c)  sep_or_term
val nsep_or_pref_map : ('a -> 'b) -> ('a,'c) nsep_or_pref -> ('b,'c) nsep_or_pref

(* Conversions to non-empty lists *)

val nsepseq_to_ne_list      : ('a,   'b) nsepseq      -> 'a Nonempty_list.t
val nsep_or_term_to_ne_list : ('a,'sep) nsep_or_term -> 'a Nonempty_list.t
val nsep_or_pref_to_ne_list : ('a,'sep) nsep_or_pref -> 'a Nonempty_list.t

(* Conversions to lists *)

val nsepseq_to_list : ('a,'b) nsepseq -> 'a list
val sepseq_to_list  : ('a,'b)  sepseq -> 'a list

val nsep_or_term_to_list : ('a,'sep) nsep_or_term -> 'a list
val sep_or_term_to_list  : ('a,'sep)  sep_or_term -> 'a list
val nsep_or_pref_to_list : ('a,'sep) nsep_or_pref -> 'a list

(* Map and concatenate lists *)

val nsepseq_concat_map : ('a,'b) nsepseq -> f:('a -> 'c list) -> 'c list
val sepseq_concat_map  : ('a,'b)  sepseq -> f:('a -> 'c list) -> 'c list

(* Conversions from/to non-empty sequences *)

val nsepseq_to_ne_list : ('a,'b) nsepseq -> 'a Nonempty_list.t
val nsepseq_of_ne_list : sep:'b -> 'a Nonempty_list.t -> ('a,'b) nsepseq

(* Conversions from lists *)

val sep_or_term_of_list:
  sep:'sep ->
  sep_or_term:[< `Sep | `Term ] ->
  'a list ->
  ('a,'sep) sep_or_term

val list_to_sepseq : 'a list -> 's -> ('a, 's) sepseq

(* Conversions to JSON *)

type json = Yojson.Safe.t

val yojson_of_nsepseq :
  ('a -> json) -> ('sep -> json) -> ('a,'sep) nsepseq -> json

val yojson_of_sepseq :
  ('a -> json) -> ('sep -> json) -> ('a,'sep) sepseq -> json

val yojson_of_sep_or_term :
  ('a -> json) -> ('sep -> json) -> ('a,'sep) sep_or_term -> json

val yojson_of_nsep_or_term :
  ('a -> json) -> ('sep -> json) -> ('a,'sep) nsep_or_term -> json

val yojson_of_nsep_or_pref :
  ('a -> json) -> ('sep -> json) -> ('a,'sep) nsep_or_pref -> json
