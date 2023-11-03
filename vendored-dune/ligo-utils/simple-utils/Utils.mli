(* Utility types and functions *)

(* Polymorphic identity function *)

val id : 'a -> 'a

(* Combinators *)

val ( <@ )  : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val swap    : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val lambda  : 'a -> 'b -> 'a
val curry   : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

(* Parametric rules for sequences

   nseq:    non-empty sequence;
   sepseq:  (possibly empty) sequence of separated items;
   nsepseq: non-empty sequence of separated items.
*)

type           'a nseq = 'a * 'a list
type ('a,'sep) nsepseq = 'a * ('sep * 'a) list
type ('a,'sep)  sepseq = ('a,'sep) nsepseq option

type ('a,'sep) nsep_or_term = [
  `Sep  of ('a,'sep) nsepseq
| `Term of ('a * 'sep) nseq
]

type ('a,'sep) sep_or_term = ('a,'sep) nsep_or_term option

type ('a,'sep) nsep_or_pref = [
  `Sep  of ('a,'sep) nsepseq
| `Pref of ('sep * 'a) nseq
]

(* Consing *)

val nseq_cons    : 'a -> 'a nseq -> 'a nseq
val nsepseq_cons : 'a -> 'sep -> ('a,'sep) nsepseq -> ('a,'sep) nsepseq
val sepseq_cons  : 'a -> 'sep -> ('a,'sep)  sepseq -> ('a,'sep) nsepseq

val nsep_or_term_cons :
  'a -> 'sep -> ('a,'sep) nsep_or_term -> ('a,'sep) nsep_or_term

val sep_or_term_cons :
  'a -> 'sep -> ('a,'sep) sep_or_term -> ('a,'sep) nsep_or_term

val nsep_or_pref_cons :
  'a -> 'sep -> ('a,'sep) nsep_or_pref -> ('a,'sep) nsep_or_pref

(* Reversing *)

val nseq_rev    : 'a nseq -> 'a nseq
val nsepseq_rev : ('a,'sep) nsepseq -> ('a,'sep) nsepseq
val sepseq_rev  : ('a,'sep)  sepseq -> ('a,'sep)  sepseq

val nsep_or_term_rev : ('a,'sep) nsep_or_term -> ('a,'sep) nsep_or_term
val sep_or_term_rev  : ('a,'sep)  sep_or_term -> ('a,'sep)  sep_or_term
val nsep_or_pref_rev : ('a,'sep) nsep_or_pref -> ('a,'sep) nsep_or_pref

(* Rightwards iterators *)

val nseq_foldl    : ('a -> 'b -> 'a) -> 'a ->        'b nseq -> 'a
val nsepseq_foldl : ('a -> 'b -> 'a) -> 'a -> ('b,'c) nsepseq -> 'a
val sepseq_foldl  : ('a -> 'b -> 'a) -> 'a -> ('b,'c)  sepseq -> 'a

val nsep_or_term_foldl :
  ('a -> 'b -> 'a) -> 'a -> ('b,'sep) nsep_or_term -> 'a

val sep_or_term_foldl :
  ('a -> 'b -> 'a) -> 'a -> ('b,'sep) sep_or_term -> 'a

val nsep_or_pref_foldl :
  ('a -> 'b -> 'a) -> 'a -> ('b,'sep) nsep_or_pref -> 'a

val nseq_iter    : ('a -> unit) ->        'a nseq -> unit
val nsepseq_iter : ('a -> unit) -> ('a,'b) nsepseq -> unit
val sepseq_iter  : ('a -> unit) -> ('a,'b)  sepseq -> unit

val nsep_or_term_iter : ('a -> unit) -> ('a,'b) nsep_or_term -> unit
val sep_or_term_iter  : ('a -> unit) -> ('a,'b)  sep_or_term -> unit
val nsep_or_pref_iter : ('a -> unit) -> ('a,'b) nsep_or_pref -> unit

(* Leftwards iterators *)

val nseq_foldr    : ('a -> 'b -> 'b) ->        'a nseq -> 'b -> 'b
val nsepseq_foldr : ('a -> 'b -> 'b) -> ('a,'c) nsepseq -> 'b -> 'b
val sepseq_foldr  : ('a -> 'b -> 'b) -> ('a,'c)  sepseq -> 'b -> 'b

val nsep_or_term_foldr : ('a -> 'b -> 'b) -> ('a,'c) nsep_or_term -> 'b -> 'b
val sep_or_term_foldr  : ('a -> 'b -> 'b) -> ('a,'c)  sep_or_term -> 'b -> 'b
val nsep_or_pref_foldr : ('a -> 'b -> 'b) -> ('a,'c) nsep_or_pref -> 'b -> 'b

(* Maps *)

val nseq_map    : ('a -> 'b) -> 'a nseq -> 'b nseq
val nsepseq_map : ('a -> 'b) -> ('a,'c) nsepseq -> ('b,'c) nsepseq
val sepseq_map  : ('a -> 'b) -> ('a,'c)  sepseq -> ('b,'c)  sepseq

val nsep_or_term_map : ('a -> 'b) -> ('a,'c) nsep_or_term -> ('b,'c) nsep_or_term
val sep_or_term_map  : ('a -> 'b) -> ('a,'c)  sep_or_term -> ('b,'c)  sep_or_term
val nsep_or_pref_map : ('a -> 'b) -> ('a,'c) nsep_or_pref -> ('b,'c) nsep_or_pref

(* Conversions to lists *)

val nseq_to_list    :        'a nseq -> 'a list
val nsepseq_to_list : ('a,'b) nsepseq -> 'a list
val sepseq_to_list  : ('a,'b)  sepseq -> 'a list

val nsep_or_term_to_list : ('a,'sep) nsep_or_term -> 'a list
val sep_or_term_to_list  : ('a,'sep)  sep_or_term -> 'a list
val nsep_or_pref_to_list : ('a,'sep) nsep_or_pref -> 'a list

(* Map and concatenate lists *)

val nseq_concat_map    :  'a       nseq -> f:('a -> 'c list) -> 'c list
val nsepseq_concat_map : ('a,'b) nsepseq -> f:('a -> 'c list) -> 'c list
val sepseq_concat_map  : ('a,'b)  sepseq -> f:('a -> 'c list) -> 'c list

(* Conversions from/to non-empty sequences *)

val nsepseq_to_nseq : ('a,'b) nsepseq -> 'a nseq
val nsepseq_of_nseq : sep:'b -> 'a nseq -> ('a,'b) nsepseq

(* Convertions from lists *)

val sep_or_term_of_list:
  sep:'sep ->
  sep_or_term:[< `Sep | `Term ] ->
  'a list ->
  ('a,'sep) sep_or_term
val list_to_sepseq : 'a list -> 's -> ('a, 's) sepseq

(* Effectful symbol generator *)

val gen_sym : unit -> string

(* General tracing function *)

val trace : string -> Core.out_channel option -> unit

(* Printing a string in red to standard error *)

val highlight : string -> unit

(* When failing to parse a specifed JSON format *)

val error_yojson_format : string -> ('a, string) result

(* An extension to the standard module [String] *)

module String :
  sig
    include module type of String
    module Map : Caml.Map.S with type key = t
    module Set : Caml.Set.S with type elt = t
  end

(* Integer maps *)

module Int :
  sig
    type t = int
    module Map : Caml.Map.S with type key = t
    module Set : Caml.Set.S with type elt = t
  end

(* Optional let *)

val (let*) : 'a option -> ('a -> 'b option) -> 'b option
val return : 'a -> 'a option
