(* Maps *)

open Core

(** Like [Stdlib.union] *)
val union :
  ('key -> 'data -> 'data -> 'data option)
  -> ('key, 'data, 'cmp) Map.t
  -> ('key, 'data, 'cmp) Map.t
  -> ('key, 'data, 'cmp) Map.t

(** Like [Map.find], but also returns the key along with the data.
    It might seem redundant to give a key to a map only to return the
    same one, however, it's possible that the original key and the
    provided key compare equally but contain different fields, such as
    is the case for variables or labels, that ignore locations during
    comparisons. *)
val find_kv
  : 'key 'data 'cmp.
     (module Comparable with type t = 'key and type comparator_witness = 'cmp)
  -> ('key, 'data, 'cmp) Map.t
  -> 'key
  -> ('key * 'data) option
