open Core

let union f m1 m2 =
  let f ~key = function
    | `Left v1 -> Some v1
    | `Right v2 -> Some v2
    | `Both (v1, v2) -> f key v1 v2
  in
  Map.merge ~f m1 m2

let find_kv
    (type key data cmp)
    (module Cmp : Comparable with type t = key and type comparator_witness = cmp)
    (map : (key, data, cmp) Map.t)
    (key' : key)
    : (key * data) option
  =
  Map.binary_search map `First_equal_to () ~compare:(fun ~key ~data:_ () ->
      Cmp.compare key key')
