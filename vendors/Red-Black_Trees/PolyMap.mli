(* Polymorphic maps

   This module does not provide a function to merge polymorphic
   maps. Use the functorial interface of the module [Map] of the OCaml
   standard library instead, at the cost of the polymorphism on the
   keys.

   No deletion is provided.
*)

type ('key, 'value) t
type ('key, 'value) map = ('key, 'value) t

(* The value of the call [create ~cmp] is an empty map with [cmp]
   being the comparison over the (future) keys.

   The value [empty] is identical to the value of the call [create
   ~cmp:Pervasives.compare].
*)

val create : cmp:('key -> 'key -> int) -> ('key, 'value) t

val empty : ('key, 'value) t -> ('key, 'new_value) t

(* Emptiness *)

val is_empty : ('key, 'value) t -> bool

(* The value of the call [add key value map] is a map containing all
   the bindings of the map [map], extended by the binding of [key] to
   [value]. If there is a binding for [key] in [map], its value is
   lost (and replaced by [value]). *)

val add : 'key -> 'value -> ('key, 'value) t -> ('key, 'value) t

(* The value of the call [add key value map] is a map containing all
   the bindings of the map [map], except for the binding of [key]. *)

val remove : 'key -> ('key, 'value) t -> ('key, 'value) t

(* The value of the call [find key map] is the value associated to the
   [key] in the map [map]. If [key] is not bound in [map], the
   exception [Not_found] is raised. *)

val find : 'key -> ('key, 'value) t -> 'value

(* The value of the call [find_opt key map] is [Some value] if the key
   [key] is bound to [value] in the map [map], and [None]
   otherwise. *)

val find_opt : 'key -> ('key, 'value) t -> 'value option

(* The value of the call [update key f map] is a map containing all
   the bindings of the map [map], extended by the binding of [key] to
   the value returned by [f], when [f maybe_value] returns
   [Some value]. On the other hand, when [f maybe_value] returns
   [None], the existing binding for [key] in [map] is removed from the
   map, if there is one. The argument [maybe_value] passed to [f] is
   [Some value] if the key [key] is bound to [value] in the map [map],
   and [None] otherwise. *)

val update : 'key -> ('value option -> 'value option) -> ('key, 'value) map -> ('key, 'value) map

(* The value of the call [bindings map] is the association list
   containing the bindings of the map [map], sorted by increasing keys
   (with respect to the total comparison function used to create the
   map). *)

val bindings : ('key, 'value) t -> ('key * 'value) list

(* The side-effect of evaluating the call [iter f map] is the
   successive side-effects of the calls [f key value], for all
   bindings [(key, value)] belonging to the map [map], sorted in
   increasing order of the keys (with respect to the total comparison
   function used to create the map). *)

val iter : ('key -> 'value -> unit) -> ('key, 'value) t -> unit

(* The call [fold_inc f map ~init] computes [(f k_n v_n ~acc:(... (f
   k_1 v_1 ~acc:init)...)], where [k_1], ..., [k_n] are the keys of
   all bindings in the map [map] in increasing order, and [v_1], ...,
   [v_n] are the associated values. *)

val fold_inc : ('key -> 'value -> acc:'a -> 'a) -> ('key, 'value) t -> init:'a -> 'a
