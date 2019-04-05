open Trace

type ('a, 'b) t

val get_exn : ('a, 'b) t -> 'a -> 'b
val get : ('a, 'b) t -> 'a -> 'b result

val set :
  ?equal:('a -> 'a -> bool) ->
  ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t

val del :
  ?equal:('a -> 'a -> bool) ->
  ('a, 'b) t -> 'a -> ('a, 'b) t

val to_list : ('a, 'b) t -> ('a * 'b) list
