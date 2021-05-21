open Trace
open Types

val bind_lmap :
  ('a, 'c) result label_map -> ('a label_map , 'c) result

val bind_fold_lmap :
  ('a -> label -> 'b -> ('a , 'd) result) -> 'a ->
  'b label_map -> ('a , 'd) result

val bind_map_lmap :
  ('a -> ('b , 'd) result) ->
  'a label_map -> ('b label_map , 'd) result

val bind_fold_map_lmap : 
  ('a -> label -> 'b -> ('a * 'c, 'd) result) -> 'a ->
  'b label_map -> ('a * 'c label_map, 'd) result
val bind_iter_lmap :
  (label -> 'a -> (unit , 'c) result) ->
  'a label_map -> (unit , 'c) result

val is_tuple_lmap : 'a Types.label_map -> bool

val get_pair : 'a Types.label_map -> ('a * 'a) option

val tuple_of_record : 'a LMap.t -> (label * 'a) list
val list_of_record_or_tuple : 'a LMap.t -> 'a list
val kv_list_of_record_or_tuple : 'a LMap.t -> (label * 'a) list

val bind_map_lmapi : 
  (label -> 'a -> ('b , 'd) result) ->
  'a label_map -> ('b label_map , 'd) result

val fold_pattern : ('a -> 'b pattern -> 'a) -> 'a -> 'b pattern -> 'a
val fold_pattern_list : ('a -> 'b pattern -> 'a) -> 'a -> 'b pattern list -> 'a
val map_pattern_t : ('a binder -> ('b binder, 'err) result) -> 'a pattern -> ('b pattern, 'err) result

val var_attribute : binder_attributes
val const_attribute : binder_attributes
val empty_attribute : binder_attributes
