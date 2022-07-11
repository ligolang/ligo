open Types

val label_range   : int -> int -> label list

val is_tuple_lmap : 'a Types.label_map -> bool


val tuple_of_record : 'a LMap.t -> (label * 'a) list

val fold_pattern : ('a -> 'b pattern -> 'a) -> 'a -> 'b pattern -> 'a
val map_pattern_t : ('a binder -> 'b binder) -> 'a pattern -> 'b pattern

val var_attribute : binder_attributes
val const_attribute : binder_attributes
val empty_attribute : binder_attributes
