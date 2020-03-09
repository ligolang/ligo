val bind_lmap :
  ('a * 'b list, 'c) result Types.label_map ->
  ('a Types.label_map * 'b list, 'c) result
val bind_cmap :
  ('a * 'b list, 'c) result Types.constructor_map ->
  ('a Types.constructor_map * 'b list, 'c) result
val bind_fold_lmap :
  ('a -> Types.label -> 'b -> ('a * 'c list, 'd) result) ->
  ('a * 'c list, 'd) result ->
  'b Types.label_map -> ('a * 'c list, 'd) result
val bind_map_lmap :
  ('a -> ('b * 'c list, 'd) result) ->
  'a Types.label_map -> ('b Types.label_map * 'c list, 'd) result
val bind_map_cmap :
  ('a -> ('b * 'c list, 'd) result) ->
  'a Types.constructor_map ->
  ('b Types.constructor_map * 'c list, 'd) result
val is_tuple_lmap : 'a Types.label_map -> bool
val get_pair :
           'a Types.label_map ->
           (('a * 'a) * 'b list, unit -> Trace.error) result



val bind_map_lmapi : 
  (Types.label -> 'a -> ('b * 'c list, 'd) result) ->
  'a Types.label_map -> ('b Types.label_map * 'c list, 'd) result
val bind_map_cmapi : 
  (Types.constructor' -> 'a -> ('b * 'c list, 'd) result) ->
  'a Types.constructor_map -> ('b Types.constructor_map * 'c list, 'd) result
