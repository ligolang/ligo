type (+'value, +'error) result
val ok : 'a -> ('a, 'c) result
val fail : 'a -> ('b, 'a) result
val update_annotation : 'error -> ('value, 'error) result -> ('value, 'error) result
val to_stdlib_result : ('value, 'error) result -> ('value * 'error list, 'error * 'error list) Stdlib.result
val warnings : ('value, 'error) result -> 'error list
val bind :
  ('a -> ('b, 'd) result) ->
  ('a, 'd) result -> ('b, 'd) result
val map : f:('a -> 'b) -> ('a, 'd) result -> ('b, 'd) result
val ( >>? ) :
  ('a, 'c) result ->
  ('a -> ('d, 'c) result) -> ('d, 'c) result
val ( let* ) :
  ('a, 'c) result ->
  ('a -> ('d, 'c) result) -> ('d, 'c) result
val ( >>|? ) : ('a, 'c) result -> ('a -> 'd) -> ('d, 'c) result
module Let_syntax :
sig
  val bind :
    ('a, 'c) result ->
    f:('a -> ('d, 'c) result) -> ('d, 'c) result
  module Open_on_rhs_bind : sig  end
end
val trace : ('a -> 'b) -> ('c, 'a) result -> ('c, 'b) result
val trace_strong : 'a -> ('b, 'c) result -> ('b, 'a) result
val try_catch : ('a -> ('b, 'a) result) -> ('b, 'a) result -> ('b, 'a) result
val to_bool : ('a, 'b) result -> bool
val to_option : ('a, 'c) result -> 'a option
val to_json : ('a -> ([> `Null ] as 'b)) -> ('a, 'd) result -> 'b
val trace_option : 'a -> 'b option -> ('b, 'a) result
val trace_assert_fail_option :
  'a -> 'b option -> (unit, 'a) result
val bind_compose :
  ('a -> ('b, 'd) result) ->
  ('e -> ('a, 'd) result) -> 'e -> ('b, 'd) result
val bind_map_option :
  ('a -> ('b, 'd) result) ->
  'a option -> ('b option, 'd) result
val bind_list :
  ('a, 'c) result list -> ('a list, 'c) result
val bind_ne_list :
  ('a, 'c) result * ('d, 'c) result list ->
  (('a * 'd list), 'c) result
val bind_smap :
  ('a, 'c) result X_map.String.t ->
  ('a X_map.String.t, 'c) result
val bind_fold_smap :
  ('a -> X_map.String.key -> 'b -> ('a, 'd) result) ->
  ('a, 'd) result ->
  'b X_map.String.t -> ('a, 'd) result
val bind_map_smap :
  ('a -> ('b, 'd) result) ->
  'a X_map.String.t ->
  ('b X_map.String.t, 'd) result
val bind_map_list :
  ('a -> ('b, 'd) result) ->
  'a list -> ('b list, 'd) result
val bind_map2_list :
  ('a1 -> 'a2 -> ('b, 'd) result) ->
  'a1 list -> 'a2 list -> ('b list, 'd) result
val bind_mapi_list :
  (int -> 'a -> ('b, 'd) result) ->
  'a list -> ('b list, 'd) result
val bind_map_list_seq :
  ('a -> ('b, 'd) result) ->
  'a list -> ('b list, 'd) result
val bind_map_ne_list :
  ('a -> ('b, 'c) result) ->
  'a X_list.Ne.t ->
  ('b X_list.Ne.t, 'c) result
val bind_iter_list :
  ('a -> (unit, 'b) result) -> 'a list -> (unit, 'b) result
val bind_location :
  ('a, 'c) result Location.wrap ->
  ('a Location.wrap, 'c) result
val bind_map_location :
  ('a -> ('b, 'd) result) ->
  'a Location.wrap ->
  ('b Location.wrap, 'd) result
val bind_fold_location :
  ('a -> 'b -> ('a, 'd) result) ->
  'a -> 'b Location.wrap -> ('a, 'd) result
val bind_fold_list :
  ('a -> 'b -> ('a, 'd) result) ->
  'a -> 'b list -> ('a, 'd) result
val bind_fold_ne_list :
  ('a -> 'b -> ('a, 'd) result) ->
  'a -> 'b X_list.Ne.t-> ('a, 'd) result
module TMap :
  functor (X : Map.OrderedType) ->
  sig
    module MX :
    sig
      type key = X.t
      type 'a t = 'a Map.Make(X).t
      val empty : 'a t
      val is_empty : 'a t -> bool
      val mem : key -> 'a t -> bool
      val add : key -> 'a -> 'a t -> 'a t
      val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
      val singleton : key -> 'a -> 'a t
      val remove : key -> 'a t -> 'a t
      val merge :
        (key -> 'a option -> 'b option -> 'c option) ->
        'a t -> 'b t -> 'c t
      val union :
        (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
      val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
      val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
      val iter : (key -> 'a -> unit) -> 'a t -> unit
      val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      val for_all : (key -> 'a -> bool) -> 'a t -> bool
      val exists : (key -> 'a -> bool) -> 'a t -> bool
      val filter : (key -> 'a -> bool) -> 'a t -> 'a t
      val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
      val cardinal : 'a t -> int
      val bindings : 'a t -> (key * 'a) list
      val min_binding : 'a t -> key * 'a
      val min_binding_opt : 'a t -> (key * 'a) option
      val max_binding : 'a t -> key * 'a
      val max_binding_opt : 'a t -> (key * 'a) option
      val choose : 'a t -> key * 'a
      val choose_opt : 'a t -> (key * 'a) option
      val split : key -> 'a t -> 'a t * 'a option * 'a t
      val find : key -> 'a t -> 'a
      val find_opt : key -> 'a t -> 'a option
      val find_first : (key -> bool) -> 'a t -> key * 'a
      val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
      val find_last : (key -> bool) -> 'a t -> key * 'a
      val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
      val map : ('a -> 'b) -> 'a t -> 'b t
      val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
      val to_seq : 'a t -> (key * 'a) Seq.t
      val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
      val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
      val of_seq : (key * 'a) Seq.t -> 'a t
    end
    val bind_fold_Map :
      (x:'a -> k:X.t -> v:'b -> ('a, 'd) result) ->
      'a -> 'b MX.t -> ('a, 'd) result
    val bind_map_Map :
      (k:X.t -> v:'a -> ('b, 'd) result) ->
      'a MX.t -> ('b MX.t, 'd) result
  end
val bind_fold_pair :
  ('a -> 'b -> ('a, 'd) result) ->
  'a -> 'b * 'b -> ('a, 'd) result
val bind_fold_triple :
  ('a -> 'b -> ('a, 'd) result) ->
  'a -> 'b * 'b * 'b -> ('a, 'd) result
val bind_fold_map_list :
  ('a -> 'b -> ('a * 'c, 'e) result) ->
  'a -> 'b list -> ('a * 'c list, 'e) result
val bind_fold_map2_list :
  ('a -> 'b -> 'c -> ('a * 'd, 'e) result) ->
  'a -> 'b list -> 'c list -> ('a * 'd list, 'e) result
val bind_fold_map_location :
  ('a -> 'b -> ('a * 'c, 'e) result) ->
  'a -> 'b Location.wrap -> ('a * 'c Location.wrap, 'e) result
val bind_fold_map_right_list :
  ('a -> 'b -> ('a * 'c, 'e) result) ->
  'a -> 'b list -> ('c list, 'e) result
val bind_fold_right_list :
  ('a -> 'b -> ('a, 'd) result) ->
  'a -> 'b list -> ('a, 'd) result
val bind_find_map_list :
  'a -> ('b -> ('c, 'a) result) -> 'b list -> ('c, 'a) result
val bind_list_iter :
  ('a -> (unit, 'c) result) ->
  'a list -> (unit, 'c) result
val bind_or : ('a, 'c) result * ('a, 'c) result -> ('a, 'c) result
val bind_map_or :
  ('a -> ('b, 'c) result) * ('a -> ('b, 'c) result) ->
  'a -> ('b, 'c) result
val bind_and :
  ('a, 'c) result * ('d, 'c) result ->
  ('a * 'd, 'c) result
val bind_and3 :
  ('a, 'c) result * ('d, 'c) result *
  ('e, 'c) result -> (('a * 'd * 'e), 'c) result
val bind_pair :
  ('a, 'c) result * ('d, 'c) result ->
  ('a * 'd, 'c) result
val bind_map_pair :
  ('a -> ('b, 'd) result) ->
  'a * 'a -> ('b * 'b, 'd) result
val bind_fold_map_pair :
  ('a -> 'b -> ('a * 'c, 'e) result) ->
  'a -> 'b * 'b -> ('a * ('c * 'c), 'e) result
val bind_map_triple :
  ('a -> ('b, 'd) result) ->
  'a * 'a * 'a -> ('b * 'b * 'b, 'd) result
val bind_list_cons :
  'a -> ('a list, 'c) result -> ('a list, 'c) result
val bind_chain : ('a -> ('a, 'b) result) list -> 'a -> ('a, 'b) result
val bind_chain_acc : ('a -> 'b -> ('a, 'c) result) list -> 'a -> 'b -> ('a, 'c) result
val bind_chain_ignore_acc :
  ('a -> ('b * 'a, 'c) result) list -> 'a -> ('a, 'c) result
val generic_try : 'a -> (unit -> 'b) -> ('b, 'a) result
val specific_try : (exn -> 'a) -> (unit -> 'b) -> ('b, 'a) result
module Assert :
sig
  val assert_fail : 'a -> ('b, 'c) result -> (unit, 'a) result
  val assert_true : 'a -> bool -> (unit, 'a) result
  val assert_list_size :
    'a -> 'b list -> int -> (unit, 'a) result
  val assert_list_empty : 'a -> 'b list -> (unit, 'a) result
  val assert_list_same_size :
    'a -> 'b list -> 'c list -> (unit, 'a) result
end
