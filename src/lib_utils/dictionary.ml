open Trace

module type DICTIONARY = sig
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
end

module Assoc : DICTIONARY = struct

  type ('a, 'b) t = ('a * 'b) list

  let get_exn x y = List.assoc y x

  let get x y = generic_try (simple_error "Dictionry.get") @@ fun () -> get_exn x y

  let set ?equal lst a b =
    let equal : 'a -> 'a -> bool =
      Option.unopt
        ~default:(=) equal
    in
    let rec aux acc = function
      | [] -> List.rev acc
      | (key, _)::tl when equal key a -> aux ((key, b) :: acc) tl
      | hd::tl -> aux (hd :: acc) tl
    in
    aux [] lst

  let del ?equal lst a =
    let equal : 'a -> 'a -> bool =
      Option.unopt
        ~default:(=) equal
    in
    let rec aux acc = function
      | [] -> List.rev acc
      | (key, _)::tl when equal key a -> aux acc tl
      | hd::tl -> aux (hd :: acc) tl
    in
    aux [] lst

  let to_list x = x
end
