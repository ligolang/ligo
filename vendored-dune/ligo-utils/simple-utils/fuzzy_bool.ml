open Core

module T = struct
  type t =
    | Yes
    | No
    | Maybe
  [@@deriving compare, sexp]
end

include T
include Comparable.Make (T)

let to_lowercase_string t =
  match t with
  | Yes -> "yes"
  | No -> "no"
  | Maybe -> "maybe"

module Indexed = struct
  module T = struct
    type 'a t =
      { yes : 'a
      ; no : 'a
      ; maybe : 'a
      }
    [@@deriving map]
  end

  include T

  let map t ~f = map f t
  let create f = { yes = f Yes; no = f No; maybe = f Maybe }
  let create_constant v = { yes = v; no = v; maybe = v }

  let fold_map t ~init ~f =
    let acc = init in
    let { yes; no; maybe } = t in
    let acc, yes = f acc yes in
    let acc, no = f acc no in
    let acc, maybe = f acc maybe in
    acc, { yes; no; maybe }

  let get t key =
    match key with
    | Yes -> t.yes
    | No -> t.no
    | Maybe -> t.maybe

  let change t key ~f =
    match key with
    | Yes -> { t with yes = f t.yes }
    | No -> { t with no = f t.no }
    | Maybe -> { t with maybe = f t.maybe }

  let of_alist_multi l =
    let rec aux t l =
      match l with
      | [] -> map t ~f:List.rev
      | (fb, value) :: tl -> aux (change t fb ~f:(List.cons value)) tl
    in
    aux (create_constant []) l

  module For_deriving = struct
    include T

    let fold_map f init t = fold_map ~f ~init t
  end
end
