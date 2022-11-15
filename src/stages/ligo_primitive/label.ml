type t = Label of string [@@deriving eq, compare, yojson, hash, sexp]

let pp ppf (l : t) : unit =
  let (Label l) = l in
  Format.fprintf ppf "%s" l


let range i j = List.map ~f:(fun i -> Label (string_of_int i)) @@ List.range i j
let of_string str = Label str
let to_string (Label str) = str
let of_int i = string_of_int i |> of_string

module Assoc = struct
  type 'a assoc = (t * 'a) list [@@deriving eq, compare, yojson, hash]
  type 'a t = 'a assoc [@@deriving eq, compare, yojson, hash]

  let iter : 'a t -> f:('a -> unit) -> unit =
   fun xs ~f -> List.iter ~f:(fun (_, x) -> f x) xs


  let map : 'a t -> f:('a -> 'b) -> 'b t =
   fun xs ~f -> List.map xs ~f:(fun (l, t) -> l, f t)


  let fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b =
   fun xs ~init ~f -> List.fold xs ~f:(fun init (_, t) -> f init t) ~init


  let fold_map : 'a t -> init:'b -> f:('b -> 'a -> 'b * 'c) -> 'b * 'c t =
   fun xs ~init ~f ->
    List.fold_map
      xs
      ~f:(fun init (l, t) ->
        let init, t = f init t in
        init, (l, t))
      ~init


  let to_list xs = xs
end