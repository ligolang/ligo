module T = struct
  type t = Label of string [@@deriving eq, yojson, hash, sexp, iter, sexp]

  (* hack to order tuples correctly (tuples are currently encoded as
     records with field names equal to int_of_string of the
     indices) *)
  let compare (Label l1 : t) (Label l2 : t) : int =
    (* arbitrary choice: strings come before ints *)
    match int_of_string_opt l1, int_of_string_opt l2 with
    | None, Some _ -> -1
    | Some _, None -> 1
    | None, None -> String.compare l1 l2
    | Some i1, Some i2 -> Int.compare i1 i2


  let join (Label l1) (Label l2) = Label (l1 ^ l2)
  let create x = Label x
end

include T
include Comparable.Make (T)

module Set = struct
  include Set

  type yojson__list = T.t list [@@deriving yojson]

  let to_yojson t = yojson__list_to_yojson @@ to_list t

  let of_yojson json =
    let open Result.Let_syntax in
    let%map list = yojson__list_of_yojson json in
    of_list list


  let hash_fold_t state t = Hash.fold_int state (Hashtbl.hash t)
  let hash = Hashtbl.hash
end

module Map = struct
  include Map

  let to_yojson (type a) (a_to_yojson : a -> Yojson.Safe.t) (t : a t) =
    (* Hack to get [ppx_deriving_yojson] to give me a local conv. If we switched to JS ppx, we wouldn't need this *)
    let module M = struct
      type t = (T.t * a) list [@@deriving to_yojson]
    end
    in
    M.to_yojson @@ to_alist t


  let of_yojson
      (type a)
      (a_of_yojson : Yojson.Safe.t -> (a, string) result)
      (json : Yojson.Safe.t)
    =
    let open Result.Let_syntax in
    let module M = struct
      type t = (T.t * a) list [@@deriving of_yojson]
    end
    in
    let%bind alist = M.of_yojson json in
    match of_alist alist with
    | `Ok t -> Ok t
    | `Duplicate_key (Label label) -> Error ("Duplicate label: " ^ label)


  let fold_map t ~init ~f =
    fold t ~init:(init, empty) ~f:(fun ~key ~data (acc, map) ->
        let acc, data = f ~key ~data acc in
        let map = Map.set map ~key ~data in
        acc, map)
end

let pp ppf (l : t) : unit =
  let (Label l) = l in
  Format.fprintf ppf "%s" l


let range i j = List.map ~f:(fun i -> Label (string_of_int i)) @@ List.range i j
let of_string str = Label str
let to_string (Label str) = str
let of_int i = string_of_int i |> of_string
let of_z i = Z.to_string i |> of_string

module Assoc = struct
  type 'a assoc = (t * 'a) list [@@deriving eq, compare, yojson, hash, sexp]
  type 'a t = 'a assoc [@@deriving eq, compare, yojson, hash, sexp]

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
