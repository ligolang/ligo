module Location = Simple_utils.Location

module T = struct
  type t = Label of string * (Location.t[@eq.ignore] [@hash.ignore])
  [@@deriving eq, yojson, hash, sexp, iter, bin_io]

  (* hack to order tuples correctly (tuples are currently encoded as
     records with field names equal to int_of_string of the
     indices) *)
  let compare (Label (l1, _) : t) (Label (l2, _) : t) : int =
    (* arbitrary choice: strings come before ints *)
    match int_of_string_opt l1, int_of_string_opt l2 with
    | None, Some _ -> -1
    | Some _, None -> 1
    | None, None -> String.compare l1 l2
    | Some i1, Some i2 -> Int.compare i1 i2


  let join (Label (l1, _)) (Label (l2, _)) = Label (l1 ^ l2, Location.generated)
  let create ?(loc = Location.generated) x = Label (x, loc)
end

include T
include Comparable.Make (T)

module Set = struct
  include Set

  type yojson__list = T.t list [@@deriving yojson]

  let to_yojson t = yojson__list_to_yojson @@ Core.Set.to_list t

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
    M.to_yojson @@ Core.Map.to_alist t


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
    | `Duplicate_key (Label (label, _)) -> Error ("Duplicate label: " ^ label)


  let fold_map t ~init ~f =
    Core.Map.fold t ~init:(init, empty) ~f:(fun ~key ~data (acc, map) ->
        let acc, data = f ~key ~data acc in
        let map = Core.Map.set map ~key ~data in
        acc, map)

  type 'a t_aux = (T.t * 'a) list [@@deriving bin_io]

  let bin_shape_t = bin_shape_t_aux

  let bin_size_t (bin_size_a : 'a Bin_prot.Size.sizer) (t : 'a t) =
    bin_size_t_aux bin_size_a @@ Core.Map.to_alist t

  let bin_write_t (bin_write_a : 'a Bin_prot.Write.writer) buf ~pos (t : 'a t) =
    bin_write_t_aux bin_write_a buf ~pos @@ Core.Map.to_alist t

  let bin_read_t (bin_read_a : 'a Bin_prot.Read.reader) buf ~pos_ref =
    let alist = bin_read_t_aux bin_read_a buf ~pos_ref in
    of_alist_exn alist
end

let pp ppf (l : t) : unit =
  let (Label (l, _)) = l in
  Format.fprintf ppf "%s" l


let range i j =
  List.map ~f:(fun i -> Label (string_of_int i, Location.generated)) @@ List.range i j


let of_string str = T.create str
let to_string (Label (str, _)) = str
let of_int i = string_of_int i |> of_string
let of_z i = Z.to_string i |> of_string

module Assoc = struct
  type 'a assoc = (t * 'a) list [@@deriving eq, compare, yojson, hash, sexp, bin_io]
  type 'a t = 'a assoc [@@deriving eq, compare, yojson, hash, sexp, bin_io]

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
