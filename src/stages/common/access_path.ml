type 'e access =
  | Access_tuple of X_z.t
  | Access_record of string
  | Access_map of 'e
  [@@deriving eq,compare,yojson,hash]

type 'e t = 'e access list
  [@@deriving eq,compare,yojson,hash]

let pp_access f ppf a =
  match a with
    | Access_tuple i  -> Format.fprintf ppf "%a" Z.pp_print i
    | Access_record s -> Format.fprintf ppf "%s" s
    | Access_map e    -> Format.fprintf ppf "[%a]" f e

let pp f ppf a =
  Format.fprintf ppf "%a"
    Simple_utils.PP_helpers.(list_sep (pp_access f) (const "."))
    a
let fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
= fun f acc path ->
  let aux acc a = match a with
    | Access_record _ ->  acc
    | Access_tuple  _ ->  acc
    | Access_map e ->
      let acc = f acc e in
       acc
  in
  List.fold ~f:aux ~init:acc path

let map : ('a -> 'b) -> 'a t -> 'b t
= fun f path ->
  let aux a = match a with
    | Access_record s -> Access_record s
    | Access_tuple  i -> Access_tuple  i
    | Access_map e ->
      let e = f e in
      Access_map e
  in
  List.map ~f:aux path

let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
= fun f acc path ->
  let aux acc a = match a with
    | Access_record s ->  (acc,Access_record s)
    | Access_tuple  i ->  (acc,Access_tuple  i)
    | Access_map e ->
      let acc,e = f acc e in
      (acc,Access_map e)
  in
  List.fold_map ~f:aux ~init:acc path
