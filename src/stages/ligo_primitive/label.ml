type t = Label of string
  [@@deriving eq, compare, yojson, hash, sexp]

let pp ppf (l:t) : unit =
  let Label l = l in
  Format.fprintf ppf "%s" l

let _range i j =
  let rec aux i j acc = if i >= j then acc else aux i (j-1) (j-1 :: acc) in
  aux i j []

let range i j =
  List.map ~f:(fun i -> Label (string_of_int i)) @@ List.range i j

let of_string str = Label str
let to_string (Label str) = str
let of_int i =
  string_of_int i
  |> of_string
