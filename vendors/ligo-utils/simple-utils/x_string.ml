type t =
  Standard of string
| Verbatim of string

let pp ppf = function
    Standard s -> Format.fprintf ppf "%S" s
  | Verbatim v -> Format.fprintf ppf "{|%s|}" v

let compare ?(compare=compare) a b = match a,b with
    (Standard a, Standard b) -> compare a b
  | (Standard _, Verbatim _) -> -1
  | (Verbatim _, Standard _) -> 1
  | (Verbatim a, Verbatim b) -> compare a b

let extract = function
    Standard s -> s
  | Verbatim v -> v
