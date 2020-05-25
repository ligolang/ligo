type t =
  Standard of string
| Verbatim of string

let pp ppf = function
    Standard s -> Format.fprintf ppf "%S" s
  | Verbatim v -> Format.fprintf ppf "{|%s|}" v

let extract = function
    Standard s -> s
  | Verbatim v -> v
