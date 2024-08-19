open Core

type t =
  | Standard of string
  | Verbatim of string
[@@deriving hash, sexp, bin_io]

let standard : string -> t = fun s -> Standard s
let verbatim : string -> t = fun s -> Verbatim s

let pp ppf = function
  | Standard s -> Format.fprintf ppf "%S" s
  | Verbatim v -> Format.fprintf ppf "{|%s|}" v

let to_yojson = function
  | Standard s -> `List [ `String "Standard"; `String s ]
  | Verbatim v -> `List [ `String "Verbatim"; `String v ]

let error_yojson_format format =
  Error
    ("Invalid JSON value.\n\
     \          An object with the following specification is expected:"
    ^ format)

let of_yojson = function
  | `List [ `String "Standard"; `String s ] -> Ok (Standard s)
  | `List [ `String "Verbatim"; `String v ] -> Ok (Verbatim v)
  | _ -> error_yojson_format "Standard string | Verbatim string"

let compare ?(compare = String.compare) a b =
  match a, b with
  | Standard a, Standard b -> compare a b
  | Standard _, Verbatim _ -> -1
  | Verbatim _, Standard _ -> 1
  | Verbatim a, Verbatim b -> compare a b

let equal a b =
  match a, b with
  | Standard a, Standard b | Verbatim a, Verbatim b -> String.equal a b
  | Standard _, Verbatim _ | Verbatim _, Standard _ -> false

let extract = function
  | Standard s -> s
  | Verbatim v -> v

let get_type = function
  | Standard _ -> `Standard
  | Verbatim _ -> `Verbatim
