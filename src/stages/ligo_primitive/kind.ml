type t =
    Type
  | Singleton
[@@deriving yojson,equal,compare,hash]

let pp ppf (_ : t) : unit =
  Format.fprintf ppf "*"
