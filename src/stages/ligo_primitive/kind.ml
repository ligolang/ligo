type t =
  | Type
  | Singleton
  | Arrow of t * t
[@@deriving yojson, equal, compare, hash]

let rec pp ppf t =
  match t with
  | Type | Singleton -> Format.fprintf ppf "*"
  | Arrow (t1, t2) -> Format.fprintf ppf "%a -> %a" pp t1 pp t2
