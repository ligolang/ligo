open Core

type t = Z.t [@@deriving equal, compare]

let[@warning "-32"] to_yojson x = `String (Z.to_string x)

let error_yojson_format format =
  Error ("Invalid JSON value.
          An object with the following specification is expected:"
         ^ format)

let[@warning "-32"] of_yojson x =
  try
    match x with
    | `String s -> Ok (Z.of_string s)
    | _ -> error_yojson_format "JSON string"
  with
  | Invalid_argument _ ->
    Error
      "Invalid formatting.\n\
      \            The Zarith library does not know how to handle this formatting."

let sexp_of_t t = Sexp.Atom (Z.to_string t)

let t_of_sexp sexp =
  match sexp with
  | Sexp.Atom str -> Z.of_string str
  | _ -> failwith "[Ligo_z.t_of_sexp] Invalid sexp encoding"

let hash_fold_t st z = Hash.fold_int64 st (Z.to_int64 z)
