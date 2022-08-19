include Z

let [@warning "-32"] to_yojson x = `String (Z.to_string x)
let [@warning "-32"] of_yojson x =
  try match x with
    | `String s -> Ok (Z.of_string s)
    | _ -> Simple_utils.Utils.error_yojson_format "JSON string"
  with
  | Invalid_argument _ ->
    Error "Invalid formatting.
            The Zarith library does not know how to handle this formatting."

let hash_fold_t st z = Hash.fold_int64 st (Z.to_int64 z)
