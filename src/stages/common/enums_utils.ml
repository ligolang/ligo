type ligo_string = Simple_utils.Ligo_string.t

type z = Z.t 
let [@warning "-32"] z_to_yojson x = `String (Z.to_string x)
let [@warning "-32"] z_of_yojson x =
  try match x with
      | `String s -> Ok (Z.of_string s)
      | _ -> Utils.error_yojson_format "JSON string"
  with
  | Invalid_argument _ ->
     Error "Invalid formatting.
            The Zarith library does not know how to handle this formatting."

let bytes_to_yojson b = `String (Bytes.to_string b)
