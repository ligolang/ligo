include Uri

let of_yojson = function
  | `String s -> Ok (Uri.of_string s)
  | y -> Error ("Expected string: but got " ^ Yojson.Safe.to_string y)


let to_yojson a = `String (Uri.to_string a)
