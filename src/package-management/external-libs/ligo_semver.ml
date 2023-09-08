include Semver

let equal v1 v2 = compare v1 v2 = 0
let to_yojson s = `String (to_string s)

let of_yojson y =
  match y with
  | `String s ->
    (match of_string s with
    | Some s -> Ok s
    | None -> Error "Semver.of_yojson failed: Semver.of_string failed")
  | _ -> Error "Semver.of_yojson failed: didn't received String as expected"
