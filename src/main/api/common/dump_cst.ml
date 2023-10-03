open Simple_utils.Display
module Helpers = Ligo_compile.Helpers

let error_format = Main_errors.Formatter.error_format
let warn_format = Main_warnings.format

let json_list (list : 'a list) (format : 'a format) : json =
  `List
    (List.map list ~f:(fun value ->
         convert ~display_format:Json ~no_colour:true (Displayable { value; format })))


let to_json (cst : Dialect_cst.t) : json =
  let cst_json, lang =
    match cst with
    | CameLIGO cst -> Cst.Cameligo.yojson_of_cst cst, "CameLIGO"
    | JsLIGO cst -> Cst.Jsligo.yojson_of_cst cst, "JsLIGO"
  in
  `Assoc [ "cst", cst_json; "lang", `String lang ]


let rec filter_opaque (json : json) : json =
  let filter_assoc pairs =
    List.filter_map pairs ~f:(fun (k, v) ->
        match v with
        | `String "<opaque>" -> None
        (* Idk how to do it better *)
        | `List (`String "D_Directive" :: _) -> None
        | `List (`List (`String "S_Directive" :: _) :: _) -> None
        | v -> Some (k, filter_opaque v))
  in
  let filter_list xs =
    List.filter_map xs ~f:(fun v ->
        match v with
        | `String "<opaque>" -> None
        | `List (`String "D_Directive" :: _) -> None
        | `List (`List (`String "S_Directive" :: _) :: _) -> None
        | v -> Some (filter_opaque v))
  in
  match json with
  | `Assoc ps -> `Assoc (filter_assoc ps)
  | `List js -> `List (filter_list js)
  | `Tuple js -> `Tuple (filter_list js)
  | `Variant (s, j) -> `Variant (s, Option.map j ~f:filter_opaque)
  | x -> x


let rec yojson_to_msgpck (json : json) : Msgpck.t =
  let open Msgpck in
  match json with
  | `Null -> of_nil
  | `Bool b -> of_bool b
  | `Int n -> of_int n
  | `Intlit n -> of_string n
  | `Float n -> of_float n
  | `String s -> of_string s
  | `Assoc mp ->
    of_map @@ List.map mp ~f:(fun (name, v) -> of_string name, yojson_to_msgpck v)
  | `List lst -> of_list @@ List.map lst ~f:yojson_to_msgpck
  | `Tuple lst -> of_list @@ List.map lst ~f:yojson_to_msgpck
  | `Variant (name, arg) ->
    let msgpck_arg =
      match arg with
      | None -> []
      | Some x -> [ yojson_to_msgpck x ]
    in
    of_list @@ (of_string name :: msgpck_arg)


let cst_to_json : Dialect_cst.t list -> json =
 fun lst -> `List (List.map lst ~f:(fun cst -> filter_opaque @@ to_json cst))


let output_format : Dialect_cst.t list format =
  { pp =
      (fun ~display_format ~no_colour f lst ->
        Parsing.Formatter.ppx_ppformat
          ~display_format
          ~no_colour
          f
          (Msgpck.BytesBuf.to_string @@ yojson_to_msgpck @@ cst_to_json lst))
  ; to_json = cst_to_json
  }
