open! Trace

let rec error_pp ?(dev = false) out (e : error) =
  let open JSON_string_utils in
  let message =
    let opt = e |> member "message" |> string in
    match opt with
    | Some msg -> ": " ^ msg
    | None -> "" in
  let error_code =
    let error_code = e |> member "error_code" in
    match error_code with
    | `Null -> ""
    | _ -> " (" ^ (J.to_string error_code) ^ ")" in
  let title =
    let opt = e |> member "title" |> string in
    Option.unopt ~default:"" opt in
  let data =
    let data = e |> member "data" in
    match data with
    | `Null -> ""
    | _ -> " " ^ (J.to_string data) ^ "\n" in
  let infos =
    let infos = e |> member "infos" in
    match infos with
    | `List lst -> lst
    | `Null -> []
    | x -> [ x ] in
  let children =
    let infos = e |> member "children" in
    match infos with
    | `List lst -> lst
    | `Null -> []
    | x -> [ x ] in
  let location =
    let opt = e |> member "data" |> member "location" |> string in
    let aux prec cur =
      match prec with
      | None -> cur |> member "data" |> member "location" |> string
      | Some s -> Some s
    in
    match List.fold_left aux opt infos with
    | None -> ""
    | Some s -> s ^ ". "
  in
  let print x = Format.fprintf out x in
  if not dev then (
    print "%s%s%s%s%s" location title error_code message data
  ) else (
    print "%s%s%s.\n%s%s\n%a\n%a\n" title error_code message data location
      (Format.pp_print_list (error_pp ~dev)) infos
      (Format.pp_print_list (error_pp ~dev)) children
  )

let result_pp_hr f out (r : _ result) =
  match r with
  | Ok (s , _) -> Format.fprintf out "%a" f s
  | Error e -> Format.fprintf out "%a" (error_pp ~dev:false) (e ())

let string_result_pp_hr = result_pp_hr (fun out s -> Format.fprintf out "%s" s)

let result_pp_dev f out (r : _ result) =
  match r with
  | Ok (s , _) -> Format.fprintf out "%a" f s
  | Error e -> Format.fprintf out "%a" (error_pp ~dev:true) (e ())

let string_result_pp_dev = result_pp_hr (fun out s -> Format.fprintf out "%s" s)

let json_pp out x = Format.fprintf out "%s" (J.to_string x)

let string_result_pp_json out (r : string result) =
  let status_json status content : J.t = `Assoc ([
      ("status" , `String status) ;
      ("content" , content) ;
    ]) in
  match r with
  | Ok (x , _) -> (
      Format.fprintf out "%a" json_pp (status_json "ok" (`String x))
    )
  | Error e -> (
      Format.fprintf out "%a" json_pp (status_json "error" (e ()))
    )

type display_format = [
  | `Human_readable
  | `Json
  | `Dev
]

let formatted_string_result_pp (display_format : display_format) =
  match display_format with
  | `Human_readable -> string_result_pp_hr
  | `Dev -> string_result_pp_dev
  | `Json -> string_result_pp_json

type michelson_format = [
  | `Text
  | `Json
  | `Hex
]

let michelson_pp (mf : michelson_format) = match mf with
  | `Text -> Michelson.pp
  | `Json -> Michelson.pp_json
  | `Hex -> Michelson.pp_hex
