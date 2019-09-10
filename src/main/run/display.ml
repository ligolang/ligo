open Trace

let dev = false

let rec error_pp out (e : error) =
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
      (Format.pp_print_list error_pp) infos
      (Format.pp_print_list error_pp) children
  )

