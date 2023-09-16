module SMap = Caml.Map.Make (String)

type token = string

type t =
  { entries : token SMap.t
  ; path : string
  }

let get_token ~registry_key lrc = SMap.find_opt registry_key lrc.entries

let read ~ligorc_path =
  let entries =
    try Some (In_channel.read_lines ligorc_path) with
    | _ -> None
  in
  match entries with
  | None -> { entries = SMap.empty; path = ligorc_path }
  | Some entries ->
    let r = Str.regexp "//\\(.*\\):_authToken=\"\\(.*\\)\"" in
    let entries =
      List.fold_left entries ~init:SMap.empty ~f:(fun lrc e ->
          if Str.string_match r e 0
          then (
            let uri = Str.matched_group 1 e in
            let token = Str.matched_group 2 e in
            SMap.add uri token lrc)
          else lrc)
    in
    { entries; path = ligorc_path }


let update_token ~registry_key ~token ligorc =
  { ligorc with entries = SMap.update registry_key (fun _ -> Some token) ligorc.entries }


let write ligorc =
  let { path; entries } = ligorc in
  let entries =
    SMap.fold
      (fun registry_key token acc ->
        Format.sprintf "%s//%s:_authToken=\"%s\"\n" acc registry_key token)
      entries
      ""
  in
  Out_channel.write_all path ~data:entries


let registry_key uri =
  uri
  |> Uri.with_uri ~scheme:None ~path:(Some "/-/api")
  |> Uri.to_string
  |> Str.global_replace (Str.regexp "\\(//\\)") ""
