open Core

type token = string

type t =
  { entries : token String.Map.t
  ; path : string
  }

let get_token ~registry_key lrc = Map.find lrc.entries registry_key

let read ~ligorc_path =
  let entries =
    try Some (In_channel.read_lines ligorc_path) with
    | _ -> None
  in
  match entries with
  | None -> { entries = String.Map.empty; path = ligorc_path }
  | Some entries ->
    let r = Str.regexp "//\\(.*\\):_authToken=\"\\(.*\\)\"" in
    let entries =
      List.fold_left entries ~init:String.Map.empty ~f:(fun lrc e ->
          if Str.string_match r e 0
          then (
            let uri = Str.matched_group 1 e in
            let token = Str.matched_group 2 e in
            Map.set lrc ~key:uri ~data:token)
          else lrc)
    in
    { entries; path = ligorc_path }


let update_token ~registry_key ~token ligorc =
  { ligorc with
    entries = Map.change ligorc.entries registry_key ~f:(fun _ -> Some token)
  }


let write ligorc =
  let { path; entries } = ligorc in
  let entries =
    Map.fold
      entries
      ~f:(fun ~key:registry_key ~data:token acc ->
        Format.sprintf "%s//%s:_authToken=\"%s\"\n" acc registry_key token)
      ~init:""
  in
  Out_channel.write_all path ~data:entries


let registry_key uri =
  uri
  |> Uri.with_uri ~scheme:None ~path:(Some "/-/api")
  |> Uri.to_string
  |> Str.global_replace (Str.regexp "\\(^//\\)") ""
