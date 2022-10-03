(* To run only the expect tests `opam exec -- dune runtest src/main/helpers/ --watch` *)

type t =
  { type_ : string [@key "type"]
  ; url : string
  ; directory : string option
  }
[@@deriving yojson]

type type_url =
  { type_ : string [@key "type"]
  ; url : string
  }
[@@deriving yojson]

let github ~user ~repo = Format.sprintf "https://github.com/%s/%s" user repo
let gitlab ~user ~repo = Format.sprintf "https://gitlab.com/%s/%s" user repo

let bitbucket ~user ~repo =
  Format.sprintf "https://bitbucket.org/%s/%s" user repo


let gist ~hash ?user () =
  match user with
  | Some user -> Format.sprintf "https://gist.github.com/%s/%s" user hash
  | None -> Format.sprintf "https://gist.github.com/%s" hash


let parse_uri_with_scheme repo_short =
  let uri = Uri.of_string repo_short in
  if Uri.equal uri Uri.empty
  then None
  else (
    match
      ( Uri.scheme uri
      , Uri.userinfo uri
      , Uri.host uri
      , Uri.port uri
      , Uri.path uri
      , Uri.query uri
      , Uri.fragment uri )
    with
    | ( Some ("http" | "https")
      , (Some _ | None)
      , (Some _ | None)
      , (Some _ | None)
      , _
      , []
      , None ) -> Some repo_short
    | ( Some ("ssh" | "git+ssh")
      , (Some _ | None)
      , Some host
      , Some port
      , path
      , []
      , None ) ->
      let path =
        if String.is_prefix path ~prefix:"/" then path else "/" ^ path
      in
      let uri = Format.sprintf "https://%s:%d%s" host port path in
      Some uri
    | Some ("ssh" | "git+ssh"), (Some _ | None), Some host, None, path, [], None
      ->
      let path =
        if String.is_prefix path ~prefix:"/" then path else "/" ^ path
      in
      let uri = Format.sprintf "https://%s%s" host path in
      Some uri
    | _ -> None)


let parse_specific_short_url short =
  match String.split short ~on:':' with
  | [ "gist"; suffix ] ->
    (match String.split suffix ~on:'/' with
    | [ hash ] -> Some (gist ~hash ())
    | [ user; hash ] -> Some (gist ~user ~hash ())
    | _ -> None)
  | [ short_url ] ->
    (match String.split short_url ~on:'/' with
    | [ user; repo ] -> Some (github ~user ~repo)
    | _ -> None)
  | [ prefix; user_repo ] ->
    (match String.split user_repo ~on:'/' with
    | [ user; repo ] ->
      (match prefix with
      | "github" -> Some (github ~user ~repo)
      | "gitlab" -> Some (gitlab ~user ~repo)
      | "bitbucket" -> Some (bitbucket ~user ~repo)
      | unsupported_prefix ->
        failwith ("Unsupported URI shorthand: " ^ unsupported_prefix))
    | _ -> None)
  | _ -> None


let parse_url url =
  match parse_uri_with_scheme url with
  | Some url -> Some url
  | None when String.is_prefix url ~prefix:"git@" ->
    let repo_short = String.chop_prefix_exn url ~prefix:"git@" in
    (match String.split repo_short ~on:':' with
    | [ host; user_repo ] ->
      let url = Format.sprintf "https://%s/%s" host user_repo in
      Some url
    | _ -> None)
  | None ->
    (match parse_specific_short_url url with
    | Some url -> Some url
    | None -> None)


let parse' j : t option =
  let module Util = Yojson.Safe.Util in
  let directory = None in
  match type_url_of_yojson j with
  | Ok { type_; url } ->
    let url = parse_url url in
    Option.map url ~f:(fun url -> { type_; url; directory })
  | Error _ ->
    let short = Util.to_string j in
    let type_ = "git" in
    let url = parse_url short in
    Option.map url ~f:(fun url -> { type_; url; directory })


let parse j =
  match of_yojson j with
  | Ok tud ->
    (* Validate url ?? *)
    Ok tud
  | Error _ ->
    (match parse' j with
    | Some t -> Ok t
    | None -> Error "repository url is invalid")


let%test _ =
  let short = "https://github.com/npm/cli.git" in
  match parse_url short with
  | Some url when String.(url = short) -> true
  | _ -> false

let%test _ =
  let short = "git@github.com:ligolang/ligo-mirror.git" in
  match parse_url short with
  | Some url when String.(url = "https://github.com/ligolang/ligo-mirror.git")
    -> true
  | _ -> false

let%test _ =
  let short = "ssh://login@server.com:12345/~/repository.git" in
  match parse_url short with
  | Some url when String.(url = "https://server.com:12345/~/repository.git") ->
    true
  | _ -> false

let%test _ =
  let short = "ssh://git@bitbucket.org/accountname/reponame.git" in
  match parse_url short with
  | Some url
    when String.(url = "https://bitbucket.org/accountname/reponame.git") -> true
  | _ -> false

let%test _ =
  let short = "git+ssh://git@gitlab.example.com/path/repo.git" in
  match parse_url short with
  | Some url when String.(url = "https://gitlab.example.com/path/repo.git") ->
    true
  | _ -> false

let%test _ =
  let short = "git+ssh://git@gitlab.example.com:path/repo.git" in
  match parse_url short with
  | Some url when String.(url = "https://gitlab.example.com/path/repo.git") ->
    true
  | _ -> false

let%test _ =
  let short = "gist:11081aaa281" in
  match parse_url short with
  | Some url when String.(url = "https://gist.github.com/11081aaa281") -> true
  | _ -> false

let%test _ =
  let short = "gist:melwyn95/6ec752891ed445e46fa069970189a621" in
  match parse_url short with
  | Some url
    when String.(
           url
           = "https://gist.github.com/melwyn95/6ec752891ed445e46fa069970189a621")
    -> true
  | _ -> false

let%test _ =
  let short = "github:user/repo" in
  match parse_url short with
  | Some url when String.(url = "https://github.com/user/repo") -> true
  | _ -> false

let%test _ =
  let short = "gitlab:user/repo" in
  match parse_url short with
  | Some url when String.(url = "https://gitlab.com/user/repo") -> true
  | _ -> false

let%test _ =
  let short = "bitbucket:user/repo" in
  match parse_url short with
  | Some url when String.(url = "https://bitbucket.org/user/repo") -> true
  | _ -> false

let%test _ =
  let short = "npm/npm" in
  match parse_url short with
  | Some url when String.(url = "https://github.com/npm/npm") -> true
  | _ -> false

(* JSON tests*)

let%test _ =
  let short =
    Yojson.Safe.from_string
      "{\"type\":\"git\",\"url\":\"https://github.com/npm/cli.git\",\"directory\":\"foor/bar\"}"
  in
  match parse short with
  | Ok { type_; url; directory }
    when String.(type_ = "git")
         && Option.equal String.equal directory (Some "foo/bar")
         && String.(url = "https://github.com/npm/cli.git") -> true
  | _ -> false

(* 2 keys type, url *)
let%test _ =
  let short = Yojson.Safe.from_string "{}" in
  match parse short with
  | Ok { type_; url; directory }
    when String.(type_ = "git")
         && Option.is_none directory
         && String.(url = "https://server.com:12345/~/repository.git") -> true
  | _ -> false

(* 1 keys url *)
let%test _ =
  let short = Yojson.Safe.from_string "{}" in
  match parse short with
  | Ok { type_; url; directory }
    when String.(type_ = "git")
         && Option.is_none directory
         && String.(url = "https://server.com:12345/~/repository.git") -> true
  | _ -> false

(* All 3 invalid url as it is *)
let%test _ =
  let short = Yojson.Safe.from_string "{}" in
  match parse short with
  | Ok { type_; url; directory }
    when String.(type_ = "git")
         && Option.is_none directory
         && String.(url = "https://server.com:12345/~/repository.git") -> true
  | _ -> false

(* 2 keys type, invalid url *)
let%test _ =
  let short = Yojson.Safe.from_string "{}" in
  match parse short with
  | Ok { type_; url; directory }
    when String.(type_ = "git")
         && Option.is_none directory
         && String.(url = "https://server.com:12345/~/repository.git") -> true
  | _ -> false

(* 1 keys invalid url *)
let%test _ =
  let short = Yojson.Safe.from_string "{}" in
  match parse short with
  | Ok { type_; url; directory }
    when String.(type_ = "git")
         && Option.is_none directory
         && String.(url = "https://server.com:12345/~/repository.git") -> true
  | _ -> false

let%test _ =
  let short = Yojson.Safe.from_string "null" in
  match parse short with
  | Ok { type_; url; directory }
    when String.(type_ = "git")
         && Option.is_none directory
         && String.(url = "https://server.com:12345/~/repository.git") -> true
  | _ -> false
