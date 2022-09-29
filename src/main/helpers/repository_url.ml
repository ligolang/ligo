(* TODO: write samll parser of repository URL *)
 
type type_url_dir = 
  { type_     : string [@key "type"]
  ; url       : string 
  ; directory : string option 
  } [@@deriving yojson]

(* To run only the expect tests ` opam exec -- dune runtest src/main/helpers/ --watch` *)

let parse_uri_with_scheme repo_short =
  let uri = Uri.of_string repo_short in
  if Uri.equal uri Uri.empty then
    None
  else
    match Uri.scheme uri, Uri.userinfo uri, Uri.host uri, Uri.port uri
      , Uri.path uri, Uri.query uri, Uri.fragment uri with
    | Some ("http" | "https"), (Some _ | None), (Some _ | None), (Some _ | None), _, [], None ->
      Some repo_short
    | Some "ssh", (Some _ | None), Some host, Some port, path, [], None ->
      let uri = Format.sprintf "https://%s:%d%s" host port path in
      Some uri
    | Some "ssh", (Some _ | None), Some host, None, path, [], None ->
      let uri = Format.sprintf "https://%s/%s" host path in
      Some uri
    | _ -> None

let parse_specific_short_url short =
  if String.is_prefix short ~prefix:"gist:" 
    then 
      Option.map ~f:(fun g -> Format.sprintf "https://gist.github.com/%s"g) 
      @@ String.chop_prefix short ~prefix:"gist:"
  else if String.is_prefix short ~prefix:"github:" 
  then 
    Option.map ~f:(fun g -> Format.sprintf "https://github.com/%s"g) 
    @@ String.chop_prefix short ~prefix:"github:"
  else if String.is_prefix short ~prefix:"gitlab:"
  then
    Option.map ~f:(fun g -> Format.sprintf "https://gitlab.com/%s"g) 
    @@ String.chop_prefix short ~prefix:"gitlab:"
  else if String.is_prefix short ~prefix:"bitbucket:"
  then
    Option.map ~f:(fun g -> Format.sprintf "https://bitbucket.org/%s"g) 
    @@ String.chop_prefix short ~prefix:"bitbucket:"
  else
    match String.split short ~on:'/' with
      [user;repo] -> 
        let url = Format.sprintf "https://github.com/%s/%s" user repo in
        Some url
    | _ -> None 

let parse repo_short : type_url_dir option =
  let type_ = "git" in
  let directory = None in
  match parse_uri_with_scheme repo_short with
    Some url -> Some { type_ ; url ; directory }
  | None when String.is_prefix repo_short ~prefix:"git@" -> 
    let repo_short = String.chop_prefix_exn repo_short ~prefix:"git@" in
    (match String.split repo_short ~on:':' with
      [host;user_repo] -> 
        let url = Format.sprintf "https://%s/%s" host user_repo in
        Some { type_ ; url ; directory }
    | _ -> None
    )
  | None -> (
    match parse_specific_short_url repo_short with
      Some url -> Some { type_ ; url ; directory }
    | None -> None 
  )

let%test _ =
  let short = "https://github.com/npm/cli.git" in
  match parse short with
  | Some { type_ ; url ; directory } 
      when 
        String.(type_ = "git") &&
        Option.is_none directory &&
        String.(url = short) -> true
  | _ -> false

let%test _ =
  let short = "git@github.com:ligolang/ligo-mirror.git" in
  match parse short with
  | Some { type_ ; url ; directory } 
      when 
        String.(type_ = "git") &&
        Option.is_none directory &&
        String.(url = "https://github.com/ligolang/ligo-mirror.git") -> true
  | _ -> false


let%test _ =
  let short = "ssh://login@server.com:12345/~/repository.git" in
  match parse short with
  | Some { type_ ; url ; directory } 
      when 
        String.(type_ = "git") &&
        Option.is_none directory &&
        String.(url = "https://server.com:12345/~/repository.git") -> true
  | _ -> false

let%test _ =
  let short = "gist:11081aaa281" in
  match parse short with
  | Some { type_ ; url ; directory } 
      when 
        String.(type_ = "git") &&
        Option.is_none directory &&
        String.(url = "https://gist.github.com/11081aaa281") -> true
  | _ -> false

let%test _ =
  let short = "gist:melwyn95/6ec752891ed445e46fa069970189a621" in
  match parse short with
  | Some { type_ ; url ; directory } 
      when 
        String.(type_ = "git") &&
        Option.is_none directory &&
        String.(url = "https://gist.github.com/melwyn95/6ec752891ed445e46fa069970189a621") -> true
  | _ -> false

let%test _ =
  let short = "github:user/repo" in
  match parse short with
  | Some { type_ ; url ; directory } 
      when 
        String.(type_ = "git") &&
        Option.is_none directory &&
        String.(url = "https://github.com/user/repo") -> true
  | _ -> false

let%test _ =
  let short = "gitlab:user/repo" in
  match parse short with
  | Some { type_ ; url ; directory } 
      when 
        String.(type_ = "git") &&
        Option.is_none directory &&
        String.(url = "https://gitlab.com/user/repo") -> true
  | _ -> false

let%test _ =
  let short = "bitbucket:user/repo" in
  match parse short with
  | Some { type_ ; url ; directory } 
      when 
        String.(type_ = "git") &&
        Option.is_none directory &&
        String.(url = "https://bitbucket.org/user/repo") -> true
  | _ -> false

let%test _ =
  let short = "npm/npm" in
  match parse short with
  | Some { type_ ; url ; directory } 
      when 
        String.(type_ = "git") &&
        Option.is_none directory &&
        String.(url = "https://github.com/npm/npm") -> true
  | _ -> false