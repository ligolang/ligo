module Constants = struct
  type command = (string * string array)
  let ligo_install_path = "./.ligo"
  let ligo_rc_path = Filename.concat (Sys.home_directory ()) ".ligorc"
  let ligo_registry = "https://beta.packages.ligolang.org/-/api"
  let esy = "esy"
  let windows = "Win32"
  let esy_add = fun ~package_name ~cache_path ~ligo_registry -> 
    ("", [|"esy"; "add"; package_name; "--prefix-path"; cache_path; "--npm-registry"; ligo_registry|])
  let esy_install = fun ~cache_path ~ligo_registry -> 
    ("", [|"esy"; "install"; "--prefix-path"; cache_path ;"--npm-registry"; ligo_registry|])
  let where = fun ~cmd -> 
    ("", [|"where"; "/q"; cmd|])
  let which = fun ~cmd -> 
    ("", [|"which"; cmd|])
  let git_clone = fun ~project_url ~project_name -> 
    ("", [|"git"; "clone"; project_url ; project_name|])
  let git_checkout = fun ~dir_path ~ref -> 
    ("", [|"git"; "--git-dir"; dir_path ;"checkout"; ref|])  
  
end

module LigoRC = struct
  module SMap = Caml.Map.Make(String)
  type token = string

  type t =
    { entries : token SMap.t
    ; path    : string }

  let get_token ~registry_key lrc =
    SMap.find_opt registry_key lrc.entries

  let read ~ligorc_path =
    let entries = try Some (In_channel.read_lines ligorc_path) with _ -> None in
    match entries with
      None -> { entries = SMap.empty ; path = ligorc_path}
    | Some entries ->
      let r = Str.regexp "//\\(.*\\):_authToken=\"\\(.*\\)\"" in
      let entries = List.fold_left entries ~init:SMap.empty ~f:(fun lrc e -> 
        if Str.string_match r e 0 
        then
          let uri = Str.matched_group 1 e in
          let token = Str.matched_group 2 e in
          SMap.add uri token lrc
        else lrc)
      in
      { entries ; path = ligorc_path}

  let update_token ~registry_key ~token ligorc =
    { ligorc with entries = SMap.update registry_key (fun _ -> Some token) ligorc.entries }
  
  let write ligorc =
    let { path ; entries } = ligorc in
    let entries = SMap.fold (fun registry_key token acc -> 
      Format.sprintf "%s//%s:_authToken=\"%s\"\n" acc registry_key token ) entries "" in
    print_endline entries;
    Out_channel.write_all path ~data:entries

  let registry_key = Str.global_replace (Str.regexp "\\(https?://\\)") ""
end

module LigoManifest = struct
  type t =
    { name               : string
    ; version            : string
    ; description        : string
    ; scripts            : (string * string) list
    ; author             : string
    ; license            : string
    ; readme             : string
    ; ligo_manifest_path : string
    } [@@deriving to_yojson]
  
  let is_empty field value =
    if String.equal value "" 
    then failwith (Format.sprintf "ERROR: %s is \"\" in package.json" field)
    else ()

  let is_version_correct version =
    if Option.is_none @@ Semver.of_string version 
    then failwith (Format.sprintf "ERROR: invalid version %s in package.json" version)
    else ()

  let validate t =
    let { name ; version ; author ; _ } = t in
    is_empty "name" name;
    is_empty "author" author;
    is_empty "version" version;
    is_version_correct version;
    t

  let read ~project_root =
    match project_root with
      None -> failwith "No package.json found!"
    | Some project_root ->
    let ligo_manifest_path = Filename.concat project_root "package.json" in
    let json = try Yojson.Safe.from_file ligo_manifest_path 
      with _ -> failwith "No package.json found!" in

    let module Util = Yojson.Safe.Util in    
    let name = try json |> Util.member "name" |> Util.to_string 
      with _ -> "No name field in package.json" in
    let version = try json |> Util.member "version" |> Util.to_string
      with _ -> "No version field in package.json'"  in
    let description = try json |> Util.member "description" |> Util.to_string
      with _ -> "" in
    let scripts = try json 
      |> Util.member "scripts" 
      |> Util.to_assoc 
      |> List.Assoc.map ~f:(Util.to_string)
      with _ -> []  in
    let author = try json |> Util.member "author" |> Util.to_string 
      with _ -> failwith "No author field  in package.json" in
    let license = try json |> Util.member "license" |> Util.to_string
      with _ -> failwith "No license field in package.json" in

    let readme = try json |> Util.member "readme" |> Util.to_string
      with _ -> "ERROR: No README data found!" in 
    { name ; version ; description ; scripts ; author ; license ; readme ; ligo_manifest_path }
end

let find_project_root () =
  let pwd = Unix.getcwd in
  let rec aux p =
    let dirs = Sys.ls_dir p in
    if List.exists ~f:(String.equal "package.json") dirs
    then Some p
    else
      let p' = Filename.dirname p in
      (* Check if we reached the root directory, since the parent of 
         the root directory is the root directory itself *)
      if Filename.equal p p'
      then None
      else aux p'
  in
  try aux (pwd ()) 
  (* In case of permission issues when reading file, catch the exception *)
  with _ -> None 

let return_good ?output_file v = 
  let fmt : Format.formatter = match output_file with
    | Some file_path -> Format.formatter_of_out_channel @@ Out_channel.create file_path
    | None -> Format.std_formatter in
  Format.fprintf fmt "%s\n" v; Format.pp_print_flush fmt ()

let return_bad v : unit = (
  if Char.(v.[String.length v - 1] = '\n') then
    Format.eprintf "%s" v
  else
    Format.eprintf "%s\n" v;
    Format.pp_print_flush Format.err_formatter ();
  )
let return_with_warn ~show_warnings warns f =
  if not (String.length (String.strip warns) = 0) && show_warnings then
    begin
      Format.eprintf "%s\n" warns;
      Format.pp_print_flush Format.err_formatter ()
    end;
  f ()

type return = Done | Compileur_Error | Exception of exn
let return_result : return:return ref -> ?show_warnings:bool -> ?output_file:string ->(unit -> ('value, _) result) -> unit =
  fun ~return ?(show_warnings=false) ?output_file f ->
    try 
      match f () with
      | Ok    (v,w) -> return:=Done; return_with_warn ~show_warnings w (fun () -> return_good ?output_file v)
      | Error (e,w) -> return:=Compileur_Error; return_with_warn ~show_warnings w (fun () -> return_bad e)
    with exn -> return := Exception exn;;

type command = (string * string array)

(* Checks if executable is present *)
let does_command_exist (cmd : string) =
  let cmd = 
    if String.equal Sys.os_type Constants.windows then
      Constants.where ~cmd
    else
      Constants.which ~cmd in
  let exit = Lwt_process.exec cmd in
  let status = Lwt_main.run exit in
  match status with
    WEXITED 0 -> Ok true
  | WEXITED 1 -> Ok false
  | _ -> Error "unknown error"

(* Runs a commands in a separate process *)
let run_command (cmd : command) =
  let status = Lwt_process.with_process_none ~stdout:`Keep ~stderr:`Keep cmd 
    (fun p -> Lwt.map  
      (fun status -> 
        match status with
          Caml.Unix.WEXITED 0 -> Ok ()
        | _ -> Error ("unknown error"))
        p#status) in
  Lwt_main.run status