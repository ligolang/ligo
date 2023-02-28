open Api_helpers

(* open Simple_utils.Display *)
module OS = Bos.OS
module Cmd = Bos.Cmd
module Trace = Simple_utils.Trace
module Constants = Cli_helpers.Constants
module T = Core
module Formatter = Ligo_formatter

type project_entity =
  [ `LIBRARY
  | `CONTRACT
  ]

module RegistryTemplate : sig
  type error =
    | PackageNotFound
    | ServerError
    | InvalidPackageInfo of string
    | IntegrityMismatch
    | InvalidTemplateType
    | UnableToDownload
    | UnableToUnzip

  val string_of_error : error -> string

  val init
    :  kind:project_entity
    -> template:string
    -> project_name:string
    -> ?version:string
    -> registry:string
    -> unit
    -> (string, error) result
end = struct
  open Lwt.Syntax
  open Cohttp_lwt
  open Cohttp_lwt_unix

  (* TODO: [later]
   1. Fix Handling of ~version overall for `ligo init`
   2. Handle pattern like <template-name>@<sem-ver> *)

  type error =
    | PackageNotFound
    | ServerError
    | InvalidPackageInfo of string
    | IntegrityMismatch
    | InvalidTemplateType
    | UnableToDownload
    | UnableToUnzip

  let string_of_error = function
    | PackageNotFound -> "Error: Package not found in registry"
    | ServerError -> "Error: registry server error"
    | InvalidPackageInfo s -> Format.sprintf "Error: Invalid package info\n%s" s
    | IntegrityMismatch -> "Error: integrity checksum failed"
    | InvalidTemplateType ->
      "Error: mis-match of template type.\n\
       Hint: Please check the ligo command `ligo init contract`/`ligo init library"
    | UnableToDownload -> "Error: unable to downloading template"
    | UnableToUnzip -> "Error: unable to write unzipped data to file"


  let ( let@ ) x f = Result.bind ~f x

  type fields =
    { _type : string
    ; tarball : Uri.t
    ; shasum : string
    }
  (* 
  4. Clean up formatter code
*)

  let package_info_endpoint ?version ~registry pkg =
    let uri =
      match version with
      | Some version -> Format.sprintf "%s/%s/%s" registry pkg version
      | None -> Format.sprintf "%s/%s" registry pkg
    in
    Uri.of_string uri


  let package_info ?version ~registry pkg =
    let endpoint = package_info_endpoint ?version ~registry pkg in
    let* resp, body = Client.get endpoint in
    let code = Cohttp.Code.code_of_status resp.status in
    let body =
      match code with
      | 404 -> Error PackageNotFound
      | code when code = Cohttp.Code.code_of_status resp.status ->
        Ok (Body.to_string body)
      | _ -> Error ServerError
    in
    match body with
    | Ok body ->
      let* body in
      Lwt.return_ok (Yojson.Safe.from_string body)
    | Error e -> Lwt.return_error e


  let get_fields ?version json =
    let open Yojson.Safe.Util in
    let@ _type, dist =
      match version with
      | None ->
        let dist_tags = member "dist-tags" json in
        let latest = member "latest" dist_tags in
        (match latest with
        | `String v ->
          let versions = member "versions" json in
          let version = member v versions in
          Ok (member "type" version, member "dist" version)
        | _ -> Error (InvalidPackageInfo "invalid package info"))
      | Some _ -> Ok (member "type" json, member "dist" json)
    in
    let@ _type =
      match _type with
      | `String t -> Ok t
      | _ -> Error (InvalidPackageInfo "invalid type field in package info")
    in
    let@ tarball, shasum =
      let tarball = member "tarball" dist in
      let shasum = member "shasum" dist in
      match tarball, shasum with
      | `String tarball, `String shasum -> Ok (tarball, shasum)
      | _ -> Error (InvalidPackageInfo "invalid tarbal/shasum in package info")
    in
    let tarball = Uri.of_string tarball in
    Ok { _type; tarball; shasum }


  let validate_template_type kind _type =
    match kind, _type with
    | `LIBRARY, "library" -> Ok ()
    | `CONTRACT, "contract" -> Ok ()
    | _ -> Error InvalidTemplateType


  let sha1 s = s |> Digestif.SHA1.digest_string |> Digestif.SHA1.to_hex
  let sha1_bytes s = s |> Digestif.SHA1.digest_bytes |> Digestif.SHA1.to_hex

  let check_integrity fname ~expected =
    let fd = Ligo_unix.openfile fname [ Ligo_unix.O_RDONLY ] 0 in
    let file_size = (Ligo_unix.stat fname).st_size in
    let buf = Bytes.create file_size in
    let rec read idx =
      let len = if idx + 512 > file_size then file_size - idx else 512 in
      let _ = Ligo_unix.read fd buf idx len in
      if len < 512 then () else read (idx + len)
    in
    let () = read 0 in
    let () = Ligo_unix.close fd in
    let got = sha1_bytes buf in
    if String.equal got expected then Ok () else Error IntegrityMismatch


  let download_tarball : uri:Uri.t -> (string, error) result Lwt.t =
   fun ~uri ->
    let* resp, body = Client.get uri in
    let code = Cohttp.Code.code_of_status resp.status in
    if Cohttp.Code.is_success code
    then (
      let fname = sha1 (Uri.to_string uri) in
      let dest = Caml.Filename.temp_file fname ".tgz" in
      let stream = Body.to_stream body in
      let* () =
        Lwt_io.with_file ~mode:Lwt_io.output dest (fun chan ->
            Lwt_stream.iter_s (Lwt_io.write chan) stream)
      in
      Lwt.return_ok dest)
    else Lwt.return_error UnableToDownload


  let unzip fname =
    let in_fd = Ligo_unix.openfile fname [ Ligo_unix.O_RDONLY ] 0 in
    let file_size = (Ligo_unix.stat fname).st_size in
    let buffer_len = De.io_buffer_size in
    let i = De.bigstring_create De.io_buffer_size in
    let o = De.bigstring_create De.io_buffer_size in
    let r = Buffer.create 0x1000 in
    let p = ref 0 in
    let refill buf =
      let len = min (file_size - !p) buffer_len in
      if len <= 0
      then 0
      else (
        let bytes = Bytes.create len in
        let len = Ligo_unix.read in_fd bytes 0 len in
        Bigstringaf.blit_from_bytes bytes ~src_off:0 buf ~dst_off:0 ~len;
        p := !p + len;
        len)
    in
    let flush buf len =
      let str = Bigstringaf.substring buf ~off:0 ~len in
      Buffer.add_string r str
    in
    match Gz.Higher.uncompress ~refill ~flush i o with
    | Ok _ ->
      let bytes = Buffer.contents_bytes r in
      let nbytes = Bytes.length bytes in
      let fname = Format.sprintf "%s.tar" (Caml.Filename.remove_extension fname) in
      let out_fd =
        Ligo_unix.openfile fname [ Ligo_unix.O_CREAT; Ligo_unix.O_RDWR ] 0o666
      in
      let mbytes = Ligo_unix.write out_fd bytes 0 nbytes in
      let () = Ligo_unix.close in_fd in
      let () = Ligo_unix.close out_fd in
      if nbytes = mbytes then Ok fname else Error UnableToUnzip
    | Error (`Msg _) ->
      let () = Ligo_unix.close in_fd in
      Error UnableToUnzip


  let touch f = Ligo_unix.openfile f [ Ligo_unix.O_CREAT ] 0o666 |> Ligo_unix.close

  let untar ~dest_dir fname =
    let fd = Ligo_unix.openfile fname [ Ligo_unix.O_RDONLY ] 0 in
    let move f =
      let f = Filename.concat dest_dir f in
      let () = Ligo_unix.mkdir_p ~perm:0o755 (Filename.dirname f) in
      let () = touch f in
      f
    in
    let () = Tar_unix.Archive.extract move fd in
    Ligo_unix.close fd


  let normalize_project_name = Str.(global_replace (regexp "/") "_")

  let init ~kind ~template ~project_name ?version ~registry () =
    let@ info = Lwt_main.run (package_info ?version ~registry template) in
    let@ { _type; tarball; shasum } = get_fields ?version info in
    let@ () = validate_template_type kind _type in
    let@ tarball = Lwt_main.run (download_tarball ~uri:tarball) in
    let@ () = check_integrity tarball ~expected:shasum in
    let@ unzipped = unzip tarball in
    let project_name = normalize_project_name project_name in
    let () = untar ~dest_dir:project_name unzipped in
    Ok project_name
end

let idregex = Str.regexp "[0-9]+\\.[0-9]+\\.[0-9]+"

module LcString = struct
  module T = struct
    type t = string

    let compare a b =
      let a = String.lowercase a in
      let b = String.lowercase b in
      String.compare a b


    let sexp_of_t = Sexp.of_string
  end

  include T
  include Base.Comparator.Make (T)
end

let contract_template_url_map =
  Map.of_alist_exn
    (module LcString)
    [ "NFT-factory-cameligo", "https://github.com/ligolang/NFT-factory-cameligo"
    ; "NFT-factory-jsligo", "https://github.com/ligolang/NFT-factory-jsligo"
    ; "randomness-cameligo", "https://github.com/ligolang/randomness-cameligo"
    ; "randomness-jsligo", "https://github.com/ligolang/randomness-jsligo"
    ; "shifumi-cameligo", "https://github.com/ligolang/shifumi-cameligo"
    ; "shifumi-jsligo", "https://github.com/ligolang/shifumi-jsligo"
    ; "multisig-cameligo", "https://github.com/ligolang/multisig-cameligo"
    ; "multisig-jsligo", "https://github.com/ligolang/multisig-jsligo"
    ; "advisor-cameligo", "https://github.com/ligolang/advisor-cameligo"
    ; "advisor-jsligo", "https://github.com/ligolang/advisor-jsligo"
    ; ( "predictive-market-cameligo"
      , "https://github.com/ligolang/predictive-market-cameligo" )
    ; "predictive-market-jsligo", "https://github.com/ligolang/predictive-market-jsligo"
    ; "permit-cameligo", "https://github.com/ligolang/permit-cameligo"
    ; "permit-jsligo", "https://github.com/ligolang/permit-jsligo"
    ; "dao-cameligo", "https://github.com/ligolang/dao-cameligo"
    ; "dao-jsligo", "https://github.com/ligolang/dao-jsligo"
    ]


let library_template_url_map =
  Map.of_alist_exn
    (module LcString)
    [ "ligo-bigarray", "https://github.com/ligolang/bigarray-cameligo"
    ; "ligo-math-lib", "https://github.com/ligolang/math-lib-cameligo"
    ; "ligo-fa", "https://github.com/ligolang/contract-catalogue"
    ; "ligo-permit", "https://github.com/ligolang/permit-cameligo"
    ; "ligo-breathalyzer", "https://github.com/marigold-dev/breathalyzer"
    ; "ligo-extendable-fa2", "https://github.com/smart-chain-fr/ligoExtendableFA2"
    ]


let determine_map ~kind =
  match kind with
  | `CONTRACT -> contract_template_url_map
  | `LIBRARY -> library_template_url_map


let list' ~kind = List.sort ~compare:String.compare @@ Map.keys (determine_map ~kind)

let list ~kind ~display_format ~no_colour () =
  format_result ~display_format ~no_colour Formatter.list_format
  @@ fun ~raise:_ -> list' ~kind


let new_project' ~project_url ~project_name ~version =
  let gc = Cli_helpers.run_command (Constants.git_clone ~project_url ~project_name) in
  match gc with
  | Ok () ->
    let is_release = Str.string_match idregex version 0 in
    let _ =
      if is_release
      then (
        let _ =
          Cli_helpers.run_command
            (Constants.git_checkout ~ref:version ~dir_path:project_name)
        in
        ())
      else ()
    in
    let _ = OS.Dir.delete ~recurse:true @@ Fpath.(v "." / project_name / ".git") in
    Ok project_name
  | Error _ ->
    Error
      "Error: The template could not be initialized\n\
       Hint: specify DIR where the template should be initialized\n\
       (e.g. ligo init library DIR --template TEMPLATE_NAME)"


let get_project_url_opt ~kind ~template = Map.find (determine_map ~kind) template

let new_project
    ~version
    ~kind
    ~project_name_opt
    ~template
    ~display_format
    ~no_colour
    ~registry
    ()
  =
  let project_name =
    match project_name_opt with
    | Some e -> e
    | None -> template
  in
  format_result ~display_format ~no_colour Formatter.new_project_format
  @@ fun ~raise ->
  match get_project_url_opt ~kind ~template with
  | Some project_url ->
    (match new_project' ~project_url ~project_name ~version with
    | Ok project_name -> [ project_name ]
    | Error e -> raise.error (`Ligo_init_git_template_error e))
  | None ->
    (match RegistryTemplate.init ~kind ~template ~project_name ~registry () with
    | Ok project_name -> [ project_name ]
    | Error PackageNotFound ->
      raise.error (`Ligo_init_unrecognized_template (list' ~kind))
    | Error e ->
      let e = RegistryTemplate.string_of_error e in
      raise.error (`Ligo_init_registry_template_error e))
