open Api_helpers

(* open Simple_utils.Display *)
module OS = Bos.OS
module Cmd = Bos.Cmd
module Trace = Simple_utils.Trace
module Constants = Cli_helpers.Constants
module T = Core
module Formatter = Ligo_formatter
module Checksum = Cli_helpers.Checksum

type project_entity =
  [ `LIBRARY
  | `CONTRACT
  ]

module RegistryTemplate : sig
  type error =
    | PackageNotFound
    | ServerError
    | InvalidPackageInfo of string
    | InvalidTemplateType
    | UnableToDownload
    | UnableToUnzip
    | IntegrityMismatch of Cli_helpers.Checksum.error

  val string_of_error : error -> string

  val init
    :  kind:project_entity
    -> template:string
    -> project_name:string
    -> ?version:string
    -> registry:Uri.t
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
    | InvalidTemplateType
    | UnableToDownload
    | UnableToUnzip
    | IntegrityMismatch of Cli_helpers.Checksum.error

  let string_of_error = function
    | PackageNotFound -> "Error: Package not found in registry"
    | ServerError -> "Error: registry server error"
    | InvalidPackageInfo s -> Format.sprintf "Error: Invalid package info\n%s" s
    | InvalidTemplateType ->
      "Error: mis-match of template type.\n\
       Hint: Please check the ligo command `ligo init contract`/`ligo init library"
    | UnableToDownload -> "Error: unable to downloading template"
    | UnableToUnzip -> "Error: unable to write unzipped data to file"
    | IntegrityMismatch e -> Checksum.string_of_error e


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
    match version with
    | Some version -> Uri.with_path registry (Format.sprintf "%s/%s" pkg version)
    | None -> Uri.with_path registry (Format.sprintf "%s" pkg)


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


  let download_tarball : uri:Uri.t -> (string, error) result Lwt.t =
   fun ~uri ->
    let* resp, body = Client.get uri in
    let code = Cohttp.Code.code_of_status resp.status in
    if Cohttp.Code.is_success code
    then (
      let fname = Cli_helpers.Checksum.sha1 (Uri.to_string uri) in
      let dest = Caml.Filename.temp_file fname ".tgz" in
      let stream = Body.to_stream body in
      let* () =
        Lwt_io.with_file ~mode:Lwt_io.output dest (fun chan ->
            Lwt_stream.iter_s (Lwt_io.write chan) stream)
      in
      Lwt.return_ok dest)
    else Lwt.return_error UnableToDownload


  let normalize_project_name = Str.(global_replace (regexp "/") "_")

  let init ~kind ~template ~project_name ?version ~registry () =
    let@ info = Lwt_main.run (package_info ?version ~registry template) in
    let@ { _type; tarball; shasum } = get_fields ?version info in
    let@ () = validate_template_type kind _type in
    let@ tarball = Lwt_main.run (download_tarball ~uri:tarball) in
    let@ _ =
      Checksum.check_integrity tarball ~expected:shasum
      |> function
      | Ok _ -> Ok ()
      | Error e -> Error (IntegrityMismatch e)
    in
    let@ unzipped =
      match Cli_helpers.unzip tarball with
      | Error Cli_helpers.UnableToUnzip -> Error UnableToUnzip
      | Ok buffer -> Ok buffer
    in
    let project_name = normalize_project_name project_name in
    let () = Cli_helpers.untar ~dest_dir:project_name unzipped in
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
