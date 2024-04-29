module Util = Yojson.Safe.Util

let ( let* ) x f = Result.bind x ~f

open Package_management_external_libs
open Package_management_shared
module Semver = Ligo_semver

type t =
  { name : string
  ; version : Semver.t
  ; description : string
  ; scripts : (string * string) list
  ; dependencies : (string * string) list
  ; dev_dependencies : (string * string) list
  ; main : string
  ; author : string
  ; type_ : string
  ; storage_fn : string option
  ; storage_arg : string option
  ; repository : Repository_url.t
  ; license : string
  ; readme : string
  ; ligo_manifest_path : string
  ; bugs : Bugs.t
  }
[@@deriving yojson]

let validate_storage ~main ~storage_fn ~storage_arg () =
  match storage_fn, storage_arg with
  | Some storage_fn, Some storage_arg ->
    let raw_options = Compiler_options.Raw_options.make () in
    let expression = Format.sprintf "%s %s" storage_fn storage_arg in
    let _, run_me =
      Ligo_api.Compile.storage raw_options [] main expression "0" "0" None None None `Json
    in
    (match Simple_utils.Trace.to_stdlib_result ~fast_fail:Fast_fail run_me with
    | Ok (_, (), _) -> Ok ()
    | Error _ ->
      Error
        "Error: Check `storage_fn` & `storage_arg` in packge.json or check your LIGO \
         storage expression")
  | _ -> Ok ()


let validate_main_file ~main =
  match Sys_unix.file_exists main with
  | `Yes ->
    let ext_opt = snd @@ Filename.split_extension main in
    let ligo_syntax_opt = Syntax.of_ext_opt ext_opt in
    (match ligo_syntax_opt with
    | Some Syntax_types.CameLIGO | Some Syntax_types.JsLIGO -> Ok ()
    | None ->
      Error
        "Error: Invalid LIGO file specifed in main field of ligo.json\n\
         Valid extension for LIGO files are (.mligo, .jsligo) ")
  | `No | `Unknown ->
    Error
      "Error: main file does not exists.\nPlease specify a valid LIGO file in ligo.json."


let validate t =
  let { main; storage_fn; storage_arg; _ } = t in
  (* stat main file here *)
  let* () = validate_main_file ~main in
  (* check storage *)
  let* () =
    validate_storage
      ~main:(BuildSystem.Source_input.From_file main)
      ~storage_fn
      ~storage_arg
      ()
  in
  Ok ()


let try_readme ~project_root =
  let ls = Ligo_unix.ls_dir project_root in
  match
    List.find ls ~f:(fun d ->
        String.equal "readme.md" (String.lowercase d)
        || String.equal "readme" (String.lowercase d))
  with
  | None -> "Error: No README data found!"
  | Some r ->
    let contents = In_channel.read_all (Filename.concat project_root r) in
    String.escaped contents


let parse_require_field ~field_name json =
  match Util.member field_name json with
  | `String "" ->
    Error (Format.asprintf "Error: %s field is empty (\"\") in ligo.json" field_name)
  | `String s -> Ok s
  | `Null -> Error (Format.asprintf "Error: No %s field in ligo.json" field_name)
  | _ -> Error (Format.asprintf "Error: Invalid %s field in ligo.json" field_name)


let parse_sem_ver version =
  match Semver.of_string version with
  | Some s -> Ok s
  | None -> Error (Format.sprintf "Error: Invalid version %s in ligo.json" version)


let parse_version json =
  let* version =
    match Util.member "version" json with
    | `String "" -> Error "Error: version field is empty (\"\") in ligo.json"
    | `String s -> Ok s
    | `Null -> Error "Error: No version field in ligo.json"
    | _ -> Error "Error: Invalid version field in ligo.json"
  in
  parse_sem_ver version


let parse_description json =
  match Util.member "description" json with
  | `String s -> Ok s
  | _ -> Ok ""


let string_assoc_list_equal assoc1 assco2 =
  List.equal
    (fun (k1, v1) (k2, v2) -> String.equal k1 k2 && String.equal v1 v2)
    assoc1
    assco2


let parse_string_assoc_list
    : (string * Yojson.Safe.t) list -> (string, string) List.Assoc.t
  =
 fun a ->
  List.filter_opt
  @@ List.map a ~f:(fun (k, v) ->
         match v with
         | `String v -> Some (k, v)
         | _ -> None)


let parse_scripts json =
  match Util.member "scripts" json with
  | `Assoc a -> Ok (parse_string_assoc_list a)
  | _ -> Ok []


let parse_dependencies json =
  match Util.member "dependencies" json with
  | `Assoc a -> Ok (parse_string_assoc_list a)
  | _ -> Ok []


let parse_dev_dependencies json =
  match Util.member "devDependencies" json with
  | `Assoc a -> Ok (parse_string_assoc_list a)
  | _ -> Ok []


let parse_repository json =
  match Util.member "repository" json with
  | `Null -> Error "Error: No repository field in ligo.json"
  | repo ->
    (match Repository_url.parse repo with
    | Ok t -> Ok t
    | Error e ->
      Error (Format.sprintf "Error: Invalid repository field in ligo.json\n%s" e))


let parse_readme ~project_root json =
  match Util.member "readme" json with
  | `String s -> Ok s
  | _ -> Ok (try_readme ~project_root)


let parse_type json =
  match Util.member "type" json with
  | `Null -> Ok "library"
  | `String "contract" -> Ok "contract"
  | `String "library" -> Ok "library"
  | _ ->
    Error "Error: Invalid type field in ligo.json\nType can be either library or contract"


let parse_storage_fn ~type_ json =
  match Util.member "storage_fn" json with
  | `String "" when String.(type_ = "contract") ->
    Error "Error: storage_fn field is empty (\"\") in ligo.json"
  | `String s when String.(type_ = "contract") -> Ok (Some s)
  | _ when String.(type_ = "contract") ->
    Error "Error: In case of a type : contract a `storage_fn` needs to be provided."
  | _ -> Ok None


let parse_storage_arg ~type_ json =
  match Util.member "storage_arg" json with
  | `String "" when String.(type_ = "contract") ->
    Error "Error: storage_arg field is empty (\"\") in ligo.json"
  | `String s when String.(type_ = "contract") -> Ok (Some s)
  | _ when String.(type_ = "contract") ->
    Error "Error: In case of a type : contract a `storage_arg` needs to be provided."
  | _ -> Ok None


let parse_bugs json =
  match Util.member "bugs" json with
  | `Null -> Error "Error: No bugs field in ligo.json"
  | bugs ->
    (match Bugs.of_yojson bugs with
    | Ok bugs -> Ok bugs
    | Error _ ->
      Error
        "Error: Invalid `bugs` field in ligo.json.\n\
         url (bug tracker url) and / or email needs to be provided\n\
         e.g.{ \"url\" : \"https://github.com/foo/bar/issues\" , \"email\" : \
         \"foo@bar.com\" }")


let read_from_json ~project_root ~ligo_manifest_path json =
  (* Required fields *)
  let* name = parse_require_field ~field_name:"name" json in
  let* version = parse_version json in
  let* author = parse_require_field ~field_name:"author" json in
  let* repository = parse_repository json in
  let* main = parse_require_field ~field_name:"main" json in
  let* license = parse_require_field ~field_name:"license" json in
  let* bugs = parse_bugs json in
  (* Optional fields *)
  let* type_ = parse_type json in
  let* description = parse_description json in
  let* scripts = parse_scripts json in
  let* dependencies = parse_dependencies json in
  let* dev_dependencies = parse_dev_dependencies json in
  let* storage_fn = parse_storage_fn ~type_ json in
  let* storage_arg = parse_storage_arg ~type_ json in
  let* readme = parse_readme ~project_root json in
  Ok
    { name
    ; version
    ; author
    ; repository
    ; main
    ; license
    ; bugs
    ; type_
    ; description
    ; scripts
    ; dependencies
    ; dev_dependencies
    ; storage_fn
    ; storage_arg
    ; readme
    ; ligo_manifest_path
    }


let read ~project_root =
  match project_root with
  | None -> failwith "Error: No ligo.json found!"
  | Some project_root ->
    let ligo_manifest_path = Filename.concat project_root "ligo.json" in
    let () =
      match Sys_unix.file_exists ligo_manifest_path with
      | `No | `Unknown -> failwith "Error: Unable to find ligo.json!"
      | `Yes -> ()
    in
    let json =
      try Yojson.Safe.from_file ligo_manifest_path with
      | _ -> failwith "Error: Error in parsing ligo.json (invalid json)"
    in
    read_from_json ~project_root ~ligo_manifest_path json


(* Unit tests *)

let ligo_manifest_path = "<valid-path>"
let project_root = "<valid-path>"
let read_from_json = read_from_json ~project_root ~ligo_manifest_path

(* name = missing *)
let%test _ =
  let json = Yojson.Safe.from_string {|{}|} in
  match read_from_json json with
  | Error e -> String.(e = "Error: No name field in ligo.json")
  | Ok _ -> false

(* name = empty *)
let%test _ =
  let json = Yojson.Safe.from_string {|{"name":""}|} in
  match read_from_json json with
  | Error e -> String.(e = {|Error: name field is empty ("") in ligo.json|})
  | Ok _ -> false

(* name = valid & version = missing *)
let%test _ =
  let json = Yojson.Safe.from_string {|{"name":"foo"}|} in
  match read_from_json json with
  | Error e -> String.(e = {|Error: No version field in ligo.json|})
  | Ok _ -> false

(* version = empty *)
let%test _ =
  let json = Yojson.Safe.from_string {|{"name":"foo","version":""}|} in
  match read_from_json json with
  | Error e -> String.(e = {|Error: version field is empty ("") in ligo.json|})
  | Ok _ -> false

(* version = invalid *)
let%test _ =
  let json = Yojson.Safe.from_string {|{"name":"foo","version":"0.1"}|} in
  match read_from_json json with
  | Error e -> String.(e = {|Error: Invalid version 0.1 in ligo.json|})
  | Ok _ -> false

(* version = valid & author = missing *)
let%test _ =
  let json = Yojson.Safe.from_string {|{"name":"foo","version":"0.1.0"}|} in
  match read_from_json json with
  | Error e -> String.(e = {|Error: No author field in ligo.json|})
  | Ok _ -> false

(* author = empty *)
let%test _ =
  let json = Yojson.Safe.from_string {|{"name":"foo","version":"0.1.0","author":""}|} in
  match read_from_json json with
  | Error e -> String.(e = {|Error: author field is empty ("") in ligo.json|})
  | Ok _ -> false

(* author = valid & repository = missing *)
let%test _ =
  let json =
    Yojson.Safe.from_string {|{"name":"foo","version":"0.1.0","author":"john doe"}|}
  in
  match read_from_json json with
  | Error e -> String.(e = {|Error: No repository field in ligo.json|})
  | Ok _ -> false

(* repository = empty *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe","repository":""}|}
  in
  match read_from_json json with
  | Error e ->
    String.(e = "Error: Invalid repository field in ligo.json\nrepository url is invalid")
  | Ok _ -> false

(* repository = valid & main = empty *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "repository":"https://github.com/npm/cli.git"}|}
  in
  match read_from_json json with
  | Error e -> String.(e = "Error: No main field in ligo.json")
  | Ok _ -> false

(* main = valid & license = missing *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo"}|}
  in
  match read_from_json json with
  | Error e -> String.(e = "Error: No license field in ligo.json")
  | Ok _ -> false

(* license = empty *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo","license":""}|}
  in
  match read_from_json json with
  | Error e -> String.(e = {|Error: license field is empty ("") in ligo.json|})
  | Ok _ -> false

(* license = valid & bugs = missing *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo","license":"MIT"}|}
  in
  match read_from_json json with
  | Error e -> String.(e = {|Error: No bugs field in ligo.json|})
  | Ok _ -> false

(* bugs = empty *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo","license":"MIT","bugs":""}|}
  in
  match read_from_json json with
  | Error e ->
    String.(
      e
      = {|Error: Invalid `bugs` field in ligo.json.
url (bug tracker url) and / or email needs to be provided
e.g.{ "url" : "https://github.com/foo/bar/issues" , "email" : "foo@bar.com" }|})
  | Ok _ -> false

(* bugs = only url *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo","license":"MIT",
         "bugs":{"url":"https://foo.com/bar"},
         "readme":"README"}|}
  in
  match read_from_json json with
  | Error _ -> false
  | Ok manifest -> String.equal manifest.bugs.url "https://foo.com/bar"

(* bugs = only email *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo","license":"MIT",
         "bugs":{"email":"foo@bar.com"}}|}
  in
  match read_from_json json with
  | Error e ->
    String.(
      e
      = {|Error: Invalid `bugs` field in ligo.json.
url (bug tracker url) and / or email needs to be provided
e.g.{ "url" : "https://github.com/foo/bar/issues" , "email" : "foo@bar.com" }|})
  | Ok _ -> false

(* bugs = both url & email *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo","license":"MIT",
         "bugs":{"url":"https://foo.com/bar","email":"foo@bar.com"},
         "readme":"README"}|}
  in
  match read_from_json json with
  | Error _ -> false
  | Ok manifest ->
    String.equal manifest.name "foo"
    && Semver.equal manifest.version (Option.value_exn (Semver.of_string "0.1.0"))
    && String.equal manifest.author "john doe"
    && String.equal manifest.repository.type_ "git"
    && String.equal manifest.repository.url "https://github.com/npm/cli.git"
    && Option.equal String.equal manifest.repository.directory None
    && String.equal manifest.main "lib.mligo"
    && String.equal manifest.license "MIT"
    && String.equal manifest.bugs.url "https://foo.com/bar"
    && Option.equal String.equal manifest.bugs.email (Some "foo@bar.com")
    && String.equal manifest.readme "README"
    && String.equal manifest.type_ "library"

(* type_ = invalid *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo","license":"MIT",
         "bugs":{"url":"https://foo.com/bar","email":"foo@bar.com"},
         "readme":"README","type":"module"}|}
  in
  match read_from_json json with
  | Error e ->
    String.(
      e
      = {|Error: Invalid type field in ligo.json
Type can be either library or contract|})
  | Ok _ -> false

(* type_ = empty *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo","license":"MIT",
         "bugs":{"url":"https://foo.com/bar","email":"foo@bar.com"},
         "readme":"README","type":""}|}
  in
  match read_from_json json with
  | Error e ->
    String.(
      e
      = {|Error: Invalid type field in ligo.json
Type can be either library or contract|})
  | Ok _ -> false

(* type_ = library *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo","license":"MIT",
         "bugs":{"url":"https://foo.com/bar","email":"foo@bar.com"},
         "readme":"README","type":"library"}|}
  in
  match read_from_json json with
  | Error _ -> false
  | Ok manifest -> String.equal manifest.type_ "library"

(* scripts = empty *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo","license":"MIT",
         "bugs":{"url":"https://foo.com/bar","email":"foo@bar.com"},
         "readme":"README","type":"library","script":{}}|}
  in
  match read_from_json json with
  | Error _ -> false
  | Ok manifest -> string_assoc_list_equal manifest.scripts []

(* scripts = valid *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo","license":"MIT",
         "bugs":{"url":"https://foo.com/bar","email":"foo@bar.com"},
         "readme":"README","type":"library",
         "scripts":{"test":"ligo run test lib.test.mligo"}}|}
  in
  match read_from_json json with
  | Error _ -> false
  | Ok manifest ->
    string_assoc_list_equal manifest.scripts [ "test", "ligo run test lib.test.mligo" ]

(* dependencies = empty *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo","license":"MIT",
         "bugs":{"url":"https://foo.com/bar","email":"foo@bar.com"},
         "readme":"README","type":"library",
         "scripts":{"test":"ligo run test lib.test.mligo"},
         "dependencies":{}}|}
  in
  match read_from_json json with
  | Error _ -> false
  | Ok manifest -> string_assoc_list_equal manifest.dependencies []

(* dependencies = valid *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo","license":"MIT",
         "bugs":{"url":"https://foo.com/bar","email":"foo@bar.com"},
         "readme":"README","type":"library",
         "scripts":{"test":"ligo run test lib.test.mligo"},
         "dependencies":{"@ligo/bigarray":"0.1.1"}}|}
  in
  match read_from_json json with
  | Error _ -> false
  | Ok manifest ->
    string_assoc_list_equal manifest.dependencies [ "@ligo/bigarray", "0.1.1" ]

(* devDependencies = empty *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo","license":"MIT",
         "bugs":{"url":"https://foo.com/bar","email":"foo@bar.com"},
         "readme":"README","type":"library",
         "scripts":{"test":"ligo run test lib.test.mligo"},
         "dependencies":{"@ligo/bigarray":"0.1.1"},
         "devDependencies":{}}|}
  in
  match read_from_json json with
  | Error _ -> false
  | Ok manifest -> string_assoc_list_equal manifest.dev_dependencies []

(* devDependencies = valid *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo","license":"MIT",
         "bugs":{"url":"https://foo.com/bar","email":"foo@bar.com"},
         "readme":"README","type":"library",
         "scripts":{"test":"ligo run test lib.test.mligo"},
         "dependencies":{"@ligo/bigarray":"0.1.1"},
         "devDependencies":{"some_tool":"1.0.0"}}|}
  in
  match read_from_json json with
  | Error _ -> false
  | Ok manifest ->
    string_assoc_list_equal manifest.dev_dependencies [ "some_tool", "1.0.0" ]

(* type_ = "contract" & storage_fn = missing *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "type":"contract",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo","license":"MIT",
         "bugs":{"url":"https://foo.com/bar","email":"foo@bar.com"},
         "readme":"README","type":"library",
         "scripts":{"test":"ligo run test lib.test.mligo"},
         "dependencies":{"@ligo/bigarray":"0.1.1"},
         "devDependencies":{"some_tool":"1.0.0"}}|}
  in
  match read_from_json json with
  | Error e ->
    String.(
      e = "Error: In case of a type : contract a `storage_fn` needs to be provided.")
  | Ok _ -> false

(* storage_fn = empty *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "type":"contract",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo","license":"MIT",
         "bugs":{"url":"https://foo.com/bar","email":"foo@bar.com"},
         "readme":"README","type":"library",
         "scripts":{"test":"ligo run test lib.test.mligo"},
         "dependencies":{"@ligo/bigarray":"0.1.1"},
         "devDependencies":{"some_tool":"1.0.0"},
         "storage_fn":""}|}
  in
  match read_from_json json with
  | Error e -> String.(e = {|Error: storage_fn field is empty ("") in ligo.json|})
  | Ok _ -> false

(* storage_fn = valid & storage_arg = missing *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "type":"contract",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo","license":"MIT",
         "bugs":{"url":"https://foo.com/bar","email":"foo@bar.com"},
         "readme":"README","type":"library",
         "scripts":{"test":"ligo run test lib.test.mligo"},
         "dependencies":{"@ligo/bigarray":"0.1.1"},
         "devDependencies":{"some_tool":"1.0.0"},
         "storage_fn":"generate_initial_storage"}|}
  in
  match read_from_json json with
  | Error e ->
    String.(
      e = "Error: In case of a type : contract a `storage_arg` needs to be provided.")
  | Ok _ -> false

(* storage_arg = empty *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "type":"contract",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo","license":"MIT",
         "bugs":{"url":"https://foo.com/bar","email":"foo@bar.com"},
         "readme":"README","type":"library",
         "scripts":{"test":"ligo run test lib.test.mligo"},
         "dependencies":{"@ligo/bigarray":"0.1.1"},
         "devDependencies":{"some_tool":"1.0.0"},
         "storage_fn":"generate_initial_storage",
         "storage_arg":""}|}
  in
  match read_from_json json with
  | Error e -> String.(e = "Error: storage_arg field is empty (\"\") in ligo.json")
  | Ok _ -> false

(* storage_arg = valid *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "type":"contract",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo","license":"MIT",
         "bugs":{"url":"https://foo.com/bar","email":"foo@bar.com"},
         "readme":"README","type":"library",
         "scripts":{"test":"ligo run test lib.test.mligo"},
         "dependencies":{"@ligo/bigarray":"0.1.1"},
         "devDependencies":{"some_tool":"1.0.0"},
         "storage_fn":"generate_initial_storage",
         "storage_arg":"1"}|}
  in
  match read_from_json json with
  | Error _ -> false
  | Ok manifest ->
    Option.equal String.equal manifest.storage_fn (Some "generate_initial_storage")
    && Option.equal String.equal manifest.storage_arg (Some "1")

(* description = missing *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "type":"contract",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo","license":"MIT",
         "bugs":{"url":"https://foo.com/bar","email":"foo@bar.com"},
         "readme":"README","type":"library",
         "scripts":{"test":"ligo run test lib.test.mligo"},
         "dependencies":{"@ligo/bigarray":"0.1.1"},
         "devDependencies":{"some_tool":"1.0.0"},
         "storage_fn":"generate_initial_storage",
         "storage_arg":"1"}|}
  in
  match read_from_json json with
  | Error _ -> false
  | Ok manifest -> String.equal manifest.description ""

(* description = empty *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "type":"contract",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo","license":"MIT",
         "bugs":{"url":"https://foo.com/bar","email":"foo@bar.com"},
         "readme":"README","type":"library",
         "scripts":{"test":"ligo run test lib.test.mligo"},
         "dependencies":{"@ligo/bigarray":"0.1.1"},
         "devDependencies":{"some_tool":"1.0.0"},
         "storage_fn":"generate_initial_storage",
         "storage_arg":"1",
         "description":""}|}
  in
  match read_from_json json with
  | Error _ -> false
  | Ok manifest -> String.equal manifest.description ""

(* description = valid *)
let%test _ =
  let json =
    Yojson.Safe.from_string
      {|{"name":"foo","version":"0.1.0","author":"john doe",
         "type":"contract",
         "repository":"https://github.com/npm/cli.git",
         "main":"lib.mligo","license":"MIT",
         "bugs":{"url":"https://foo.com/bar","email":"foo@bar.com"},
         "readme":"README","type":"library",
         "scripts":{"test":"ligo run test lib.test.mligo"},
         "dependencies":{"@ligo/bigarray":"0.1.1"},
         "devDependencies":{"some_tool":"1.0.0"},
         "storage_fn":"generate_initial_storage",
         "storage_arg":"1",
         "description":"A LIGO package"}|}
  in
  match read_from_json json with
  | Error _ -> false
  | Ok manifest -> String.equal manifest.description "A LIGO package"
