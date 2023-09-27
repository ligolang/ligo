(* -*- compile-command: "cd .. ; dune runtest" -*- *)

open Test_helpers

module Slow_test = struct
  module OS = Bos.OS
  module Cmd = Bos.Cmd

  type error = InstallTestFails of string

  let ( let* ) o f = Result.bind o ~f
  let get_tmp_dir () : string = OS.Dir.default_tmp () |> Fpath.to_string

  let rm_rf ~(path : string) : unit =
    OS.Dir.delete ~recurse:true (Fpath.v path)
    |> function
    | Ok () -> ()
    | Error (`Msg s) -> raise @@ Failure s


  let create_tmp ~(path : string) : (string, [> Rresult.R.msg ]) result =
    let tmp_path = get_tmp_dir () ^ "/" ^ path in
    OS.Dir.create ~path:true (Fpath.v path)
    |> function
    | Ok _ -> Ok tmp_path
    | Error e -> Error e


  let git_clone ~(repo : string) ~(dest : string) : Cmd.t =
    Cmd.(v "git" % "clone" % repo % dest)


  let cd ~(dir : string) : (unit, [> Rresult.R.msg ]) result =
    OS.Dir.set_current @@ Fpath.v dir


  let ligo_install_pkgs
      ~project_root
      ~(package_dir : string)
      ~(cache_path : string)
      ~(ligo_registry : string)
      : (unit, error) result
    =
    let ligo_registry = Uri.of_string ligo_registry in
    let cache_path = Fpath.v cache_path in
    Lwt_main.run @@ Install.run ~project_root package_dir cache_path ligo_registry
    |> function
    | Ok () -> Ok ()
    | Error s -> Error (InstallTestFails (Install.string_of_error s))


  let ligo_compile_contract ~(src : string) : Cmd.t =
    Cmd.(v "ligo" % "compile" % "contract" % src)


  let get_repo repo = "https://github.com/ligolang/" ^ repo ^ ".git"

  let find_main_contract : Fpath.t list -> (string, [> Rresult.R.msg ]) result =
   fun lst ->
    lst
    |> List.map ~f:Fpath.to_string
    |> List.filter ~f:(String.is_substring ~substring:"main")
    |> List.hd
    |> function
    | Some s -> Ok s
    | None -> Error (Rresult.R.msg "couldn't find main contract")


  let get_main_path ~(repo : string) : (string, [> Rresult.R.msg ]) result =
    let* lst = OS.Dir.contents Fpath.(v (repo ^ "/" ^ "src")) in
    find_main_contract lst


  let repos = [ "dao-cameligo"; "permit-jsligo"; "permit-cameligo" ]

  let run_cmd_to_null : Cmd.t -> (unit, [> Rresult.R.msg ]) result =
   fun cmd -> OS.Cmd.(run_out cmd |> to_null)


  let run_cmd_to_string : Cmd.t -> (string, [> Rresult.R.msg ]) result =
   fun cmd -> OS.Cmd.(run_out cmd |> to_string ~trim:true)


  let package_dir = "."
  let cache_path = ".ligo"
  let ligo_registry = "https://packages.ligolang.org/-/api"

  let rec run_seq : string -> string -> (string, [> Rresult.R.msg ]) result =
   fun cwd repo ->
    (* create the tmp dir for the repo *)
    let* repo_dest = create_tmp ~path:repo in
    (* check if dir present, if true then delete and re-clone *)
    match OS.Dir.must_exist (Fpath.v repo_dest) with
    | Ok repo_dest ->
      rm_rf ~path:(Fpath.to_string repo_dest);
      run_seq cwd repo
    (* run clone cmd*)
    | _ ->
      let* _ = run_cmd_to_null @@ git_clone ~repo:(get_repo repo) ~dest:repo_dest in
      (* SIDE-EFFECT : cd to the temp repo dir *)
      let* _ = cd ~dir:repo_dest in
      (* run ligo install *)
      let project_root = Some repo_dest in
      let _ = ligo_install_pkgs ~project_root ~package_dir ~cache_path ~ligo_registry in
      (* find the main file *)
      let* src = get_main_path ~repo:repo_dest in
      (* compile contract and save the output as a string *)
      let* s = run_cmd_to_string (ligo_compile_contract ~src) in
      (* SIDE-EFFECT : cd back to cwd *)
      let* _ = cd ~dir:cwd in
      (* SIDE-EFFECT : delete the repo *)
      rm_rf ~path:repo_dest;
      (* return the generated .tz file as a string *)
      Ok s


  (* This runs the complete user flow when the command `ligo install` is used *)
  let run : string -> (string * string) list =
   fun cwd ->
    List.map ~f:(fun repo -> repo, run_seq cwd repo) repos
    |> List.fold ~init:[] ~f:(fun init (repo, s) ->
           match s with
           | Ok s -> (repo, s) :: init
           | Error (`Msg msg) -> raise @@ Failure msg)


  let get_test_tz_files : path:string -> (string * string) list =
   fun ~path ->
    let path = Fpath.v path in
    OS.Dir.contents path
    |> function
    | Ok files ->
      List.map
        ~f:(fun file ->
          OS.File.read file
          |> function
          | Ok s ->
            let file =
              List.hd_exn @@ String.split ~on:'.' @@ List.last_exn @@ Fpath.segs file
            in
            file, String.strip s
          | Error (`Msg msg) -> raise @@ Failure msg)
        files
    | Error (`Msg msg) -> raise @@ Failure msg


  let minify : string -> string =
   fun tz_file ->
    let is_uneccessary : char -> bool =
     fun c ->
      not (Char.equal c ' ' || Char.equal c '\t' || Char.equal c '\n' || Char.equal c '\r')
    in
    String.filter tz_file ~f:is_uneccessary


  module StringMap = Caml.Map.Make (struct
    type t = string

    let compare = String.compare
  end)

  let test_tz_files : unit -> (string * string) list =
   fun () ->
    List.map
      ~f:(fun (repo, s) -> repo, minify s)
      (get_test_tz_files ~path:"./install_tests/sample_tz_files")


  let generated_tz_files : unit -> (string * string) list =
   fun () ->
    OS.Dir.current ()
    |> function
    | Ok cwd -> run (Fpath.to_string cwd) |> List.map ~f:(fun (repo, s) -> repo, minify s)
    | Error (`Msg msg) -> raise @@ Failure msg


  (* map of static sample test files *)
  let repo_test_map : string StringMap.t =
    Caml.List.to_seq @@ test_tz_files () |> StringMap.of_seq


  (* map of generated test files *)

  let repo_gen_map : string StringMap.t =
    Caml.List.to_seq @@ generated_tz_files () |> StringMap.of_seq


  (* let test_dao_jsligo ~raise:_ () =
  Alcotest.(check string)
    "test dao-jsligo"
    (StringMap.find "dao-jsligo" repo_test_map)
    (StringMap.find "dao-jsligo" repo_gen_map) *)

  let test_dao_cameligo ~raise:_ () =
    Alcotest.(check string)
      "test dao-cameligo"
      (StringMap.find "dao-cameligo" repo_test_map)
      (StringMap.find "dao-cameligo" repo_gen_map)


  let test_permit_jsligo ~raise:_ () =
    Alcotest.(check string)
      "test permit-jsligo"
      (StringMap.find "permit-jsligo" repo_test_map)
      (StringMap.find "permit-jsligo" repo_gen_map)


  let test_permit_cameligo ~raise:_ () =
    Alcotest.(check string)
      "test permit-cameligo"
      (StringMap.find "permit-cameligo" repo_test_map)
      (StringMap.find "permit-cameligo" repo_gen_map)


  let main =
    test_suite
      "LIGO install tests"
      [ test
          (* "test dao-jsligo output with ligo version 0.70.0 and latest ligo version's output"
        test_dao_jsligo
    ; test *)
          "test dao-cameligo output with ligo version 0.70.0 and latest ligo version's \
           output"
          test_dao_cameligo
      ; test
          "test permit-jsligo output with ligo version 0.70.0 and latest ligo version's \
           output"
          test_permit_jsligo
      ; test
          "test permit-cameligo output with ligo version 0.70.0 and latest ligo \
           version's output"
          test_permit_cameligo
      ]
end

let () =
  Printexc.record_backtrace true;
  run_test @@ test_suite "LIGO Slow install tests" [ Slow_test.main ];
  ()
