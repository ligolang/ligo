open Test_helpers
module OS = Bos.OS
module Cmd = Bos.Cmd

type error = InstallTestFails of string

let string_of_error = function
  | InstallTestFails msg -> msg


let ( let* ) o f = Result.bind o ~f

let rm_rf ~(path : string) : unit =
  OS.Dir.delete ~recurse:true (Fpath.v path)
  |> function
  | Ok () -> ()
  | Error (`Msg s) -> raise @@ Failure s


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
  Lwt_main.run
  @@ Package_management.Alpha.run ~project_root package_dir cache_path ligo_registry
  |> function
  | Ok () -> Ok ()
  | Error s -> Error (InstallTestFails (Package_management.Alpha.string_of_error s))


let ligo_compile_contract ~(src : string) ~(project_root : string) : Cmd.t =
  Cmd.(v "ligo" % "compile" % "contract" % "--project-root" % project_root % src)


let run_cmd_to_null : Cmd.t -> (unit, [> Rresult.R.msg ]) result =
 fun cmd -> OS.Cmd.(run_out cmd |> to_null)


let run_cmd_to_string : Cmd.t -> (string, [> Rresult.R.msg ]) result =
 fun cmd -> OS.Cmd.(run_out cmd |> to_string ~trim:true)


let package_dir = "."
let cache_path = ".ligo"
let ligo_registry = "https://packages.ligolang.org/-/api"
let workflow_path = "install_tests/workflow/"
let lockfile_path = workflow_path ^ "esy.lock/"
let ligo_package_dir_path = workflow_path ^ ".ligo/"
let installation_json_path = workflow_path ^ "_esy/"

let batch_rm_rf path_list =
  let f path = rm_rf ~path in
  List.iter ~f path_list


let error_to_rresult_error error =
  let f error = Rresult.R.msg (string_of_error error) in
  Result.map_error ~f error


let run_workflow_test ~path_list =
  let* project_root = OS.Dir.current () in
  let project_root = Fpath.to_string project_root in
  let workflow_path = project_root ^ "/" ^ workflow_path in
  let path_list = List.map path_list ~f:(fun path -> workflow_path ^ path) in
  let src = workflow_path ^ "main.mligo" in
  batch_rm_rf path_list;
  let* () =
    ligo_install_pkgs
      ~project_root:(Some workflow_path)
      ~package_dir
      ~cache_path
      ~ligo_registry
    |> error_to_rresult_error
  in
  let output =
    ligo_compile_contract ~src ~project_root:workflow_path |> run_cmd_to_string
  in
  match output with
  | Error e -> Error e
  | Ok _ -> Ok ()


(* #! /bin/sh

CMD="ligo install --package-management-alpha ; ligo compile contract ./main.mligo"

sh -c "rm -rf esy.lock _esy .ligo; $CMD"
sh -c "rm -rf _esy .ligo; $CMD"

sh -c "rm -rf esy.lock _esy .ligo; $CMD"
sh -c "rm -rf .ligo; $CMD"

sh -c "rm -rf esy.lock _esy .ligo; $CMD"
sh -c "rm -rf _esy; $CMD"

sh -c "$CMD" *)

let remove_ligoPackageDir_installationJson () =
  let first_run =
    run_workflow_test
      ~path_list:[ lockfile_path; ligo_package_dir_path; installation_json_path ]
  in
  let second_run =
    run_workflow_test ~path_list:[ ligo_package_dir_path; installation_json_path ]
  in
  match first_run, second_run with
  | Ok (), Ok () -> ()
  | Error (`Msg msg), _ -> raise @@ Failure ("First run failed : " ^ msg)
  | _, Error (`Msg msg) -> raise @@ Failure ("Second run failed : " ^ msg)


let remove_ligoPackageDir () =
  let first_run =
    run_workflow_test
      ~path_list:[ lockfile_path; ligo_package_dir_path; installation_json_path ]
  in
  let second_run = run_workflow_test ~path_list:[ ligo_package_dir_path ] in
  match first_run, second_run with
  | Ok (), Ok () -> ()
  | Error (`Msg msg), _ -> raise @@ Failure ("First run failed : " ^ msg)
  | _, Error (`Msg msg) -> raise @@ Failure ("Second run failed : " ^ msg)


let remove_installationJson () =
  let first_run =
    run_workflow_test
      ~path_list:[ lockfile_path; ligo_package_dir_path; installation_json_path ]
  in
  let second_run = run_workflow_test ~path_list:[ installation_json_path ] in
  match first_run, second_run with
  | Ok (), Ok () -> ()
  | Error (`Msg msg), _ -> raise @@ Failure ("First run failed : " ^ msg)
  | _, Error (`Msg msg) -> raise @@ Failure ("Second run failed : " ^ msg)


let remove_nothing () =
  let first_run = run_workflow_test ~path_list:[] in
  match first_run with
  | Ok () -> ()
  | Error (`Msg msg) -> raise @@ Failure ("First and the only run failed : " ^ msg)


let test_remove_ligoPackageDir_installationJson ~raise:_ () =
  Alcotest.(check unit)
    "test if removing .ligo/ & _esy/ generates code"
    (remove_ligoPackageDir_installationJson ())
    ()


let test_remove_ligoPackageDir ~raise:_ () =
  Alcotest.(check unit)
    "test if removing .ligo/ generates code"
    (remove_ligoPackageDir ())
    ()


let test_remove_installationJson ~raise:_ () =
  Alcotest.(check unit)
    "test if removing _esy/ generates code"
    (remove_installationJson ())
    ()


let test_remove_nothing ~raise:_ () =
  Alcotest.(check unit) "test if removing nothing generates code" (remove_nothing ()) ()


let main =
  (* assert between checked-in michelson output from compiler version 0.70.0 and test generated output *)
  test_suite
    "LIGO install tests"
    [ test
        "Test workflow when running [ligo install --package-management-alpha] followed \
         by [ligo compile contract ./main.mligo] in the install_tests/workflow directoy. \
         In this test by removing .ligo/ and _esy/ directories"
        test_remove_ligoPackageDir_installationJson
    ; test
        "Test workflow when running [ligo install --package-management-alpha] followed \
         by [ligo compile contract ./main.mligo] in the install_tests/workflow directoy. \
         In this test by removing .ligo/ directory"
        test_remove_ligoPackageDir
    ; test
        "Test workflow when running [ligo install --package-management-alpha] followed \
         by [ligo compile contract ./main.mligo] in the install_tests/workflow directoy. \
         In this test by removing _esy/ directory"
        test_remove_installationJson
    ; test
        "Test workflow when running [ligo install --package-management-alpha] followed \
         by [ligo compile contract ./main.mligo] in the install_tests/workflow directoy. \
         In this test by removing nothin"
        test_remove_nothing
    ]
