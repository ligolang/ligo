module ModRes = Preprocessor.ModRes

let option_eq a b = 0 = Option.compare Fpath.compare (Some a) b

let empty_project ~raise:_ () =
  let t = ModRes.make "projects/uninitialized_project" in
  assert (Option.is_none t)


let no_installation_json ~raise:_ () =
  let t = ModRes.make "projects/no_installation_json" in
  assert (Option.is_none t)


let no_lock_file ~raise:_ () =
  let t = ModRes.make "projects/no_lock_file" in
  assert (Option.is_none t)


let fpathify p =
  match Fpath.of_string p with
  | Ok p -> p
  | _ -> failwith "fpath failed"


let working_project ~raise:_ () =
  let project_root = "projects/working_project" in
  let t = ModRes.make project_root in
  let pwd = Caml.Sys.getcwd () in
  let relative p = Filename.concat (Filename.concat pwd project_root) p |> fpathify in
  let inclusion_paths =
    ModRes.get_dependencies ~file:"projects/working_project/main.mligo" t
  in
  let () =
    assert (
      List.equal
        Fpath.equal
        (inclusion_paths |> List.map ~f:(fun (ModRes.Path p) -> fpathify p))
        ModRes.
          [ relative ".ligo/source/i/ligo_list_helpers__1.0.0__bf074147"
          ; relative ".ligo/source/i/ligo_set_helpers__1.0.2__5cd724a1"
          ])
  in
  let list_helpers_path =
    ModRes.find_external_file ~file:"ligo-list-helpers/list.mligo" ~inclusion_paths
    |> Option.map ~f:fpathify
  in
  let () =
    assert (
      option_eq
        (relative ".ligo/source/i/ligo_list_helpers__1.0.0__bf074147/list.mligo")
        list_helpers_path)
  in
  let set_helpers_path =
    ModRes.find_external_file ~file:"ligo-set-helpers/set.mligo" ~inclusion_paths
    |> Option.map ~f:fpathify
  in
  assert (
    option_eq
      (relative ".ligo/source/i/ligo_set_helpers__1.0.2__5cd724a1/set.mligo")
      set_helpers_path)


let complex_project ~raise:_ () =
  let project_root = "projects/complex_project" in
  let t = ModRes.make project_root in
  let pwd = Caml.Sys.getcwd () in
  let relative p = Filename.concat (Filename.concat pwd project_root) p |> fpathify in
  let base_inclusion_paths =
    ModRes.get_dependencies ~file:"projects/complex_project/main.mligo" t
  in
  let () =
    assert (
      List.equal
        Fpath.equal
        (base_inclusion_paths |> List.map ~f:(fun (ModRes.Path p) -> fpathify p))
        ModRes.
          [ relative ".ligo/source/i/ligo__test__1__1.0.0__a381d5ee"
          ; relative ".ligo/source/i/ligo_foo__1.0.6__2355cc08"
          ; relative ".ligo/source/i/ligo_list_helpers__1.0.1__6233bebd"
          ; relative ".ligo/source/i/ligo_test__2__1.0.0__d841d05a"
          ; relative ".ligo/source/i/webpack__5.68.0__95002730"
          ])
  in
  let list_helpers_path =
    ModRes.find_external_file
      ~file:"ligo-list-helpers/list.mligo"
      ~inclusion_paths:base_inclusion_paths
    |> Option.map ~f:fpathify
  in
  let () =
    assert (
      option_eq
        (relative ".ligo/source/i/ligo_list_helpers__1.0.1__6233bebd/list.mligo")
        list_helpers_path)
  in
  let set_helpers_path =
    ModRes.find_external_file
      ~file:"ligo-set-helpers/set.mligo"
      ~inclusion_paths:base_inclusion_paths
  in
  let () = assert (Option.equal String.equal set_helpers_path None) in
  let foo_inclusion_paths =
    ModRes.get_dependencies
      ~file:
        (relative ".ligo/source/i/ligo_foo__1.0.6__2355cc08/main.mligo" |> Fpath.to_string)
      t
  in
  let list_helpers_path =
    ModRes.find_external_file
      ~file:"ligo-list-helpers/list.mligo"
      ~inclusion_paths:foo_inclusion_paths
    |> Option.map ~f:fpathify
  in
  let () =
    assert (
      option_eq
        (relative ".ligo/source/i/ligo_list_helpers__1.0.0__bf074147/list.mligo")
        list_helpers_path)
  in
  let set_helpers_path =
    ModRes.find_external_file
      ~file:"ligo-set-helpers/set.mligo"
      ~inclusion_paths:foo_inclusion_paths
    |> Option.map ~f:fpathify
  in
  let () =
    assert (
      option_eq
        (relative ".ligo/source/i/ligo_set_helpers__1.0.3__6998bccf/set.mligo")
        set_helpers_path)
  in
  let set_helpers_path =
    ModRes.find_external_file
      ~file:"ligo-set/set.mligo"
      ~inclusion_paths:foo_inclusion_paths
  in
  let () = assert (Option.equal String.equal None set_helpers_path) in
  ()
