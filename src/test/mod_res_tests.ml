module ModRes = Preprocessor.ModRes

let option_eq a b = 0 = Option.compare String.compare (Some a) b

let empty_project ~raise:_ () =
  let t = ModRes.make "projects/uninitialized_project" in
  assert (Option.is_none t)

let no_installation_json ~raise:_ () =
  let t = ModRes.make "projects/no_installation_json" in
  assert (Option.is_none t)

let no_lock_file ~raise:_ () =
  let t = ModRes.make "projects/no_lock_file" in
  assert (Option.is_none t)

let working_project ~raise:_ () =
  let t = ModRes.make "projects/working_project" in

  let inclusion_paths =
    ModRes.get_dependencies
      ~file:"/foo/projects/ligo-foo/main.mligo" t in
  let () =
    assert (List.equal ModRes.equal_paths inclusion_paths
                       ModRes.[
    Path "/foo/.esy/source/i/ligo_list_helpers__1.0.0__bf074147";
    Path "/foo/.esy/source/i/ligo_set_helpers__1.0.2__5cd724a1"]) in

  let list_helpers_path = ModRes.find_external_file
    ~file:"ligo-list-helpers/list.mligo" ~inclusion_paths in
  let () = assert (option_eq
    "/foo/.esy/source/i/ligo_list_helpers__1.0.0__bf074147/list.mligo"
    list_helpers_path) in

  let set_helpers_path = ModRes.find_external_file
    ~file:"ligo-set-helpers/set.mligo" ~inclusion_paths in
  assert (option_eq
    "/foo/.esy/source/i/ligo_set_helpers__1.0.2__5cd724a1/set.mligo"
    set_helpers_path)

let complex_project ~raise:_ () =
  let t = ModRes.make "projects/complex_project" in

  let base_inclusion_paths =
    ModRes.get_dependencies ~file:"/foo/projects/ligo-main/main.mligo" t in
  let () =
    assert (List.equal ModRes.equal_paths base_inclusion_paths
                       ModRes.[
    Path "/foo/.esy/source/i/ligo__test__1__1.0.0__a381d5ee" ;
    Path "/foo/.esy/source/i/ligo_foo__1.0.6__2355cc08" ;
    Path "/foo/.esy/source/i/ligo_list_helpers__1.0.1__6233bebd" ;
    Path "/foo/.esy/source/i/ligo_test__2__1.0.0__d841d05a" ;
    Path "/foo/.esy/source/i/webpack__5.68.0__95002730"]) in

  let list_helpers_path = ModRes.find_external_file
    ~file:"ligo-list-helpers/list.mligo" ~inclusion_paths:base_inclusion_paths in
  let () = assert (option_eq
    "/foo/.esy/source/i/ligo_list_helpers__1.0.1__6233bebd/list.mligo"
    list_helpers_path) in

  let set_helpers_path = ModRes.find_external_file
    ~file:"ligo-set-helpers/set.mligo" ~inclusion_paths:base_inclusion_paths in
  let () = assert (Option.equal String.equal set_helpers_path None) in

  let foo_inclusion_paths =
    ModRes.get_dependencies ~file:"/foo/.esy/source/i/ligo_foo__1.0.6__2355cc08/main.mligo" t in

  let list_helpers_path = ModRes.find_external_file
    ~file:"ligo-list-helpers/list.mligo" ~inclusion_paths:foo_inclusion_paths in
  let () = assert (option_eq
    "/foo/.esy/source/i/ligo_list_helpers__1.0.0__bf074147/list.mligo"
    list_helpers_path) in

  let set_helpers_path = ModRes.find_external_file
    ~file:"ligo-set-helpers/set.mligo" ~inclusion_paths:foo_inclusion_paths in
  let () = assert (option_eq
    "/foo/.esy/source/i/ligo_set_helpers__1.0.3__6998bccf/set.mligo"
    set_helpers_path) in

  let set_helpers_path = ModRes.find_external_file
    ~file:"ligo-set/set.mligo" ~inclusion_paths:foo_inclusion_paths in
  let () = assert (Option.equal String.equal None set_helpers_path)
  in ()
