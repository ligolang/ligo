(** Attempts to find the absolute path to [file_path] in the [lib_name] library
    by looking at the dependencies of [file] using the provided [project_root].

    For example, given the following call to this function:

    [resolve_lib_path
      ~project_root:"contracts/lsp"
      ~file:"contracts/lsp/registry.jsligo"
      ~lib_name:"bigarray"
      ~file_path:(Filename.concat "lib" "bigarray.mligo")]

    We will get something like this as a result:

    [Some "<some_path>/src/test/contracts/lsp/.ligo/source/i/ligo__s__bigarray__1.0.0__cf1c9d6c/lib/bigarray.mligo"]

    Where [<some_path>] is machine dependent. *)
let resolve_lib_path
    ~(project_root : string)
    ~(file : string)
    ~(lib_name : string)
    ~(file_path : string)
    : string option
  =
  let t = Preprocessor.ModRes.make project_root in
  let inclusion_paths = Preprocessor.ModRes.get_dependencies ~file t in
  Option.map
    (List.find inclusion_paths ~f:(fun (Path path) ->
         String.is_substring path ~substring:lib_name))
    ~f:(fun (Path path) -> Filename.concat path file_path)
