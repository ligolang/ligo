module PP = PP
module Errors = Errors
module To_yojson = To_yojson
module Formatter = Formatter
include Types
module List = Simple_utils.List

module Source_input = struct
  type file_name = string

  type raw_input =
    { id : file_name
    ; code : string
    }

  type raw_input_lsp =
    { file : file_name
    ; code : string
    }

  type code_input =
    | From_file of file_name
    | HTTP of Uri.t
    | Raw of raw_input
    | Raw_input_lsp of raw_input_lsp

  let id_of_code_input : code_input -> file_name = function
    | From_file file_name -> file_name
    | HTTP uri -> Filename.basename @@ Uri.to_string uri
    | Raw { id; code = _ } -> id
    | Raw_input_lsp { file; code = _ } -> file
end

module type M = sig
  type file_name = Source_input.file_name
  type raw_input = Source_input.raw_input
  type code_input = Source_input.code_input
  type module_name = string
  type compilation_unit
  type meta_data

  val preprocess
    :  code_input
    -> compilation_unit * meta_data * (file_name * module_name) list

  module AST : sig
    type t

    (* An interface describes the signature of a module *)
    type interface

    (* Environment should be a local notion of the BuildSystem *)
    type environment

    val link : t -> t -> t
    val link_interface : interface -> interface -> interface
    val init_env : environment
    val add_module_to_environment : module_name -> interface -> environment -> environment
    val add_interface_to_environment : interface -> environment -> environment

    (* This should probably be taken in charge be the compiler, which should be able to handle "libraries" *)
    val make_module_in_ast : module_name -> t -> interface -> t -> t
    val make_module_in_interface : module_name -> interface -> interface -> interface
  end

  val link_imports : AST.t -> objs:(AST.t * AST.interface) SMap.t -> AST.t

  val compile
    :  AST.environment
    -> file_name
    -> meta_data
    -> compilation_unit
    -> AST.t * AST.interface

  val lib_ast : unit -> AST.t
  val lib_interface : unit -> AST.interface
end

module Make (M : M) = struct
  type file_name = M.file_name
  type code_input = M.code_input

  type vertice =
    M.file_name * M.meta_data * M.compilation_unit * (M.file_name * M.module_name) list

  type graph = G.t * vertice SMap.t
  type error = Errors.t
  type ast = M.AST.t
  type env = M.AST.environment
  type interface = M.AST.interface
  type 'a build_error = ('a, error) result

  let dependency_graph : code_input -> graph =
   fun code_input ->
    let rec dfs (acc : M.file_name) (dep_g, vertices) (code_input, mangled_name) =
      let id = Source_input.id_of_code_input code_input in
      if not @@ SMap.mem id vertices
      then (
        let c_unit, meta_data, deps = M.preprocess code_input in
        let vertices = SMap.add id (mangled_name, meta_data, c_unit, deps) vertices in
        let dep_g = G.add_vertex dep_g id in
        let dep_g =
          (* Don't add a loop on the first element *)
          if Node.equal acc id then dep_g else G.add_edge dep_g acc id
        in
        let dep_g, vertices =
          let f (x, y) =
            let dependency_code_input =
              match Caml.Sys.backend_type with
              | Other "js_of_ocaml" -> Source_input.HTTP (Uri.of_string x)
              | _ -> Source_input.From_file x
            in
            dependency_code_input, y
          in
          let deps = List.map ~f deps in
          List.fold ~f:(dfs id) ~init:(dep_g, vertices) deps
        in
        dep_g, vertices)
      else (
        let dep_g = G.add_edge dep_g acc id in
        dep_g, vertices)
    in
    let vertices = SMap.empty in
    let dep_g = G.empty in
    let file_name = Source_input.id_of_code_input code_input in
    dfs file_name (dep_g, vertices) @@ (code_input, file_name)

  let solve_graph : graph -> file_name -> ((file_name * vertice) list, error) result =
   fun (dep_g, vertices) file_name ->
    if Dfs.has_cycle dep_g
    then (
      let graph = Format.asprintf "%a" PP.graph (dep_g, file_name) in
      Error (Errors.build_dependency_cycle graph))
    else (
      let aux v order =
        let elem = SMap.find v vertices in
        (v, elem) :: order
      in
      let order = TopSort.fold aux dep_g [] in
      Ok order)

  let link ~objs linking_order =
    (* Add the module at the beginning of the file *)
    let aux map (file_name, (mangled_name, _, _, _deps_lst)) =
      let ast, intf =
        match SMap.find_opt file_name objs with
        | Some ast -> ast
        | None -> failwith "failed to find module"
      in
      let map = SMap.add mangled_name (ast, intf) map in
      map
    in
    let mangled_objs = List.fold ~f:aux ~init:SMap.empty linking_order in
    (* Separate the program and the dependency (those are process differently) *)
    let (file_name, (_, _, _, _deps_lst)), linking_order =
      match List.rev linking_order with
      | [] -> failwith "compiling nothing"
      | hd :: tl -> hd, tl
    in
    let contract, contract_intf =
      match SMap.find_opt file_name objs with
      | Some ast -> ast
      | None -> failwith "failed to find module"
    in
    (* Add all dependency at the beginning of the file *)
    let add_modules (file_name, (mangled_name, _, _, _deps_lst)) =
      let module_binder = mangled_name in
      (* Get the ast_type of the module *)
      let ast_typed, interface =
        match SMap.find_opt file_name objs with
        | Some ast -> ast
        | None -> failwith "failed to find module"
      in
      module_binder, ast_typed, interface
    in
    let header_list = List.map ~f:add_modules @@ linking_order in
    let contract, contract_intf =
      List.fold_left
        ~f:(fun (c, ci) (module_binder, ast, interface) ->
          ( M.AST.make_module_in_ast module_binder ast interface c
          , M.AST.make_module_in_interface module_binder interface ci ))
        ~init:(contract, contract_intf)
        header_list
    in
    (* Link the stdlib *)
    let contract = M.AST.link (M.lib_ast ()) contract in
    let contract_intf = M.AST.link_interface (M.lib_interface ()) contract_intf in
    (* Finally link all the imports *)
    let contract = M.link_imports contract ~objs:mangled_objs in
    contract, contract_intf

  let add_modules_in_env (env : M.AST.environment) deps =
    let aux env (module_name, (_, intf)) =
      M.AST.add_module_to_environment module_name intf env
    in
    List.fold_left ~f:aux ~init:env deps

  let add_deps_to_env
      (asts_typed : (ast * interface) SMap.t)
      (_file_name, (_meta, _c_unit, deps))
    =
    let aux (file_name, module_name) =
      let file_name =
        match Caml.Sys.backend_type with
        | Other "js_of_ocaml" ->
          Filename.basename file_name (* Because HTTP URIs could be deepers *)
        | _ -> file_name
      in
      let ast_typed =
        match SMap.find_opt file_name asts_typed with
        | Some ast -> ast
        | None ->
          failwith
            ("Failed for "
            ^ file_name
            ^ " File typed before dependency. The build system is broken, contact the \
               devs")
      in
      module_name, ast_typed
    in
    let deps = List.map ~f:aux deps in
    let init_env =
      M.AST.add_interface_to_environment (M.lib_interface ()) M.AST.init_env
    in
    let env_with_deps = add_modules_in_env init_env deps in
    env_with_deps

  let compile_file_with_deps asts (file_name, (_mangled_name, meta, c_unit, deps)) =
    let env_with_deps = add_deps_to_env asts (file_name, (meta, c_unit, deps)) in
    let ast, ast_intf = M.compile env_with_deps file_name meta c_unit in
    SMap.add file_name (ast, ast_intf) asts

  let compile_unqualified : code_input -> ast build_error =
   fun main_code_input ->
    let deps = dependency_graph main_code_input in
    let main_file_name = Source_input.id_of_code_input main_code_input in
    match solve_graph deps main_file_name with
    | Ok ordered_deps ->
      let asts_typed =
        List.fold ~f:compile_file_with_deps ~init:SMap.empty ordered_deps
      in
      Ok (fst @@ SMap.find main_file_name asts_typed)
    | Error e -> Error e

  let compile_qualified : code_input -> (ast * interface) build_error =
   fun code_input ->
    let deps = dependency_graph code_input in
    let file_name = Source_input.id_of_code_input code_input in
    match solve_graph deps file_name with
    | Ok linking_order ->
      let objs = List.fold ~f:compile_file_with_deps ~init:SMap.empty linking_order in
      let contract, contract_interface = link ~objs linking_order in
      Ok (contract, contract_interface)
    | Error e -> Error e
end
