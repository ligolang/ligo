module PP = PP
module Errors = Errors
module To_yojson = To_yojson
module Formatter = Formatter
include Types
open Core

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
  type imports = file_name list
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

    val add_module_to_environment
      :  file_name
      -> module_name
      -> imports
      -> interface
      -> environment
      -> environment

    val add_interface_to_environment : interface -> environment -> environment

    (* This should probably be taken in charge be the compiler, which should be able to handle "libraries" *)
    val make_module_in_ast : module_name -> t -> t -> t
    val make_module_in_interface : module_name -> interface -> interface -> interface
  end

  val link_imports : AST.t -> intfs:AST.environment -> AST.t

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
  type obj_env = ast SMap.t
  type intf_env = M.AST.environment
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
            let dependency_code_input = Source_input.From_file x in
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

  let link ~(objs : obj_env) ~(intfs : intf_env) linking_order =
    (* Separate the program and the dependency (those are process differently) *)
    let (file_name, (_, _, _, _deps_lst)), linking_order =
      match List.rev linking_order with
      | [] -> failwith "compiling nothing"
      | hd :: tl -> hd, tl
    in
    let contract =
      match SMap.find_opt file_name objs with
      | Some ast -> ast
      | None -> failwith "failed to find module"
    in
    (* Add all dependency at the beginning of the file *)
    let add_modules (file_name, (mangled_name, _, _, _deps_lst)) =
      let module_binder = mangled_name in
      (* Get the ast_type of the module *)
      let ast_typed =
        match SMap.find_opt file_name objs with
        | Some ast -> ast
        | None -> failwith "failed to find module"
      in
      module_binder, ast_typed
    in
    let header_list = List.map ~f:add_modules @@ linking_order in
    let contract =
      List.fold_left
        ~f:(fun c (module_binder, ast) -> M.AST.make_module_in_ast module_binder ast c)
        ~init:contract
        header_list
    in
    (* Link the stdlib *)
    let contract = M.AST.link (M.lib_ast ()) contract in
    (* Finally link all the imports *)
    let contract = M.link_imports contract ~intfs in
    contract

  let compile_file_with_deps
      ((objs, intfs) : obj_env * intf_env)
      (file_name, (mangled_name, meta, c_unit, _deps))
    =
    let imports = List.map ~f:(fun (x, _) -> x) _deps in
    let ast, ast_intf = M.compile intfs file_name meta c_unit in
    let intfs =
      M.AST.add_module_to_environment file_name mangled_name imports ast_intf intfs
    in
    let objs = SMap.add file_name ast objs in
    objs, intfs

  let compile_unqualified : code_input -> ast build_error =
   fun main_code_input ->
    let deps = dependency_graph main_code_input in
    let main_file_name = Source_input.id_of_code_input main_code_input in
    match solve_graph deps main_file_name with
    | Ok ordered_deps ->
      let init_env =
        M.AST.add_interface_to_environment (M.lib_interface ()) M.AST.init_env
      in
      let objs, _ =
        List.fold ~f:compile_file_with_deps ~init:(SMap.empty, init_env) ordered_deps
      in
      Ok (SMap.find main_file_name objs)
    | Error e -> Error e

  let compile_qualified : code_input -> ast build_error =
   fun code_input ->
    let deps = dependency_graph code_input in
    let file_name = Source_input.id_of_code_input code_input in
    match solve_graph deps file_name with
    | Ok linking_order ->
      let init_env =
        M.AST.add_interface_to_environment (M.lib_interface ()) M.AST.init_env
      in
      let objs, intfs =
        List.fold ~f:compile_file_with_deps ~init:(SMap.empty, init_env) linking_order
      in
      let contract = link ~objs ~intfs linking_order in
      Ok contract
    | Error e -> Error e
end
