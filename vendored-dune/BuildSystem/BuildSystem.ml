module PP = PP
module Errors = Errors
module To_yojson = To_yojson
module Formatter = Formatter

open Types
module List = Simple_utils.List
module Source_input = struct
  type file_name = string
  type raw_input = { id : file_name ; code : string }
  type code_input = From_file of file_name | Raw of raw_input
  let id_of_code_input : code_input -> file_name = function
  | From_file file_name -> file_name
  | Raw { id ; code = _  } -> id
end

module type M =
  sig
    type file_name = Source_input.file_name
    type raw_input = Source_input.raw_input
    type code_input = Source_input.code_input
    type module_name = string
    type compilation_unit
    type meta_data
    val preprocess : code_input -> compilation_unit * meta_data * (file_name * module_name) list
    module AST : sig
      type declaration
      type t = declaration list
      (* Environment should be a local notion of the BuildSystem *)
      type environment
      val add_module_to_env : module_name -> environment -> environment -> environment
      val add_ast_to_env : t -> environment -> environment
      val init_env : environment

      (* This should probably be taken in charge be the compiler, which should be able to handle "libraries" *)
      val make_module_declaration : module_name -> t -> declaration
    end
    val compile : AST.environment -> file_name -> meta_data -> compilation_unit -> AST.t
    val lib_ast : unit -> AST.t
  end

module Make (M : M) =
  struct

  type file_name = M.file_name
  type code_input = M.code_input
  type vertice = M.file_name * M.meta_data * M.compilation_unit * (M.file_name * M.module_name) list
  type graph = G.t * vertice SMap.t
  type error = Errors.t
  type ast = M.AST.t
  type env = M.AST.environment
  type 'a build_error = ('a, error) result


  let dependency_graph : code_input -> graph =
    fun code_input ->
    let rec dfs (acc:M.file_name) (dep_g,vertices) (code_input,mangled_name) =
      let id = Source_input.id_of_code_input code_input in
      if not @@ SMap.mem id vertices then
        let c_unit, meta_data, deps = M.preprocess code_input in
        let vertices = SMap.add id (mangled_name,meta_data,c_unit,deps) vertices in
        let dep_g = G.add_vertex dep_g id in
        let dep_g =
          (* Don't add a loop on the first element *)
          if Node.equal acc id then dep_g
          else G.add_edge dep_g acc id
        in
        let dep_g,vertices =
          let deps = List.map ~f:(fun (x,y) -> Source_input.From_file x, y) deps in
          List.fold ~f:(dfs id) ~init:(dep_g,vertices) deps
        in
        (dep_g,vertices)
      else
        let dep_g = G.add_edge dep_g acc id in
        (dep_g,vertices)
    in
    let vertices = SMap.empty in
    let dep_g = G.empty in
    let file_name = Source_input.id_of_code_input code_input in
    dfs file_name (dep_g,vertices) @@ (code_input,file_name)

  let solve_graph : graph -> file_name -> ((file_name * vertice) list,error) result =
    fun (dep_g,vertices) file_name ->
    if Dfs.has_cycle dep_g
    then (
      let graph = Format.asprintf "%a" PP.graph (dep_g,file_name) in
      Error (Errors.build_dependency_cycle graph)
    )
    else
      let aux v order =
        let elem = SMap.find v vertices in
        (v,elem)::order
      in
      let order = TopSort.fold aux dep_g [] in
      Ok (order)

  let aggregate_dependencies_as_headers order_deps asts_typed =
    (* Add the module at the beginning of the file *)
    let aux map ((file_name),(_,_,_,deps_lst)) =
      let (ast,_) =
        match (SMap.find_opt file_name asts_typed) with
          Some ast -> ast
        | None -> failwith "failed to find module"
      in

      let map = SMap.add file_name ast map in
      map
    in
    let asts_typed = List.fold ~f:aux ~init:SMap.empty order_deps in
    (* Separate the program and the dependency (those are process differently) *)
    let (file_name,(_,_,_,_deps_lst)),order_deps = match List.rev order_deps with
      | [] -> failwith "compiling nothing"
      | hd::tl -> (hd,tl) in
    let contract =
      match (SMap.find_opt file_name asts_typed) with
        Some ast -> ast
      | None -> failwith "failed to find module"
    in
    (* Add all dependency at the beginning of the file *)
    let add_modules dep_types (file_name,(mangled_name,_,_, _deps_lst)) =
      let module_binder = mangled_name in
      (* Get the ast_type of the module *)
      let ast_typed =
        match (SMap.find_opt file_name asts_typed) with
          Some ast -> ast
        | None -> failwith "failed to find module"
      in
      (dep_types,(M.AST.make_module_declaration module_binder ast_typed))
    in
    let _,header_list = List.fold_map_right ~f:add_modules ~init:(SMap.empty) @@ order_deps in
    let aggregated = List.fold_left ~f:(fun c a ->  a::c) ~init:contract header_list in
    aggregated

  let add_modules_in_env (env : M.AST.environment) deps =
    let aux env (module_name, (_,ast)) =
      M.AST.add_module_to_env module_name ast env
    in
    List.fold_left ~f:aux ~init:env deps

  let add_deps_to_env (asts_typed : (ast * env) SMap.t) (_file_name, (_meta,_c_unit,deps)) =
    let aux (file_name,module_name) =
      let ast_typed =
        match (SMap.find_opt file_name asts_typed) with
          Some (ast) -> ast
        | None -> failwith "File typed before dependency. The build system is broken, contact the devs"
      in
      (module_name, ast_typed)
    in
    let deps = List.map ~f:aux deps in
    let env_with_deps = add_modules_in_env M.AST.init_env deps in
    env_with_deps

  let compile_file_with_deps asts (file_name, (mangled_name,meta,c_unit,deps)) =
    let env_with_deps = add_deps_to_env asts (file_name, (meta,c_unit,deps)) in
    let ast = M.compile env_with_deps file_name meta c_unit in
    let ast_env = M.AST.add_ast_to_env (ast:ast) env_with_deps in
    SMap.add file_name (ast,ast_env) asts

  let compile_unqualified : code_input -> ast build_error =
    fun main_code_input ->
      let deps = dependency_graph main_code_input in
      let main_file_name = Source_input.id_of_code_input main_code_input in
      match solve_graph deps main_file_name with
        Ok (ordered_deps) ->
        let asts_typed = List.fold ~f:(compile_file_with_deps) ~init:(SMap.empty) ordered_deps in
        Ok (fst @@ SMap.find main_file_name asts_typed)
      | Error e -> Error e

  let compile_qualified : code_input -> ast build_error =
    fun code_input ->
      let deps = dependency_graph code_input in
      let file_name = Source_input.id_of_code_input code_input in
      match solve_graph deps file_name with
        Ok (ordered_deps) ->
        let asts_typed = List.fold ~f:(compile_file_with_deps) ~init:(SMap.empty) ordered_deps in
        let contract = aggregate_dependencies_as_headers ordered_deps asts_typed in
        let contract = M.lib_ast () @ contract in
        Ok contract
      | Error e -> Error e
  end

