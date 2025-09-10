module PP = PP
module Errors = Errors
module To_yojson = To_yojson
module Formatter = Formatter
module Location = Simple_utils.Location
module Ne_list = Simple_utils.Ne_list
open Core
include Types

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
    (* FIXME remove all non-needed code_input *)
    | From_file of file_name
    | HTTP of Uri.t
    | Raw of raw_input
    | Raw_input_lsp of raw_input_lsp

  let map_code_input : code_input -> f:(file_name -> file_name) -> code_input =
   fun code_input ~f ->
    match code_input with
    | From_file file_name -> From_file (f file_name)
    | HTTP uri -> HTTP uri
    | Raw { id; code } -> Raw { id = f id; code }
    | Raw_input_lsp { file; code } -> Raw_input_lsp { file = f file; code }

  let id_of_code_input : code_input -> file_name = function
    | From_file file_name -> file_name
    | HTTP uri -> Filename.basename @@ Uri.to_string uri
    | Raw { id; code = _ } -> id
    | Raw_input_lsp { file; code = _ } -> file
end

module T = struct
  type file_name = Source_input.file_name
  type raw_input = Source_input.raw_input
  type code_input = Source_input.code_input
  type module_name = string

  (** `import` composes metadata of imported module
      `location` is used for error reporting *)
  type import =
    { code_input : code_input
    ; module_name : module_name
    ; location : Location.t
    }

  type imports = import list
end

include T

module type M = sig
  (** Metadata of contract being built *)
  type meta_data

  (** Module representing compilation unit *)
  module C_unit : sig
    type t

    (* Composes all data needed for compilation *)
    type meta =
      { code_input : code_input
      ; location : Location.t
      ; module_name : module_name
      ; meta : meta_data
      ; imports : imports
      }
  end

  (** Converts import into ready to compile C_unit.t, gathers meta_data and imports *)
  val preprocess : import -> C_unit.t * meta_data * imports

  (** Module reprenting target AST which contract compiles to *)
  module AST : sig
    (** Target AST type *)
    type t

    (** An interface describes the signature of a module *)
    type interface

    (** Links two asts into one *)
    val link : t -> t -> t

    (* This should probably be taken in charge be the compiler, which should be able to handle "libraries" *)

    (** Adds inline module to the ast *)
    val make_module_in_ast : t -> module_name * interface * t -> t
  end

  (** Module representing compilation environment *)
  module Environment : sig
    type t

    val init_env : t
    val add_interface : t -> AST.interface -> t
    val find_interface : t -> module_name -> AST.interface
    val add_module : t -> C_unit.meta -> AST.interface -> t
  end

  (* Actually performs compilation *)
  val compile : C_unit.t -> C_unit.meta -> Environment.t -> AST.t * AST.interface

  (* Returns compiled standard library *)
  val std_lib : unit -> AST.t * AST.interface

  (** Applies left transformations required for resulting AST *)
  val postprocess : AST.t -> intfs:Environment.t -> AST.t
end

module type S = functor (M : M) -> sig
  (** Vertex of dependency graph *)
  type vertex = M.C_unit.t * M.C_unit.meta

  val module_name_of_vertex : vertex -> module_name

  (** Dependency graph *)
  type graph = G.t * vertex SMap.t

  type 'a build_result = ('a, Errors.t) Result.t

  (** Builds dependency graph from code_input *)
  val dependency_graph : code_input -> graph

  (** Checks if graph is a DAG and returns topsorted list of files to compile *)
  val solve_graph : graph -> module_name -> (module_name * vertex) Ne_list.t build_result

  (** Builds input without linking all the code_input dependencies into one AST.
      Useful for inspection, debugging and testing. *)
  val build_unqualified : code_input -> M.AST.t build_result

  (** Builds input and links all its dependencies into its ast *)
  val build_qualified : code_input -> (M.AST.t * M.Environment.t) build_result
end

module Make : S =
functor
  (M : M)
  ->
  struct
    include M

    type vertex = C_unit.t * C_unit.meta

    let module_name_of_vertex : vertex -> module_name = fun (_, meta) -> meta.module_name

    type graph = G.t * vertex SMap.t
    type 'a build_result = ('a, Errors.t) Result.t
    type obj_map = AST.t SMap.t

    let dependency_graph : code_input -> graph =
     fun code_input ->
      let rec dfs
          (acc : module_name)
          (dep_g, vertices)
          ({ code_input; module_name; location } as import)
        =
        let id = Source_input.id_of_code_input code_input in
        if not @@ Map.mem vertices id
        then (
          (* Historically, preprocess is used for extracting dependencies also *)
          let c_unit, meta, imports = preprocess import in
          let vertices =
            Map.set
              vertices
              ~key:id
              ~data:(c_unit, C_unit.{ code_input; location; module_name; meta; imports })
          in
          let dep_g = G.add_vertex dep_g id in
          let dep_g =
            (* Don't add a loop on the first element *)
            if Node.equal acc id then dep_g else G.add_edge dep_g acc id
          in
          let dep_g, vertices = List.fold ~f:(dfs id) ~init:(dep_g, vertices) imports in
          dep_g, vertices)
        else (
          let dep_g = G.add_edge dep_g acc id in
          dep_g, vertices)
      in
      let vertices = SMap.empty in
      let dep_g = G.empty in
      let file_name = Source_input.id_of_code_input code_input in
      let module_name = file_name in
      dfs file_name (dep_g, vertices)
      @@ { code_input; module_name; location = Location.dummy }

    let solve_graph : graph -> file_name -> (file_name * vertex) Ne_list.t build_result =
     fun (dep_g, vertices) file_name ->
      if Dfs.has_cycle dep_g
      then (
        let graph = Format.asprintf "%a" PP.graph (dep_g, file_name) in
        Error (Errors.build_dependency_cycle graph))
      else (
        let aux v order =
          let elem = Map.find_exn vertices v in
          (v, elem) :: order
        in
        let order = TopSort.fold aux dep_g [] in
        match order with
        | hd :: tl -> Ok (hd :: tl)
        | [] -> Error (Errors.build_compiling_nothing))

    let link ~(objs : obj_map) ~(intfs : Environment.t) linking_order =
      (* Separate the program and the dependency (those are process differently) *)
      let (file_name, (_, C_unit.{ imports = _deps_lst; _ })), linking_order =
        match Ne_list.rev linking_order with
        | hd :: tl -> hd, tl
      in
      (* NOTE The contract build system was invoked for must present in the build environment at this point *)
      let contract = Map.find_exn objs file_name in
      (* Add all dependency at the beginning of the file *)
      let add_modules (file_name, (_, C_unit.{ module_name; imports = _deps_lst; _ })) =
        let module_binder = module_name in
        (* Get the ast_type of the module *)
        (* NOTE Same for its deps: they were already compiled since we are at linking stage *)
        let ast_typed = Map.find_exn objs file_name in
        module_binder, ast_typed
      in
      let header_list = List.map ~f:add_modules @@ linking_order in
      let contract =
        List.fold_left
          ~f:(fun c (module_binder, ast) ->
            AST.make_module_in_ast
              c
              (module_binder, Environment.find_interface intfs module_binder, ast))
          ~init:contract
          header_list
      in
      (* Link the stdlib *)
      let contract = AST.link (Tuple2.get1 @@ std_lib ()) contract in
      (* Finally link all the imports *)
      let contract = postprocess contract ~intfs in
      contract

    let compile_file_with_deps
        ((objs, intfs) : obj_map * Environment.t)
        (file_name, (c_unit, c_unit_meta))
      =
      let ast, ast_intf = compile c_unit c_unit_meta intfs in
      let intfs = Environment.add_module intfs c_unit_meta ast_intf in
      let objs = Map.set objs ~key:file_name ~data:ast in
      objs, intfs

    let build
        :  code_input
        -> (file_name * (file_name * vertex) Ne_list.t * obj_map * Environment.t) build_result
      =
     fun code_input ->
      let deps = dependency_graph code_input in
      let file_name = Source_input.id_of_code_input code_input in
      match solve_graph deps file_name with
      | Ok linking_order ->
        let init_env = Environment.(add_interface init_env) (Tuple2.get2 @@ std_lib ()) in
        let objs, intfs =
          List.fold ~f:compile_file_with_deps ~init:(SMap.empty, init_env) @@ Ne_list.to_list linking_order
        in
        Ok (file_name, linking_order, objs, intfs)
      | Error e -> Error e

    let build_unqualified : code_input -> AST.t build_result =
     fun code_input ->
      let open Result.Monad_infix in
      build code_input
      >>= fun (module_name, _, objs, _) ->
      (* NOTE The contract build system was invoked for must present in the build environment at this point *)
      Ok (Map.find_exn objs module_name)

    let build_qualified : code_input -> (AST.t * Environment.t) build_result =
     fun code_input ->
      let open Result.Monad_infix in
      build code_input
      >>= fun (_, linking_order, objs, intfs) ->
      Ok (link ~objs ~intfs linking_order, intfs)
  end
