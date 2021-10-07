module PP = PP
module To_yojson = To_yojson
module Formatter = Formatter

open Trace
open Types
open Main_errors

type file_name = string
type module_name = string
type graph = G.t * (Ligo_compile.Helpers.meta * Ligo_compile.Of_core.form * Buffer.t * (string * string) list) SMap.t

type 'a build_error = 'a 
(* Build system *)

let dependency_graph ~raise : options:Compiler_options.t -> string -> Ligo_compile.Of_core.form -> file_name -> graph build_error =
  fun ~options syntax form file_name ->
  let vertices = SMap.empty in
  let dep_g = G.empty in
  let rec dfs acc (dep_g,vertices) (file_name,form) =
    if not @@ SMap.mem file_name vertices then
      let meta = trace ~raise build_error_tracer @@ Ligo_compile.Of_source.extract_meta syntax file_name in
      let c_unit, deps =
      Ligo_compile.Helpers.preprocess_file ~raise ~options ~meta file_name in
      let vertices = SMap.add file_name (meta,form,c_unit,deps) vertices in
      let dep_g = G.add_vertex dep_g file_name in
      let dep_g =
        (* Don't add a loop on the first element *)
        if String.equal acc file_name then dep_g
        else G.add_edge dep_g acc file_name
      in
      let files = List.map ~f:(fun (a,_) -> (a,Ligo_compile.Of_core.Env)) deps in
      let dep_g,vertices = List.fold ~f:(dfs file_name) ~init:(dep_g,vertices) files in
      (dep_g,vertices)
    else
      let dep_g = G.add_edge dep_g acc file_name in
      (dep_g,vertices)
  in
  dfs file_name (dep_g,vertices) @@ (file_name,form)

let solve_graph ~raise : graph -> file_name -> _ list =
  fun (dep_g,vertices) file_name ->
  if Dfs.has_cycle dep_g
  then (
    let graph = Format.asprintf "%a" PP.graph (dep_g,file_name) in
    raise.raise @@ build_dependency_cycle @@ graph
  )
  else
    let aux v order =
      let elem = SMap.find v vertices in
      (v,elem)::order
    in
    let order = Dfs.fold_component aux [] dep_g file_name in
    order

let add_modules_in_env env deps =
  let aux env (module_name, (_,ast_typed_env)) = Ast_typed.(
    let env = Environment.add_module module_name ast_typed_env env in
    env
  )
  in
  List.fold_left ~f:aux ~init:env deps

let aggregate_contract ~raise order_deps asts_typed =
  (* Add the module at the beginning of the file *)
  let aux map (file_name,(_,_,_,deps_lst)) =
    let (Ast_typed.Module_Fully_Typed contract,_) =
      trace_option ~raise (build_corner_case __LOC__ "raise.raise to find typed module") @@
      SMap.find_opt file_name asts_typed in
    let aux ast_typed (file_name,module_name) =
      ((Location.wrap @@ (Ast_typed.Module_alias {alias=module_name;binders=file_name,[]}: Ast_typed.declaration))
      :: ast_typed)
    in
    let ast_typed = List.fold_left ~f:aux ~init:contract deps_lst in
    let map = SMap.add file_name ast_typed map in
    map
  in
  let asts_typed = List.fold ~f:aux ~init:SMap.empty order_deps in
  (* Separate the program and the dependency (those are process differently) *)
  let (file_name,(_,_,_,_deps_lst)),order_deps = match List.rev order_deps with
    | [] -> raise.raise @@ build_corner_case __LOC__ "compiling nothing"
    | hd::tl -> (hd,tl) in
  let contract =
    trace_option ~raise (build_corner_case __LOC__ "raise.raise to find typed module") @@
    SMap.find_opt file_name asts_typed in
  (* Add all dependency at the beginning of the file *)
  let add_modules dep_types (file_name,(_,_,_, _deps_lst)) =
    let module_binder = file_name in
    (* Get the ast_type of the module *)
    let ast_typed =
      trace_option ~raise (build_corner_case __LOC__ "raise.raise to find typed module") @@
      SMap.find_opt file_name asts_typed in
    (dep_types,Some (Location.wrap @@ (Ast_typed.Declaration_module {module_binder;module_=Ast_typed.Module_Fully_Typed ast_typed}: Ast_typed.declaration)))
  in
  let _,header_list = List.fold_map_right ~f:add_modules ~init:(SMap.empty) @@ order_deps in
  let contract = List.fold_left ~f:(fun c a -> match a with Some a -> a::c | None -> c)
    ~init:contract header_list in
  Ast_typed.Module_Fully_Typed contract

let add_deps_to_env ~raise ~(options:Compiler_options.t) asts_typed (_file_name, (_meta,_form,_c_unit,deps)) =
  let aux (file_name,module_name) =
    let ast_typed =
      trace_option ~raise (build_corner_case __LOC__
      "File typed before dependency. The build system is broken, contact the devs")
      @@ SMap.find_opt file_name asts_typed
    in
    (module_name, ast_typed)
  in
  let deps = List.map ~f:aux deps in
  let env_with_deps = add_modules_in_env options.init_env deps in
  env_with_deps

let infer_file_with_deps ~raise ~add_warning ~(options:Compiler_options.t) asts_typed (file_name, (meta,form,c_unit,deps)) =
  let env_with_deps = add_deps_to_env ~raise  ~options asts_typed (file_name, (meta,form,c_unit,deps)) in
  let options = {options with init_env = env_with_deps } in
  
  let ast_core = Ligo_compile.Utils.to_core ~raise ~add_warning ~options ~meta c_unit file_name in
  let inferred = Ligo_compile.Of_core.infer ~raise ~options ast_core in
  (inferred, env_with_deps)

let typecheck_file_with_deps ~raise ~add_warning ~(options:Compiler_options.t) asts_typed (file_name, (_meta,form,_c_unit,_deps)) ast_core_inferred =
  let ast_typed,ast_typed_env = Ligo_compile.Of_core.typecheck ~raise ~add_warning ~options form ast_core_inferred in
  SMap.add file_name (ast_typed,ast_typed_env) asts_typed

let infer_and_typecheck_file_with_deps ~raise ~add_warning ~(options:Compiler_options.t) asts_typed (file_name, (meta,form,c_unit,deps)) =
  let (ast_core_inferred, env_with_deps) = infer_file_with_deps ~raise ~add_warning ~options asts_typed (file_name, (meta, form, c_unit, deps)) in
  let options = { options with init_env = env_with_deps } in
  typecheck_file_with_deps ~raise ~add_warning ~options asts_typed (file_name, (meta, form, c_unit, deps)) ast_core_inferred

let infer_contract ~raise ~add_warning : options:Compiler_options.t -> string -> Ligo_compile.Of_core.form -> file_name -> _ =
  fun ~options syntax entry_point main_file_name ->
    let deps = dependency_graph ~raise syntax ~options entry_point main_file_name in
    let ordered_deps = solve_graph ~raise deps main_file_name in
    (* This assumes that there are no dependency cycles involving the main file.
       Dependency cycles are not supported anyway. *)
    let mains, ordered_deps_only = List.partition_tf ~f:(fun (this_file_name, _) -> String.equal this_file_name main_file_name) ordered_deps in
    let main = assert (List.length mains == 1); List.hd_exn mains in
    let asts_typed = List.fold ~f:(infer_and_typecheck_file_with_deps ~raise ~add_warning ~options) ~init:(SMap.empty) ordered_deps_only in
    let (inferred_main, env_with_deps_of_main) = infer_file_with_deps ~raise ~add_warning ~options asts_typed main in
    (main, inferred_main, env_with_deps_of_main, asts_typed)

let type_contract ~raise ~add_warning : options:Compiler_options.t -> string -> Ligo_compile.Of_core.form -> file_name -> _ =
  fun ~options syntax entry_point file_name ->
    let (main, inferred_main, env_with_deps_of_main, asts_typed) = infer_contract ~raise ~add_warning ~options syntax entry_point file_name in
    let options = { options with init_env = env_with_deps_of_main } in
    let asts_typed = typecheck_file_with_deps ~raise ~add_warning ~options asts_typed main inferred_main in
    SMap.find file_name asts_typed

let combined_contract ~raise ~add_warning : options:Compiler_options.t -> _ -> _ -> file_name -> _ =
  fun ~options syntax entry_point file_name ->
    let deps = dependency_graph ~raise syntax ~options entry_point file_name in
    let order_deps = solve_graph ~raise deps file_name in
    let asts_typed = List.fold ~f:(infer_and_typecheck_file_with_deps ~raise ~add_warning ~options) ~init:(SMap.empty) order_deps in
    let contract = aggregate_contract ~raise order_deps asts_typed in
    (contract, snd @@ SMap.find file_name asts_typed)

let build_mini_c ~raise ~add_warning : options:Compiler_options.t -> _ -> _ -> file_name -> _ =
  fun ~options syntax entry_point file_name ->
    let contract,env = combined_contract ~raise ~add_warning ~options syntax entry_point file_name in
    let mini_c       = trace ~raise build_error_tracer @@ Ligo_compile.Of_typed.compile contract in
    (mini_c,env)

let build_contract ~raise ~add_warning : options:Compiler_options.t -> string -> _ -> file_name -> _ =
  fun ~options syntax entry_point file_name ->
    let mini_c,_   = build_mini_c ~raise ~add_warning ~options syntax (Contract entry_point) file_name in
    let michelson  = trace ~raise build_error_tracer @@ Ligo_compile.Of_mini_c.aggregate_and_compile_contract ~options mini_c entry_point in
    michelson

let build_contract_use ~raise ~add_warning : options:Compiler_options.t -> string -> file_name -> _ =
  fun ~options syntax file_name ->
    let contract,env = combined_contract ~raise ~add_warning ~options syntax Ligo_compile.Of_core.Env file_name in
    let mini_c,map = trace ~raise build_error_tracer @@ Ligo_compile.Of_typed.compile_with_modules contract in
    (mini_c, map, contract, env)

let build_contract_module ~raise ~add_warning : options:Compiler_options.t -> string -> _ -> file_name -> module_name -> _ =
  fun ~options syntax entry_point file_name module_name ->
  let deps = dependency_graph ~raise syntax ~options entry_point file_name in
  let order_deps = solve_graph ~raise deps file_name in
  let asts_typed = List.fold ~f:(infer_and_typecheck_file_with_deps ~raise ~add_warning ~options) ~init:(SMap.empty) order_deps in
  let _, env = SMap.find file_name asts_typed in
  let contract = aggregate_contract ~raise order_deps asts_typed in
  let module_contract = Ast_typed.Declaration_module { module_binder = module_name;
                                                       module_ = contract } in
  let contract = Ast_typed.Module_Fully_Typed [Location.wrap module_contract] in
  let mini_c,map = trace ~raise build_error_tracer @@ Ligo_compile.Of_typed.compile_with_modules contract in
  (mini_c, map, contract, env)
