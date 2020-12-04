module PP = PP
module To_yojson = To_yojson
module Formatter = Formatter

open Trace
open Types
open Main_errors

type file_name = string
type graph = G.t * (Compile.Helpers.meta * Compile.Of_core.form * Buffer.t * (string * string) list) SMap.t

type 'a build_error = ('a , Main_errors.all) result
(* Build system *)

let dependency_graph : options:Compiler_options.t -> string -> Compile.Of_core.form -> file_name -> graph build_error =
  fun ~options syntax form file_name ->
  let vertices = SMap.empty in
  let dep_g = G.empty in
  let rec dfs acc (dep_g,vertices) (file_name,form) =
    if not @@ SMap.mem file_name vertices then
      let%bind meta = trace build_error_tracer @@ Compile.Of_source.extract_meta syntax file_name in
      let%bind c_unit, deps = trace preproc_tracer @@ Compile.Helpers.preprocess_file ~options ~meta file_name in
      let vertices = SMap.add file_name (meta,form,c_unit,deps) vertices in
      let dep_g = G.add_vertex dep_g file_name in
      let dep_g =
        (* Don't add a loop on the first element *)
        if String.equal acc file_name then dep_g
        else G.add_edge dep_g acc file_name
      in
      let files = List.map (fun (a,_) -> (a,Compile.Of_core.Env)) deps in
      let%bind dep_g,vertices = bind_fold_list (dfs file_name) (dep_g,vertices) files in
      ok @@ (dep_g,vertices)
    else
      let dep_g = G.add_edge dep_g acc file_name in
      ok @@ (dep_g,vertices)
  in
  dfs file_name (dep_g,vertices) @@ (file_name,form)

let solve_graph : graph -> file_name -> (_ list,_) result =
  fun (dep_g,vertices) file_name ->
  if Dfs.has_cycle dep_g
  then (
    let graph = Format.asprintf "%a" PP.graph (dep_g,file_name) in
    fail @@ build_dependency_cycle @@ graph
  )
  else
    let aux v order =
      let elem = SMap.find v vertices in
      (v,elem)::order
    in
    let order = Dfs.fold_component aux [] dep_g file_name in
    ok @@ order

let add_modules_in_env env deps =
  let aux env (module_name, (_,ast_typed_env)) = Ast_typed.(
    let env = Environment.add_module module_name ast_typed_env env in
    let record = List.mapi ( fun i ({expr_var;env_elt}:environment_binding) ->
    Label (Var.to_name @@ Location.unwrap @@ expr_var),
    {associated_type=env_elt.type_value;michelson_annotation=None;decl_pos=i})
    @@ Environment.get_expr_environment ast_typed_env  in
    let module_var = Location.wrap @@ Var.of_name module_name in
    let record = ez_t_record record in
    Environment.add_ez_binder module_var record env
  )
  in
  List.fold_left aux env deps

let aggregate_contract order_deps asts_typed =
  (* Add the module at the beginning of the file *)
  let aux map (file_name,(_,_,_,deps_lst)) =
    let%bind (Ast_typed.Program_Fully_Typed contract,_) =
      trace_option (build_corner_case __LOC__ "Fail to find typed module") @@
      SMap.find_opt file_name asts_typed in
    let aux ast_typed (file_name,module_name) =
      let file_name = Location.wrap @@ Var.of_name file_name in
      let module_name = Location.wrap @@ Var.of_name module_name in
      let expr = Ast_typed.(make_e @@ E_variable file_name) @@ Ast_typed.t_unit () in
      ((Location.wrap @@ Ast_typed.Declaration_constant {binder=module_name;expr;inline=true})
      :: ast_typed)
    in
    let ast_typed = List.fold_left aux contract deps_lst in
    let map = SMap.add file_name ast_typed map in
    ok @@ map
  in
  let%bind asts_typed = bind_fold_list aux SMap.empty order_deps in
  (* Separate the program and the dependency (those are process differently) *)
  let%bind (file_name,(_,_,_,_deps_lst)),order_deps = match List.rev order_deps with
    | [] -> fail @@ build_corner_case __LOC__ "compiling nothing"
    | hd::tl -> ok @@ (hd,tl) in
  let%bind contract =
    trace_option (build_corner_case __LOC__ "Fail to find typed module") @@
    SMap.find_opt file_name asts_typed in
  (* Add all dependency at the beginning of the file *)
  let add_modules dep_types (file_name,(_,_,_, _deps_lst)) =
    let file_var = Location.wrap @@ Var.of_name file_name in
    (* Get the ast_type of the module *)
    let%bind ast_typed =
      trace_option (build_corner_case __LOC__ "Fail to find typed module") @@
      SMap.find_opt file_name asts_typed in
    (* Filter on Declaration_constant *)
    let aux decl decls = match Location.unwrap decl with
    | Ast_typed.Declaration_constant dc -> dc :: decls
    | Ast_typed.Declaration_type _ -> decls
    in
    let ast_typed = List.fold_right aux ast_typed [] in
    (* Don't do anything if the module only contain types *)
    if List.length ast_typed <> 0 then begin
      (* Put declaration list as let ... in record [] *)
      let record = List.map Ast_typed.(
        fun {binder;expr;inline=_} ->
        Label (Var.to_name binder.wrap_content),
        e_a_variable binder expr.type_expression
      ) ast_typed in
      let record = Ast_typed.(ez_e_a_record record) in
      let dep_types = SMap.add file_name record.type_expression dep_types in
      (* Put the declaration list as a serie of let ... in *)
      let expr   = List.fold_right Ast_typed.(
        fun {binder;expr;inline} record ->
        e_a_let_in binder inline expr @@ record
      ) ast_typed record in
      ok @@
      (dep_types,Some (Location.wrap @@ Ast_typed.Declaration_constant {binder=file_var;expr;inline=false}))
    end else begin ok @@ (dep_types,None) end
  in
  let%bind header_list = bind_fold_map_right_list add_modules (SMap.empty) @@ order_deps in
  let contract = List.fold_left (fun c a -> match a with Some a -> a::c | None -> c)
    contract header_list in
  ok @@ Ast_typed.Program_Fully_Typed contract

let type_file_with_dep ~options ~protocol_version asts_typed (file_name, (meta,form,c_unit,deps)) =
  let%bind ast_core = Compile.Utils.to_core ~options ~meta c_unit file_name in
  let aux (file_name,module_name) =
    let%bind ast_typed =
      trace_option (build_corner_case __LOC__
      "File typed before dependency. The build system is broken, contact the devs")
      @@ SMap.find_opt file_name asts_typed
    in
    ok @@ (module_name, ast_typed)
  in
  let%bind deps = bind_map_list aux deps in
  let%bind init_env   = Compile.Helpers.get_initial_env protocol_version in
  let init_env = add_modules_in_env init_env deps in
  let%bind ast_typed,ast_typed_env,_ = Compile.Of_core.compile ~typer_switch:options.typer_switch ~init_env form ast_core in
  ok @@ SMap.add file_name (ast_typed,ast_typed_env) asts_typed

let type_contract : options:Compiler_options.t -> string -> Compile.Of_core.form -> _ -> file_name -> (_, _) result =
  fun ~options syntax entry_point protocol_version file_name ->
    let%bind deps = dependency_graph syntax ~options entry_point file_name in
    let%bind order_deps = solve_graph deps file_name in
    let%bind asts_typed = bind_fold_list (type_file_with_dep ~options ~protocol_version) (SMap.empty) order_deps in
    ok @@ fst @@ SMap.find file_name asts_typed

let combined_contract : options:Compiler_options.t -> string -> _ -> _ -> file_name -> (_, _) result =
  fun ~options syntax entry_point protocol_version file_name ->
    let%bind deps = dependency_graph syntax ~options entry_point file_name in
    let%bind order_deps = solve_graph deps file_name in
    let%bind asts_typed = bind_fold_list (type_file_with_dep ~options ~protocol_version) (SMap.empty) order_deps in
    let%bind contract = aggregate_contract order_deps asts_typed in
    ok @@ contract

let build_mini_c : options:Compiler_options.t -> string -> _ -> _ -> file_name -> (_, _) result =
  fun ~options syntax entry_point protocol_version file_name ->
    let%bind contract = combined_contract ~options syntax entry_point protocol_version file_name in
    let%bind mini_c   = trace build_error_tracer @@ Compile.Of_typed.compile @@ contract in
    ok @@ mini_c

let build_contract : options:Compiler_options.t -> string -> string -> _ -> file_name -> (_, _) result =
  fun ~options syntax entry_point protocol_version file_name ->
    let%bind mini_c     = build_mini_c ~options syntax (Contract entry_point) protocol_version file_name in
    let%bind michelson  = trace build_error_tracer @@ Compile.Of_mini_c.aggregate_and_compile_contract mini_c entry_point in
    ok michelson
