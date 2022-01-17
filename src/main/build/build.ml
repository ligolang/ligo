
open Simple_utils
open Trace
open Main_errors

module type Params = sig
  val raise : all raise
  val add_warning : Main_warnings.all -> unit
  val options : Compiler_options.t
end

module M (Params : Params) =
  struct
    let raise = Params.raise
    let add_warning = Params.add_warning
    let options = Params.options
    type file_name = string
    type module_name = string
    type compilation_unit = Buffer.t
    type meta_data = Ligo_compile.Helpers.meta
    let preprocess : file_name -> compilation_unit * meta_data * (file_name * module_name) list =
      fun file_name ->
      let meta = Ligo_compile.Of_source.extract_meta ~raise "auto" file_name in
      let c_unit, deps = Ligo_compile.Helpers.preprocess_file ~raise ~meta ~options file_name in
      c_unit,meta,deps
    module AST = struct
      type declaration = Ast_typed.declaration_loc
      type t = declaration list
      type environment = Environment.t
      let add_ast_to_env : t -> environment -> environment = fun ast env ->
        Environment.append ast env
      let add_module_to_env : module_name -> environment -> environment -> environment =
        fun module_name ast_typed_env env ->
          Environment.add_module ~public:() module_name (Environment.to_program ast_typed_env) env
      let init_env : environment = options.init_env
      let make_module_declaration : module_name -> t -> declaration =
        fun module_binder ast_typed ->
        (Location.wrap @@ (Ast_typed.Declaration_module {module_binder;module_=ast_typed;module_attr={public=true}}: Ast_typed.declaration))
      let make_module_alias : module_name -> file_name -> declaration =
        fun module_name file_name ->
        Location.wrap @@ (Ast_typed.Module_alias {alias=module_name;binders=file_name,[]}: Ast_typed.declaration)
    end
    let compile : AST.environment -> file_name -> meta_data -> compilation_unit -> AST.t =
      fun env file_name meta c_unit ->
      let options = {options with init_env = env } in
      let ast_core = Ligo_compile.Utils.to_core ~raise ~add_warning ~options ~meta c_unit file_name in
      let inferred = Ligo_compile.Of_core.infer ~raise ~options ast_core in
      let ast_typed = Ligo_compile.Of_core.typecheck ~raise ~add_warning ~options Ligo_compile.Of_core.Env inferred in
      ast_typed

  end

module Infer (Params : Params) = struct
  include M(Params)
  module AST = struct
    include AST
    type declaration = Ast_core.declaration Location.wrap
    type t = declaration list
      type environment = Environment.core
      let add_ast_to_env : t -> environment -> environment = fun ast env ->
        Environment.append_core ast env
      let add_module_to_env : module_name -> environment -> environment -> environment =
        fun module_name ast_typed_env env ->
          Environment.add_core_module ~public:() module_name (Environment.to_core_program ast_typed_env) env
      let init_env : environment = Environment.init_core @@ Checking.untype_program @@ Environment.to_program @@ options.init_env
      let make_module_declaration : module_name -> t -> declaration =
        fun module_binder ast_typed ->
        (Location.wrap @@ (Ast_core.Declaration_module {module_binder;module_=ast_typed;module_attr={public=true}}: Ast_core.declaration))
      let make_module_alias : module_name -> file_name -> declaration =
        fun module_name file_name ->
        Location.wrap @@ (Ast_core.Module_alias {alias=module_name;binders=file_name,[]}: Ast_core.declaration)
  end

  let compile : AST.environment -> file_name -> meta_data -> compilation_unit -> AST.t =
    fun _ file_name meta c_unit ->
    Ligo_compile.Utils.to_core ~raise ~add_warning ~options ~meta c_unit file_name

end

module Build(Params : Params) = BuildSystem.Make(M(Params))

type file_name = string

let dependency_graph ~raise ~add_warning : options:Compiler_options.t -> string -> Ligo_compile.Of_core.form -> file_name -> _ =
  fun ~options _syntax _form file_name ->
    let open Build(struct
      let raise = raise
      let add_warning = add_warning
      let options = options
    end) in
    dependency_graph file_name

let infer_contract ~raise ~add_warning : options:Compiler_options.t -> file_name -> Ast_core.module_ =
  fun ~options main_file_name ->
    let open BuildSystem.Make(Infer(struct
      let raise = raise
      let add_warning = add_warning
      let options = options
    end)) in
    trace ~raise build_error_tracer @@ from_result (compile_separate main_file_name)

let type_contract ~raise ~add_warning : options:Compiler_options.t -> string -> Ligo_compile.Of_core.form -> file_name -> _ =
  fun ~options _syntax _entry_point file_name ->
    let open Build(struct
      let raise = raise
      let add_warning = add_warning
      let options = options
    end) in
    trace ~raise build_error_tracer @@ from_result (compile_separate file_name)

let combined_contract ~raise ~add_warning : options:Compiler_options.t -> 'a -> file_name -> Ast_typed.program =
  fun ~options _syntax file_name ->
    let open BuildSystem.Make(Infer(struct
      let raise = raise
      let add_warning = add_warning
      let options = options
    end)) in
    let contract = trace ~raise build_error_tracer @@ from_result (compile_combined file_name) in
    let contract = Ligo_compile.Of_core.typecheck ~raise ~add_warning ~options Env contract in
    contract

let build_typed ~raise ~add_warning :
  options:Compiler_options.t -> string -> Ligo_compile.Of_core.form -> file_name -> Ast_typed.program * Ast_typed.program =
    fun ~options _syntax entry_point file_name ->
      let open Build(struct
        let raise = raise
        let add_warning = add_warning
        let options = options
      end) in
      let contract = combined_contract ~raise ~add_warning ~options _syntax file_name in
      let applied =
        match entry_point with
        | Ligo_compile.Of_core.Contract entrypoint ->
          trace ~raise self_ast_typed_tracer @@ Self_ast_typed.all_contract entrypoint contract
        | View (view_name,main_name) ->
          trace ~raise self_ast_typed_tracer @@ Self_ast_typed.all_view view_name main_name contract
        | Env -> contract
      in
      applied, contract

let build_expression ~raise ~add_warning : options:Compiler_options.t -> string -> string -> file_name option -> _ =
  fun ~options syntax expression file_name ->
    let contract, aggregated_prg =
      match file_name with
      | Some init_file ->
         let module_ = combined_contract ~raise ~add_warning ~options syntax init_file in
         let contract = Ligo_compile.Of_typed.compile_program ~raise module_ in
         (module_, contract)
      | None -> ([], fun x -> Ligo_compile.Of_typed.compile_expression ~raise x)
    in
    let typed_exp       = Ligo_compile.Utils.type_expression ~raise ~options file_name syntax expression contract in
    let aggregated      = Ligo_compile.Of_typed.compile_expression_in_context ~raise typed_exp aggregated_prg in
    let mini_c_exp      = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
    (mini_c_exp ,aggregated)

(* TODO: this function could be called build_michelson_code since it does not really reflect a "contract" (no views, parameter/storage types) *)
let build_contract ~raise ~add_warning : options:Compiler_options.t -> string -> string -> file_name -> Stacking.compiled_expression * Ast_typed.program =
  fun ~options syntax entry_point file_name ->
    let typed_prg, contract = build_typed ~raise ~add_warning ~options syntax (Ligo_compile.Of_core.Contract entry_point) file_name in
    let aggregated = Ligo_compile.Of_typed.apply_to_entrypoint_contract ~raise typed_prg entry_point in
    let mini_c = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
    let michelson  = Ligo_compile.Of_mini_c.compile_contract ~raise ~options mini_c in
    michelson, contract

let build_views ~raise ~add_warning :
  options:Compiler_options.t -> string -> string -> string list * Ast_typed.program -> file_name -> (string * Stacking.compiled_expression) list =
  fun ~options syntax main_name (declared_views,program) source_file ->
    let views =
      let annotated_views = Ligo_compile.Of_typed.get_views @@ program in
      match declared_views with
      | [] -> List.map annotated_views ~f:fst
      | _ -> (
        (* detects whether a declared view (passed with --views command line option) overwrites an annotated view ([@view] let ..)*)
        let () = List.iter annotated_views
          ~f:(fun (x,loc) ->
            if not (List.mem declared_views x ~equal:String.equal) then
              add_warning (`Main_view_ignored loc)
          )
        in
        declared_views
      )
    in
    let f view_name =
      let _, contract  = build_typed ~raise ~add_warning:(fun _ -> ()) ~options syntax (Ligo_compile.Of_core.View (main_name,view_name)) source_file in
      let aggregated = Ligo_compile.Of_typed.apply_to_entrypoint_view ~raise contract view_name in
      let mini_c = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
      let michelson  = Ligo_compile.Of_mini_c.compile_view ~raise ~options mini_c in
      (view_name, michelson)
    in
    List.map ~f views

(* build_context builds a context to be used later for evaluation *)
let build_context ~raise ~add_warning :
  options:Compiler_options.t -> string -> file_name -> Ast_typed.program =
    fun ~options _syntax file_name ->
      let open Build(struct
        let raise = raise
        let add_warning = add_warning
        let options = options
      end) in
      let contract = trace ~raise build_error_tracer @@ Trace.from_result (compile_combined file_name) in
      contract
