open Simple_utils
open Trace
open Main_errors
open Ligo_prim
module Stdlib = Stdlib
module Source_input = BuildSystem.Source_input

let loc = Location.env

module type Params = sig
  val raise : (all, Main_warnings.all) raise
  val options : Compiler_options.t
  val std_lib : Stdlib.t
  val top_level_syntax : Syntax_types.t
end

module M (Params : Params) = struct
  let raise = Params.raise
  let options = Params.options
  let std_lib = Params.std_lib
  let top_level_syntax = Params.top_level_syntax

  type file_name = Source_input.file_name
  type raw_input = Source_input.raw_input
  type code_input = Source_input.code_input
  type module_name = string
  type compilation_unit = Buffer.t
  type meta_data = Ligo_compile.Helpers.meta

  let preprocess
      : code_input -> compilation_unit * meta_data * (file_name * module_name) list
    =
   fun code_input ->
    let syntax =
      Syntax.of_string_opt
        ~support_pascaligo:options.common.deprecated
        ~raise
        (Syntax_name "auto")
        (match code_input with
        | HTTP uri -> Some (Http_uri.get_filename uri)
        | From_file file_name -> Some file_name
        | Raw { id; _ } -> Some id
        | Raw_input_lsp { file; _ } -> Some file)
    in
    let meta = Ligo_compile.Of_source.extract_meta syntax in
    let c_unit, deps =
      match code_input with
      | HTTP uri ->
        let code = Http_uri.fetch uri in
        Ligo_compile.Helpers.preprocess_string ~raise ~meta ~options:options.frontend code
      | From_file file_name ->
        Ligo_compile.Helpers.preprocess_file
          ~raise
          ~meta
          ~options:options.frontend
          file_name
      | Raw { id = _; code } ->
        Ligo_compile.Helpers.preprocess_string ~raise ~meta ~options:options.frontend code
      | Raw_input_lsp { file; code } ->
        Ligo_compile.Helpers.preprocess_raw_input
          ~raise
          ~meta
          ~options:options.frontend
          file
          code
    in
    c_unit, meta, deps


  module AST = struct
    type declaration = Ast_typed.declaration
    type signature = Ast_typed.signature
    type t = Ast_typed.program
    type sig_environment = Environment.signature
    type environment = Environment.t

    let add_ast_to_env : t -> environment -> environment =
     fun ast env -> Environment.append env ast


    let add_module_to_env
        : module_name -> environment -> sig_environment -> environment -> environment
      =
     fun module_name ast_typed_env sig_env env ->
      let module_name = Module_var.of_input_var ~loc module_name in
      Environment.add_module
        ~public:()
        module_name
        (Environment.to_module ast_typed_env)
        sig_env
        env


    let init_env : environment =
      let type_env = options.middle_end.init_env in
      Environment.append type_env std_lib.content_typed


    let make_module_declaration : module_name -> t -> signature -> declaration =
     fun module_binder ast_typed signature ->
      let module_ =
        Ast_typed.
          { module_content = Module_expr.M_struct ast_typed
          ; module_location = loc
          ; signature
          }
      in
      let module_binder = Module_var.of_input_var ~loc module_binder in
      Location.wrap
        ~loc
        Ast_typed.(
          D_module
            { module_binder; module_; module_attr = { public = true; hidden = true } })
  end

  let lib_ast : unit -> AST.t = fun () -> std_lib.content_typed

  let compile : AST.environment -> file_name -> meta_data -> compilation_unit -> AST.t =
   fun env file_name meta c_unit ->
    let options = Compiler_options.set_init_env options env in
    let core_c_unit = Ligo_compile.Utils.to_core ~raise ~options ~meta c_unit file_name in
    let ast_core =
      let syntax =
        Syntax.of_string_opt
          ~raise
          ~support_pascaligo:options.common.deprecated
          (Syntax_name "auto")
          (Some file_name)
      in
      Helpers.inject_declaration ~options ~raise syntax core_c_unit
    in
    Ligo_compile.Of_core.typecheck ~raise ~options ast_core
end

module Infer (Params : Params) = struct
  include M (Params)

  module AST = struct
    type declaration = Ast_core.declaration
    type t = Ast_core.program
    type environment = unit

    let add_ast_to_env : t -> environment -> environment = fun _ () -> ()

    let add_module_to_env : module_name -> environment -> environment -> environment =
     fun _ () () -> ()


    let init_env : environment = ()

    let make_module_declaration : module_name -> t -> declaration =
     fun module_binder ast_typed ->
      let module_ = Location.wrap ~loc (Module_expr.M_struct ast_typed) in
      let module_binder = Module_var.of_input_var ~loc module_binder in
      Location.wrap
        ~loc
        Ast_core.(
          D_module
            { module_binder; module_; module_attr = { public = true; hidden = true } })
  end

  let lib_ast : unit -> AST.t = fun () -> std_lib.content_core

  let compile : AST.environment -> file_name -> meta_data -> compilation_unit -> AST.t =
   fun () file_name meta c_unit ->
    let module_ = Ligo_compile.Utils.to_core ~raise ~options ~meta c_unit file_name in
    let module_ =
      let syntax =
        Syntax.of_string_opt
          ~support_pascaligo:options.common.deprecated
          ~raise
          (Syntax_name "auto")
          (Some file_name)
      in
      Helpers.inject_declaration ~options ~raise syntax module_
    in
    module_
end

(*  unfortunately slow:
  module Build_typed(Params : Params) = BuildSystem.Make(M(Params))
*)
module Build_core (Params : Params) = BuildSystem.Make (Infer (Params))

let get_top_level_syntax ~options ?filename () : Syntax_types.t =
  match Compiler_options.(options.frontend.syntax) with
  | Some x -> x
  | None ->
    (match
       Trace.to_option
       @@ Syntax.of_string_opt
            ~support_pascaligo:options.common.deprecated
            (Syntax_name "auto")
            filename
     with
    | Some x -> x
    | None -> failwith "Top-level syntax not found")


let dependency_graph ~raise
    :  options:Compiler_options.t -> ?cform:Ligo_compile.Of_core.form
    -> Source_input.file_name -> _
  =
 fun ~options ?cform:_ filename ->
  let open Build_core (struct
    let raise = raise
    let options = options
    let std_lib = Stdlib.get ~options
    let top_level_syntax = get_top_level_syntax ~options ~filename ()
  end) in
  dependency_graph (Source_input.From_file filename)


(* unqualified usages : list-declaration ; print *)
let unqualified_core ~raise
    : options:Compiler_options.t -> Source_input.file_name -> Ast_core.program
  =
 fun ~options filename ->
  let std_lib = Stdlib.get ~options in
  let open Build_core (struct
    let raise = raise
    let options = options
    let std_lib = std_lib
    let top_level_syntax = get_top_level_syntax ~options ~filename ()
  end) in
  trace ~raise build_error_tracer
  @@ from_result (compile_unqualified (Source_input.From_file filename))


let qualified_core ~raise
    : options:Compiler_options.t -> Source_input.code_input -> Ast_core.program
  =
 fun ~options source ->
  let open Build_core (struct
    let raise = raise
    let options = options
    let std_lib = Stdlib.get ~options

    let top_level_syntax =
      match source with
      | HTTP uri -> get_top_level_syntax ~options ~filename:(Http_uri.get_filename uri) ()
      | Raw_input_lsp _ -> Syntax_types.CameLIGO
      | From_file filename -> get_top_level_syntax ~options ~filename ()
      | Raw _ -> Syntax_types.CameLIGO
  end) in
  trace ~raise build_error_tracer @@ from_result (compile_qualified source)


let qualified_core_from_string ~raise
    : options:Compiler_options.t -> Source_input.raw_input -> Ast_core.program
  =
 fun ~options input ->
  let std_lib = Stdlib.get ~options in
  let open Build_core (struct
    let raise = raise
    let options = options
    let std_lib = std_lib
    let top_level_syntax = get_top_level_syntax ~options ~filename:input.id ()
  end) in
  trace ~raise build_error_tracer
  @@ from_result (compile_qualified (Source_input.Raw input))


let qualified_core_from_raw_input ~raise
    : options:Compiler_options.t -> string -> string -> Ast_core.program
  =
 fun ~options file code ->
  let std_lib = Stdlib.get ~options in
  let open Build_core (struct
    let raise = raise
    let options = options
    let std_lib = std_lib
    let top_level_syntax = get_top_level_syntax ~options ~filename:file ()
  end) in
  trace ~raise build_error_tracer
  @@ from_result (compile_qualified (Source_input.Raw_input_lsp { file; code }))


let qualified_typed ~raise
    :  options:Compiler_options.t -> ?cform:Ligo_compile.Of_core.form
    -> Source_input.code_input -> Ast_typed.program
  =
 fun ~options ?cform source ->
  (* let std_lib = Stdlib.get ~options in
    let open Build_typed(struct
      let raise = raise
      let options = options
      let std_lib = std_lib
      let top_level_syntax = get_top_level_syntax ~options ~filename ()
    end) in
    let prg = trace ~raise build_error_tracer @@ from_result (compile_qualified (Source_input.From_file filename)) in *)
  let prg = qualified_core ~raise ~options source in
  Ligo_compile.Of_core.typecheck ~raise ~options ?cform prg


let qualified_typed_str ~raise : options:Compiler_options.t -> string -> Ast_typed.program
  =
 fun ~options code ->
  let std_lib = Stdlib.get ~options in
  let open Build_core (struct
    (* idially should use Build_typed *)
    let raise = raise
    let options = options
    let std_lib = std_lib
    let top_level_syntax = get_top_level_syntax ~options ()
  end) in
  let id =
    match options.frontend.syntax with
    | Some s -> "from_build" ^ Syntax.to_ext s
    | None -> "from_build"
  in
  let s = Source_input.Raw { code; id } in
  let x = trace ~raise build_error_tracer @@ from_result (compile_qualified s) in
  Ligo_compile.Of_core.typecheck ~raise ~options x


let qualified_typed_with_signature ~raise
    :  options:Compiler_options.t -> ?cform:Ligo_compile.Of_core.form
    -> Source_input.code_input -> Ast_typed.program * Ast_typed.signature
  =
 fun ~options ?cform source ->
  (* let std_lib = Stdlib.get ~options in
    let open Build_typed(struct
      let raise = raise
      let options = options
      let std_lib = std_lib
      let top_level_syntax = get_top_level_syntax ~options ~filename ()
    end) in
    let prg = trace ~raise build_error_tracer @@ from_result (compile_qualified (Source_input.From_file filename)) in *)
  let prg = qualified_core ~raise ~options source in
  Ligo_compile.Of_core.typecheck_with_signature ~raise ~options ?cform prg


type expression_mini_c =
  { expression : Mini_c.expression
  ; ast_type : Ast_aggregated.type_expression
  }

type expression_michelson =
  { expression : Stacking.compiled_expression
  ; ast_type : Ast_aggregated.type_expression
  }

let build_expression ~raise
    :  options:Compiler_options.t -> Syntax_types.t -> string
    -> Source_input.file_name option -> expression_michelson
  =
 fun ~options syntax expression file_name_opt ->
  let init_prg =
    let f : Source_input.file_name -> Ast_typed.program =
     fun filename -> qualified_typed ~raise ~options (Source_input.From_file filename)
    in
    let default = Stdlib.select_lib_typed syntax (Stdlib.get ~options) in
    Option.value_map file_name_opt ~f ~default
  in
  let typed_exp =
    Ligo_compile.Utils.type_expression ~raise ~options syntax expression init_prg
  in
  let aggregated =
    Ligo_compile.Of_typed.compile_expression_in_context
      ~raise
      ~options:options.middle_end
      init_prg
      typed_exp
  in
  let expanded_exp = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
  let mini_c_exp = Ligo_compile.Of_expanded.compile_expression ~raise expanded_exp in
  let stacking_exp =
    Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c_exp
  in
  { expression = stacking_exp; ast_type = aggregated.type_expression }


let parse_module_path ~loc s =
  if String.equal s ""
  then []
  else (
    let ms = String.split s ~on:'.' in
    List.map ~f:(Module_var.of_input_var ~loc) ms)


type ('a, 'b) named =
  { name : 'a
  ; value : 'b
  }

type contract_michelson =
  { entrypoint : (Value_var.t, Stacking.compiled_expression) named
  ; views : (Value_var.t, Stacking.compiled_expression) named list
  }

let rec build_contract_aggregated ~raise
    :  options:Compiler_options.t -> string list -> string -> string list
    -> Source_input.code_input -> _
  =
 fun ~options entry_points module_ cli_views source ->
  let module_path = parse_module_path ~loc module_ in
  let typed_prg = qualified_typed ~raise ~options source in
  let contract_info, typed_contract =
    trace ~raise self_ast_typed_tracer
    @@ Ligo_compile.Of_core.specific_passes
         (Ligo_compile.Of_core.Contract { entrypoints = entry_points; module_path })
         typed_prg
  in
  let entry_point, contract_type = contract_info in
  let _, typed_views =
    let form =
      let command_line_views =
        match cli_views with
        | [] -> None
        | x -> Some x
      in
      Ligo_compile.Of_core.View
        { command_line_views; contract_entry = entry_point; module_path; contract_type }
    in
    trace ~raise self_ast_typed_tracer
    @@ Ligo_compile.Of_core.specific_passes form typed_prg
  in
  let aggregated =
    Ligo_compile.Of_typed.apply_to_entrypoint_contract
      ~raise
      ~options:options.middle_end
      ~contract_pass:true
      typed_contract
      entry_point
      module_path
  in
  let agg_views =
    match typed_views with
    | [] -> None
    | _ -> build_aggregated_views ~raise ~options module_path typed_views
  in
  let parameter_ty, storage_ty =
    trace_option
      ~raise
      (`Self_ast_aggregated_tracer
        (Self_ast_aggregated.Errors.corner_case "Could not recover types from contract"))
      (let open Simple_utils.Option in
      let open Ast_aggregated in
      let* { type1 = input_ty; _ } =
        Ast_aggregated.get_t_arrow aggregated.type_expression
      in
      Ast_aggregated.get_t_pair input_ty)
  in
  entry_point, (parameter_ty, storage_ty), aggregated, agg_views


and build_contract_stacking ~raise
    :  options:Compiler_options.t -> string list -> string -> string list
    -> Source_input.code_input
    -> _
       * (Stacking.compiled_expression * _)
       * ((Value_var.t * Stacking.compiled_expression) list * _)
  =
 fun ~options entry_points module_ cli_views source ->
  let entrypoint, _, aggregated, agg_views =
    build_contract_aggregated ~raise ~options entry_points module_ cli_views source
  in
  let expanded = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
  let mini_c = Ligo_compile.Of_expanded.compile_expression ~raise expanded in
  let contract = Ligo_compile.Of_mini_c.compile_contract ~raise ~options mini_c in
  let views = build_views ~raise ~options agg_views in
  entrypoint, (contract, aggregated), (views, agg_views)


(* building a contract in michelson *)
and build_contract ~raise ~options entry_point module_ views source =
  let entrypoint, (contract, _), (views, _) =
    build_contract_stacking ~raise ~options entry_point module_ views source
  in
  let entrypoint = { name = entrypoint; value = contract } in
  let views = List.map ~f:(fun (name, value) -> { name; value }) views in
  { entrypoint; views }


(* Meta ligo needs contract and views as aggregated programs *)
and build_contract_meta_ligo ~raise ~options entry_point views file_name =
  let entry_point, (_, contract), (_, views) =
    build_contract_stacking
      ~raise
      ~options
      entry_point
      ""
      views
      (Source_input.From_file file_name)
  in
  entry_point, contract, views


and build_aggregated_views ~raise
    :  options:Compiler_options.t -> Module_var.t list -> Ast_typed.program
    -> (Value_var.t list * Ast_aggregated.expression) option
  =
 fun ~options module_path contract ->
  let contract, view_info =
    Self_ast_typed.Helpers.update_module
      module_path
      Ast_typed.Helpers.fetch_views_in_program
      contract
  in
  match view_info with
  | [] -> None
  | _ ->
    let aggregated =
      Ligo_compile.Of_typed.apply_to_entrypoint_view
        ~raise:{ raise with warning = (fun _ -> ()) }
        ~options:options.middle_end
        module_path
        contract
        view_info
    in
    let view_names = List.map ~f:(fun (_, b) -> Binder.get_var b) view_info in
    Some (view_names, aggregated)


and build_views ~raise
    :  options:Compiler_options.t -> (Value_var.t list * Ast_aggregated.expression) option
    -> (Value_var.t * Stacking.compiled_expression) list
  =
 fun ~options lst_opt ->
  match lst_opt with
  | None -> []
  | Some (view_names, aggregated) ->
    let expanded = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
    let mini_c = Ligo_compile.Of_expanded.compile_expression ~raise expanded in
    let mini_c =
      trace ~raise self_mini_c_tracer @@ Self_mini_c.all_expression options mini_c
    in
    let mini_c_tys =
      trace_option
        ~raise
        (`Self_mini_c_tracer
          (Self_mini_c.Errors.corner_case "Error reconstructing type of views"))
      @@ Mini_c.get_t_tuple mini_c.type_expression
    in
    let nb_of_views = List.length view_names in
    let aux i view =
      let idx_ty =
        trace_option
          ~raise
          (`Self_mini_c_tracer
            (Self_mini_c.Errors.corner_case "Error reconstructing type of view"))
        @@ List.nth mini_c_tys i
      in
      let idx = Mini_c.e_proj mini_c idx_ty i nb_of_views in
      (* let idx = trace ~raise self_mini_c_tracer @@ Self_mini_c.all_expression options idx in *)
      view, idx
    in
    let views = List.mapi ~f:aux view_names in
    let aux (vn, mini_c) =
      vn, Ligo_compile.Of_mini_c.compile_view ~raise ~options mini_c
    in
    let michelsons = List.map ~f:aux views in
    let () =
      Ligo_compile.Of_michelson.check_view_restrictions
        ~raise
        (List.map ~f:snd michelsons)
    in
    michelsons
