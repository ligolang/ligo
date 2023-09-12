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
end

module Separate (Params : Params) = struct
  include M (Params)

  module AST = struct
    type t = Ast_typed.program
    type interface = Ast_typed.signature

    let link_interface
        Ast_typed.{ sig_sort = ss1; sig_items = si1 }
        Ast_typed.{ sig_sort = ss2; sig_items = si2 }
      =
      let open Ast_typed in
      let sig_sort =
        match ss1, ss2 with
        | Ss_contract x, Ss_module | Ss_module, Ss_contract x -> Ss_contract x
        | Ss_contract x, Ss_contract y ->
          ignore (x, y);
          (* interesting  *) assert false
        | _ -> Ss_module
      in
      { sig_sort; sig_items = si1 @ si2 }


    let link
        Ast_typed.{ pr_module = m1; pr_sig = s1 }
        Ast_typed.{ pr_module = m2; pr_sig = s2 }
      =
      Ast_typed.{ pr_module = m1 @ m2; pr_sig = link_interface s1 s2 }


    type environment = Ast_typed.sig_item list

    let init_env : environment = []

    let add_module_to_environment : module_name -> interface -> environment -> environment
      =
     fun module_binder module_intf env ->
      let module_binder = Module_var.of_input_var ~loc module_binder in
      let module_sig =
        Ast_typed.signature_make
          ~sig_items:module_intf.sig_items
          ~sig_sort:module_intf.sig_sort
      in
      env @ [ S_module (module_binder, module_sig) ]


    let add_interface_to_environment : interface -> environment -> environment =
     fun intf env ->
      List.fold intf.sig_items ~init:env ~f:(fun env decl -> env @ [ decl ])


    let make_module_in_ast : module_name -> t -> interface -> t -> t =
     fun module_binder module_ast module_intf ast ->
      let module_binder = Module_var.of_input_var ~loc module_binder in
      let new_decl =
        Location.wrap
          ~loc
          Ast_typed.(
            D_module
              { module_binder
              ; module_ =
                  { module_content = Module_expr.M_struct module_ast.pr_module
                  ; signature =
                      Ast_typed.signature_make
                        ~sig_items:module_intf.sig_items
                        ~sig_sort:module_intf.sig_sort
                  ; module_location = loc
                  }
              ; module_attr = { public = true; hidden = true }
              ; annotation = ()
              })
      in
      { ast with pr_module = new_decl :: ast.pr_module }


    let make_module_in_interface : module_name -> interface -> interface -> interface =
     fun module_binder module_intf ast ->
      let module_binder = Module_var.of_input_var ~loc module_binder in
      let new_item =
        Ast_typed.S_module
          ( module_binder
          , Ast_typed.signature_make
              ~sig_items:module_intf.sig_items
              ~sig_sort:module_intf.sig_sort )
      in
      { ast with sig_items = new_item :: ast.sig_items }
  end

  let lib_ast : unit -> AST.t = fun () -> std_lib.content_typed
  let lib_interface : unit -> AST.interface = fun () -> (lib_ast ()).pr_sig

  let compile
      :  AST.environment -> file_name -> meta_data -> compilation_unit
      -> AST.t * AST.interface
    =
   fun env file_name meta c_unit ->
    let syntax =
      Syntax.of_string_opt
        ~support_pascaligo:options.common.deprecated
        ~raise
        (Syntax_name "auto")
        (Some file_name)
    in
    let options = Compiler_options.set_syntax options (Some syntax) in
    let module_ = Ligo_compile.Utils.to_core ~raise ~options ~meta c_unit file_name in
    let module_ = Helpers.inject_declaration ~options ~raise syntax module_ in
    let prg =
      let context =
        Ast_typed.signature_make ~sig_items:env ~sig_sort:Ast_typed.ss_module
      in
      Ligo_compile.Of_core.typecheck_with_signature ~raise ~options ~context module_
    in
    prg, prg.pr_sig
end

module Infer (Params : Params) = struct
  include M (Params)

  module AST = struct
    type t = Ast_core.program

    let link t1 t2 = t1 @ t2

    type interface = unit list

    let link_interface t1 t2 = t1 @ t2

    type environment = unit

    let init_env : environment = ()

    let add_module_to_environment : module_name -> interface -> environment -> environment
      =
     fun _ _ () -> ()


    let add_interface_to_environment : interface -> environment -> environment =
     fun _ () -> ()


    let make_module_in_ast : module_name -> t -> interface -> t -> t =
     fun module_binder module_ast _module_intf ast ->
      let module_ = Location.wrap ~loc (Module_expr.M_struct module_ast) in
      let module_binder = Module_var.of_input_var ~loc module_binder in
      Location.wrap
        ~loc
        Ast_core.(
          D_module
            { module_binder
            ; module_
            ; module_attr = { public = true; hidden = true }
            ; annotation = None
            })
      :: ast


    let make_module_in_interface : module_name -> interface -> interface -> interface =
     fun _module_binder _ intf -> intf
  end

  let lib_ast : unit -> AST.t = fun () -> std_lib.content_core
  let lib_interface : unit -> AST.interface = fun () -> []

  let compile
      :  AST.environment -> file_name -> meta_data -> compilation_unit
      -> AST.t * AST.interface
    =
   fun () file_name meta c_unit ->
    let syntax =
      Syntax.of_string_opt
        ~support_pascaligo:options.common.deprecated
        ~raise
        (Syntax_name "auto")
        (Some file_name)
    in
    let options = Compiler_options.set_syntax options (Some syntax) in
    let module_ = Ligo_compile.Utils.to_core ~raise ~options ~meta c_unit file_name in
    Helpers.inject_declaration ~options ~raise syntax module_, []
end

module Build_typed (Params : Params) = BuildSystem.Make (Separate (Params))
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


let dependency_graph ~raise : options:Compiler_options.t -> Source_input.file_name -> _ =
 fun ~options filename ->
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
  let ast, _ =
    trace ~raise build_error_tracer @@ from_result (compile_qualified source)
  in
  ast


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
  let ast, _ =
    trace ~raise build_error_tracer
    @@ from_result (compile_qualified (Source_input.Raw input))
  in
  ast


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
  let ast, _ =
    trace ~raise build_error_tracer
    @@ from_result (compile_qualified (Source_input.Raw_input_lsp { file; code }))
  in
  ast


let qualified_typed ~raise
    : options:Compiler_options.t -> Source_input.code_input -> Ast_typed.program
  =
 fun ~options source ->
  let open Build_typed (struct
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
  let ast, _ =
    trace ~raise build_error_tracer @@ from_result (compile_qualified source)
  in
  ast


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
  let ast, _ = trace ~raise build_error_tracer @@ from_result (compile_qualified s) in
  Ligo_compile.Of_core.typecheck ~raise ~options ast


let qualified_typed_with_signature ~raise
    : options:Compiler_options.t -> Source_input.code_input -> Ast_typed.program
  =
 fun ~options source -> qualified_typed ~raise ~options source


type expression_michelson =
  { expression : Stacking.compiled_expression
  ; ast_type : Ast_aggregated.type_expression
  }

let build_expression ~raise
    :  options:Compiler_options.t -> Syntax_types.t -> string
    -> Source_input.file_name option -> expression_michelson Lwt.t
  =
 fun ~options syntax expression file_name_opt ->
  let open Lwt.Let_syntax in
  let init_prg =
    let f : Source_input.file_name -> Ast_typed.program =
     fun filename -> qualified_typed ~raise ~options (Source_input.From_file filename)
    in
    let default = Stdlib.select_lib_typed syntax (Stdlib.get ~options) in
    Option.value_map file_name_opt ~f ~default
  in
  let typed_exp =
    let init_sig =
      (* can't use the contract signature directly because
         it would force users to export declaration in Jsligo *)
      Ast_typed.to_signature init_prg.pr_module
    in
    Ligo_compile.Utils.type_expression ~raise ~options syntax expression init_sig
  in
  let aggregated =
    Ligo_compile.Of_typed.compile_expression_in_context
      ~raise
      ~options:options.middle_end
      ~force_uncurry:options.backend.function_body
      None
      init_prg
      typed_exp
  in
  let expanded_exp = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
  let mini_c_exp = Ligo_compile.Of_expanded.compile_expression ~raise expanded_exp in
  let%map stacking_exp =
    if options.backend.function_body
    then Ligo_compile.Of_mini_c.compile_expression_function ~raise ~options mini_c_exp
    else Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c_exp
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

type view_michelson = (Ligo_prim.Value_var.t, Stacking.compiled_expression) named

let rec build_contract_aggregated ~raise
    : options:Compiler_options.t -> string -> Source_input.code_input -> _
  =
 fun ~options module_ source ->
  let module_path = parse_module_path ~loc module_ in
  let typed_prg = qualified_typed ~raise ~options source in
  let typed_prg =
    trace ~raise self_ast_typed_tracer @@ Self_ast_typed.all_program typed_prg
  in
  let _sig, contract_sig =
    let sig_ = Ast_typed.to_extended_signature typed_prg in
    trace_option
      ~raise
      (`Self_ast_typed_tracer (Self_ast_typed.Errors.not_a_contract module_))
      (Ast_typed.Misc.get_contract_signature sig_ module_path)
  in
  let aggregated =
    Ligo_compile.Of_typed.apply_to_entrypoint_with_contract_type
      ~raise
      ~options:options.middle_end
      typed_prg
      module_path
      contract_sig
  in
  let agg_views =
    build_aggregated_views
      ~raise
      ~options
      ~storage_ty:contract_sig.storage
      module_path
      typed_prg
  in
  contract_sig, aggregated, agg_views


and build_contract_stacking ~raise
    :  options:Compiler_options.t -> string -> Source_input.code_input
    -> ((Stacking.compiled_expression * _)
       * ((Value_var.t * Stacking.compiled_expression) list * _))
       Lwt.t
  =
 fun ~options module_ source ->
  let open Lwt.Let_syntax in
  let _, aggregated, agg_views =
    build_contract_aggregated ~raise ~options module_ source
  in
  let expanded = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
  let mini_c = Ligo_compile.Of_expanded.compile_expression ~raise expanded in
  let%bind contract = Ligo_compile.Of_mini_c.compile_contract ~raise ~options mini_c in
  let%map views = build_views ~raise ~options agg_views in
  (contract, aggregated), (views, agg_views)


(* building a contract in michelson *)
and build_contract ~raise ~options module_ source =
  let open Lwt.Let_syntax in
  let%map (contract, _), (views, _) =
    build_contract_stacking ~raise ~options module_ source
  in
  let entrypoint = { name = Magic_vars.generated_main; value = contract } in
  let views = List.map ~f:(fun (name, value) -> { name; value }) views in
  { entrypoint; views }


(* Meta ligo needs contract and views as aggregated programs *)
and build_contract_meta_ligo ~raise ~options file_name =
  let open Lwt.Let_syntax in
  let%map (_, contract), (_, views) =
    build_contract_stacking ~raise ~options "" (Source_input.From_file file_name)
  in
  contract, views


and build_aggregated_views ~raise
    :  options:Compiler_options.t -> storage_ty:Ast_typed.ty_expr -> Module_var.t list
    -> Ast_typed.program -> (Value_var.t list * Ast_aggregated.expression) option
  =
 fun ~options ~storage_ty module_path contract ->
  let module_upd_views, view_info =
    Self_ast_typed.Helpers.update_module
      module_path
      (Ast_typed.fetch_views_in_module ~storage_ty)
      contract.pr_module
  in
  let contract = { contract with pr_module = module_upd_views } in
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
    -> (Value_var.t * Stacking.compiled_expression) list Lwt.t
  =
 fun ~options lst_opt ->
  let open Lwt.Let_syntax in
  match lst_opt with
  | None -> Lwt.return []
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
      view, idx
    in
    let views = List.mapi ~f:aux view_names in
    let aux (vn, mini_c) =
      let%map view = Ligo_compile.Of_mini_c.compile_view ~raise ~options mini_c in
      vn, view
    in
    let%map michelsons = Lwt_list.map_p aux views in
    let () =
      Ligo_compile.Of_michelson.check_view_restrictions
        ~raise
        (List.map ~f:snd michelsons)
    in
    michelsons
