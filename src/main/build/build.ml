module Trace = Simple_utils.Trace
module Http_uri = Simple_utils.Http_uri
open Main_errors
open Ligo_prim
module Stdlib = Stdlib
module Source_input = BuildSystem.Source_input

let loc = Location.env

module type Params = sig
  val raise : (all, Main_warnings.all) Trace.raise
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
  type imports = file_name list

  let preprocess
      : code_input -> compilation_unit * meta_data * (file_name * module_name) list
    =
   fun code_input ->
    let file_name = Source_input.id_of_code_input code_input in
    let syntax = Syntax.of_string_opt ~raise (Syntax_name "auto") (Some file_name) in
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


    type environment = Checking.Persistent_env.t

    let init_env : environment = Checking.Persistent_env.empty

    let add_module_to_environment
        : file_name -> module_name -> imports -> interface -> environment -> environment
      =
     fun path module_binder imports module_intf env ->
      let module_binder = Module_var.of_input_var ~loc module_binder in
      Checking.Persistent_env.add_signature env module_binder path imports module_intf


    let add_interface_to_environment : interface -> environment -> environment =
     fun intf env -> Checking.Persistent_env.add_virtual env intf


    let make_module_in_ast : module_name -> t -> t -> t =
     fun module_binder module_ast ast ->
      let module_binder = Module_var.of_input_var ~loc module_binder in
      let new_decl =
        Location.wrap
          ~loc
          Ast_typed.(
            D_module
              { module_binder
              ; module_ =
                  { module_content = Module_expr.M_struct module_ast.pr_module
                  ; signature = module_ast.pr_sig
                  ; module_location = loc
                  }
              ; module_attr =
                  { Type_or_module_attr.default_attributes with hidden = true }
              ; annotation = ()
              })
      in
      { ast with pr_module = new_decl :: ast.pr_module }


    let make_module_in_interface : module_name -> interface -> interface -> interface =
     fun module_binder module_intf ast ->
      let module_binder = Module_var.of_input_var ~loc module_binder in
      let new_item =
        Location.wrap ~loc
        @@ Ast_typed.S_module
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
    let syntax = Syntax.of_string_opt ~raise (Syntax_name "auto") (Some file_name) in
    let options = Compiler_options.set_syntax options (Some syntax) in
    let module_ = Ligo_compile.Utils.to_core ~raise ~options ~meta c_unit file_name in
    let module_ = Helpers.inject_declaration ~options ~raise syntax module_ in
    let prg =
      Ligo_compile.Of_core.typecheck_with_signature ~raise ~options ~context:env module_
    in
    prg, prg.pr_sig


  let link_imports : AST.t -> intfs:AST.environment -> AST.t =
   fun prg ~intfs ->
    let module_ =
      prg.pr_module
      |> Self_ast_typed.Helpers.Declaration_mapper.map_module
         @@ fun decl ->
         let loc = decl.location in
         match Location.unwrap decl with
         | D_import
             (Import_rename
               { alias = import_name; imported_module = mangled_module_name; import_attr })
           ->
           (* Create module alias for mangled module *)
           let intf = Checking.Persistent_env.find_signature intfs mangled_module_name in
           Location.wrap ~loc
           @@ Ast_typed.D_module
                { module_binder = import_name
                ; module_ =
                    { module_content = M_variable mangled_module_name
                    ; signature = intf
                    ; module_location = Location.generated
                    }
                ; module_attr = import_attr
                ; annotation = ()
                }
         | _ -> decl
    in
    { prg with pr_module = module_ }
end

module Infer (Params : Params) = struct
  include M (Params)

  module AST = struct
    type t = Ast_core.program
    type imports = module_name list

    let link t1 t2 = t1 @ t2

    type interface = unit list

    let link_interface t1 t2 = t1 @ t2

    type environment = unit

    let init_env : environment = ()

    let add_module_to_environment
        : file_name -> module_name -> imports -> interface -> environment -> environment
      =
     fun _ _ _ _ () -> ()


    let add_interface_to_environment : interface -> environment -> environment =
     fun _ () -> ()


    let make_module_in_ast : module_name -> t -> t -> t =
     fun module_binder module_ast ast ->
      let module_ = Location.wrap ~loc (Module_expr.M_struct module_ast) in
      let module_binder = Module_var.of_input_var ~loc module_binder in
      Location.wrap
        ~loc
        Ast_core.(
          D_module
            { module_binder
            ; module_
            ; module_attr = { Type_or_module_attr.default_attributes with hidden = true }
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
    let syntax = Syntax.of_string_opt ~raise (Syntax_name "auto") (Some file_name) in
    let options = Compiler_options.set_syntax options (Some syntax) in
    let module_ = Ligo_compile.Utils.to_core ~raise ~options ~meta c_unit file_name in
    Helpers.inject_declaration ~options ~raise syntax module_, []


  let link_imports : AST.t -> intfs:AST.environment -> AST.t =
   fun prg ~intfs:_ ->
    let open Ast_core in
    prg
    |> Ast_core.Helpers.Declaration_mapper.map_module
       @@ fun decl ->
       let loc = decl.location in
       match Location.unwrap decl with
       | D_import
           (Import_rename { alias; imported_module = mangled_module_name; import_attr })
         ->
         Location.wrap ~loc
         @@ D_module
              { module_binder = alias
              ; module_ = Location.wrap ~loc (Module_expr.M_variable mangled_module_name)
              ; module_attr = import_attr
              ; annotation = None
              }
       | _ -> decl
end

module Separate_v2 (Params : Params) = struct
  include Separate (Params)

  let raise = Params.raise
  let options = Params.options

  type file_name = Source_input.file_name
  type raw_input = Source_input.raw_input
  type code_input = Source_input.code_input
  type module_name = string
  type compilation_unit = Ast_core.program
  type meta_data = Ligo_compile.Helpers.meta
  type imports = file_name list

  let preprocess
      : code_input -> compilation_unit * meta_data * (file_name * module_name) list
    =
   fun code_input ->
    let c_unit, meta, _ = preprocess code_input in
    let Ligo_compile.Helpers.{ syntax } = meta in
    let file_name = Source_input.id_of_code_input code_input in
    let dir = Filename.dirname file_name in
    let options = Compiler_options.set_syntax options (Some syntax) in
    let c_unit =
      c_unit
      |> Fn.flip (Ligo_compile.Utils.to_core ~raise ~options ~meta) file_name
      |> Helpers.inject_declaration ~options ~raise syntax
      |> Helpers.elaborate_imports dir
    in
    let get_deps =
      match syntax with
      | CameLIGO -> failwith "Ast_core.Ligo_dep_cameligo.dependencies missing"
      | JsLIGO -> Ast_core.Ligo_dep_jsligo.dependencies
    in
    (* Duplicating path, because function signature requires file_name * module_name *)
    (* which are the same at this point *)
    let deps = List.map ~f:(fun path -> path, path) (get_deps c_unit) in
    c_unit, meta, deps


  let compile
      :  AST.environment -> file_name -> meta_data -> compilation_unit
      -> AST.t * AST.interface
    =
   fun env file_name meta c_unit ->
    let Ligo_compile.Helpers.{ syntax } = meta in
    let options = Compiler_options.set_syntax options (Some syntax) in
    let prg =
      Ligo_compile.Of_core.typecheck_with_signature ~raise ~options ~context:env c_unit
    in
    prg, prg.pr_sig


  let link_imports : AST.t -> intfs:AST.environment -> AST.t =
   fun prg ~intfs ->
    let module_ =
      let rec f decl =
        let loc = decl.Location.location in
        match Location.unwrap decl with
        | Ast_typed.D_import
            (Import_all_as { alias = import_name; module_str; import_attr }) ->
          let imported_module = Module_var.of_input_var ~loc module_str in
          (* Create module alias for imported module *)
          let intf = Checking.Persistent_env.find_signature intfs imported_module in
          [ Location.wrap ~loc
            @@ Ast_typed.D_module
                 { module_binder = import_name
                 ; module_ =
                     { module_content = M_variable imported_module
                     ; signature = intf
                     ; module_location = Location.generated
                     }
                 ; module_attr = import_attr
                 ; annotation = ()
                 }
          ]
        | D_import (Import_selected { imported; module_str; import_attr }) ->
          let imported_module = Module_var.of_input_var ~loc module_str in
          let Ast_typed.{ sig_items = intf; _ } =
            Checking.Persistent_env.find_signature intfs imported_module
          in
          let Simple_utils.Ne_list.(h :: tl) = imported in
          let imported = h :: tl in
          let get_value_type (var : Value_var.t) : Ast_typed.type_expression =
            (* Type of the imported value must be inside the signature after typecheck *)
            List.find_map_exn intf ~f:(fun item ->
                match Location.unwrap item with
                | Ast_typed.S_value (v, t, _) ->
                  if Value_var.equal var v then Some t else None
                | _ -> None)
          in
          (* makes `let x = External_module_name.x` entry *)
          let make_value var =
            let type_ = get_value_type var in
            let binder = Binder.make var type_ in
            let expr =
              Ast_typed.
                { expression_content =
                    Ast_typed.E_module_accessor
                      { module_path = [ imported_module ]; element = var }
                ; location = Location.generated
                ; type_expression = type_
                }
            in
            let attr =
              { Value_attr.default_attributes with public = import_attr.public }
            in
            Location.wrap ~loc @@ Ast_typed.(D_value Value_decl.{ binder; expr; attr })
          in
          List.map imported ~f:make_value
        | D_module ({ module_; _ } as decl) ->
          let module_ =
            match module_.module_content with
            | M_struct module_ast ->
              let module_ast = List.concat_map module_ast ~f in
              { module_ with module_content = M_struct module_ast }
            | _ -> module_
          in
          [ Location.wrap ~loc @@ Ast_typed.D_module { decl with module_ } ]
        (* At this point all Import_rename decls must be replaced with the D_module ones *)
        | D_import (Import_rename _) ->
          failwith "Import_rename declaration persists after nanopasses"
        | D_value _
        | D_irrefutable_match _
        | D_type _
        | D_module_include _
        | D_signature _ -> [ decl ]
      in
      List.concat_map prg.pr_module ~f
    in
    { prg with pr_module = module_ }
end

module Build_typed (Params : Params) = BuildSystem.Make (Separate (Params))
module Build_typed_v2 (Params : Params) = BuildSystem.Make (Separate_v2 (Params))
module Build_core (Params : Params) = BuildSystem.Make (Infer (Params))

let get_top_level_syntax ~options ?filename () : Syntax_types.t =
  match Compiler_options.(options.frontend.syntax) with
  | Some x -> x
  | None ->
    (match Trace.to_option @@ Syntax.of_string_opt (Syntax_name "auto") filename with
    | Some x -> x
    | None -> failwith "Top-level syntax not found")


let top_level_syntax_of_code_input ~options : Source_input.code_input -> Syntax_types.t =
 fun code_input ->
  let filename = Source_input.id_of_code_input code_input in
  get_top_level_syntax ~options ~filename ()


let dependency_graph ~raise : options:Compiler_options.t -> Source_input.code_input -> _ =
 fun ~options code_input ->
  let open Build_core (struct
    let raise = raise
    let options = options
    let std_lib = Stdlib.get ~options

    let top_level_syntax =
      get_top_level_syntax
        ~options
        ~filename:(Source_input.id_of_code_input code_input)
        ()
  end) in
  dependency_graph code_input


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
  Trace.trace ~raise build_error_tracer
  @@ Trace.from_result (compile_unqualified (Source_input.From_file filename))


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
    Trace.trace ~raise build_error_tracer @@ Trace.from_result (compile_qualified source)
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
    Trace.trace ~raise build_error_tracer
    @@ Trace.from_result (compile_qualified (Source_input.Raw input))
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
    Trace.trace ~raise build_error_tracer
    @@ Trace.from_result (compile_qualified (Source_input.Raw_input_lsp { file; code }))
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
    Trace.trace ~raise build_error_tracer @@ Trace.from_result (compile_qualified source)
  in
  ast


let qualified_typed_v2 ~raise
    : options:Compiler_options.t -> Source_input.code_input -> Ast_typed.program
  =
 fun ~options source ->
  let open Build_typed_v2 (struct
    let raise = raise
    let options = options
    let std_lib = Stdlib.get ~options
    let top_level_syntax = top_level_syntax_of_code_input ~options source
  end) in
  let ast, _ =
    Trace.trace ~raise build_error_tracer @@ Trace.from_result (compile_qualified source)
  in
  ast


let qualified_typed_with_env ~raise
    :  options:Compiler_options.t -> Source_input.code_input
    -> Ast_typed.program * Checking.Persistent_env.t
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
  let ast, intfs =
    Trace.trace ~raise build_error_tracer @@ Trace.from_result (compile_qualified source)
  in
  ast, intfs


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
  let ast, _ =
    Trace.trace ~raise build_error_tracer @@ Trace.from_result (compile_qualified s)
  in
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
      Checking.Persistent_env.of_init_sig @@ Ast_typed.to_signature init_prg.pr_module
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


let build_type_expression ~raise
    :  options:Compiler_options.t -> Syntax_types.t -> string
    -> Source_input.file_name option
    -> (Mini_c.meta, string) Tezos_micheline.Micheline.node
  =
 fun ~options syntax ty_expression file_name_opt ->
  let init_prg =
    let f : Source_input.file_name -> Ast_typed.program =
     fun filename -> qualified_typed ~raise ~options (Source_input.From_file filename)
    in
    let default = Stdlib.select_lib_typed syntax (Stdlib.get ~options) in
    Option.value_map file_name_opt ~f ~default
  in
  let init_sig =
    (* can't use the contract signature directly because
       it would force users to export declaration in Jsligo *)
    Checking.Persistent_env.of_init_sig @@ Ast_typed.to_signature init_prg.pr_module
  in
  Ligo_compile.Utils.type_ty_expression ~raise ~options syntax ty_expression init_sig


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
    Trace.trace ~raise self_ast_typed_tracer @@ Self_ast_typed.all_program typed_prg
  in
  let module_path =
    let open Ast_typed.Misc in
    (* if `module_path` is empty, `typed_prg` isn't a contract and `typed_prg`
      contains only one contract module: let `module_path` become the path
      to that single contract module *)
    if List.is_empty module_path && Option.is_none (get_contract_opt typed_prg.pr_sig)
    then (
      match get_all_contracts typed_prg with
      | [ (single_contract_module, _) ] -> [ single_contract_module ]
      | _ -> module_path)
    else module_path
  in
  let _sig, contract_sig =
    let sig_ = Ast_typed.to_extended_signature typed_prg in
    Trace.trace_option
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
      Trace.trace ~raise self_mini_c_tracer @@ Self_mini_c.all_expression options mini_c
    in
    let mini_c_tys =
      Trace.trace_option
        ~raise
        (`Self_mini_c_tracer
          (Self_mini_c.Errors.corner_case "Error reconstructing type of views"))
      @@ Mini_c.get_t_tuple mini_c.type_expression
    in
    let nb_of_views = List.length view_names in
    let aux i view =
      let idx_ty =
        Trace.trace_option
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
