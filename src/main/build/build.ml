module Trace = Simple_utils.Trace
module Http_uri = Simple_utils.Http_uri
open Main_errors
open Ligo_prim
module Stdlib = Stdlib
module Source_input = BuildSystem.Source_input
module Ligo_dep_cameligo = Ligo_dep_cameligo
module Ligo_dep_jsligo = Ligo_dep_jsligo

let loc = Location.env

module type Params = sig
  val raise : (all, Main_warnings.all) Trace.raise
  val options : Compiler_options.t
  val top_level_syntax : Syntax_types.t
end

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


let syntax_of_code_input code_input =
  let file_name = Source_input.id_of_code_input code_input in
  Syntax.of_string_opt (Syntax_name "auto") @@ Some file_name


(** Actually performs preprocessing *)
let preprocess_code_input ~raise ~meta ~options code_input =
  match code_input with
  | Source_input.HTTP uri ->
    let code = Http_uri.fetch uri in
    Ligo_compile.Helpers.preprocess_string
      ~raise
      ~meta
      ~options:options.Compiler_options.frontend
      code
  | From_file file_name ->
    Ligo_compile.Helpers.preprocess_file
      ~raise
      ~meta
      ~options:options.Compiler_options.frontend
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


include BuildSystem.T

module M (Params : Params) = struct
  let raise = Params.raise
  let options = Params.options

  type meta_data = Ligo_compile.Helpers.meta

  module C_unit = struct
    (** Initially compilation is driven up to Ast_core in order
        to be able to extract its dependencies *)
    type t = Ast_core.program

    type meta =
      { code_input : code_input
      ; location : Location.t
      ; module_name : module_name
      ; meta : meta_data
      ; imports : imports
      }
  end

  (** Returns actual filepaths contents of which will included into resulting ast *)
  let extract_deps ~syntax ~file_name c_unit =
    match syntax with
    | Syntax_types.CameLIGO ->
      let std_lib =
        (* We need stdlib for [Build.Stdlib.get],
          because if [no_stdlib] we get [Build.Stdlib.empty] *)
        let options = Compiler_options.set_no_stdlib options false in
        Stdlib.get ~options |> fun x -> x.Stdlib.content_typed.pr_module
      in
      Ligo_dep_cameligo.imports_of_deps ~options file_name
      @@ Ligo_dep_cameligo.dependencies ~std_lib c_unit
    | JsLIGO ->
      Ligo_dep_jsligo.imports_of_deps ~options file_name
      @@ Ligo_dep_jsligo.dependencies c_unit


  (** Compiles preprocessed input into Ast_core *)
  let compile_to_core ~raise ~options ~meta file_name c_unit =
    let Ligo_compile.Helpers.{ syntax } = meta in
    let options = Compiler_options.set_syntax options (Some syntax) in
    let c_unit =
      c_unit
      |> Fn.flip (Ligo_compile.Utils.to_core ~raise ~options ~meta) file_name
      |> Helpers.inject_declaration ~options ~raise syntax
    in
    let deps = extract_deps ~syntax ~file_name c_unit in
    let dirname = Filename.dirname file_name in
    let c_unit =
      match syntax with
      | JsLIGO ->
        Helpers.process_imports
          ~f:(fun file_name ->
            let file_name =
              match Filename.split_extension file_name with
              | name, None -> name ^ ".jsligo"
              | name, _ -> file_name
            in
            Helpers.normalize_path @@ Filename.concat dirname file_name)
          c_unit
      | CameLIGO -> Helpers.add_module_aliases c_unit deps
    in
    let deps = List.map deps ~f:Tuple2.get1 in
    c_unit, deps


  (** Performs preprocessing and reports error in case of failure *)
  let preprocess_import ~raise ~meta ~options import =
    let c_unit, deps =
      Trace.map_error
        ~raise
        (preprocess_code_input ~meta ~options import.code_input)
        ~f:(fun err1 ->
          let info =
            match err1 with
            | `Preproc_tracer (`Preprocessing_generic e) -> e
          in
          if not
             @@ String.is_substring
                  ~substring:"No such file or directory"
                  info.Simple_utils.Region.value
          then (`Preproc_tracer (`Preprocessing_generic info) : Main_errors.all)
          else (
            let loc = import.location in
            let module_ = Module_var.of_input_var ~loc import.module_name in
            let error : Checking.Errors.typer_error =
              Checking.Errors.unbound_module_variable module_ loc
            in
            (`Checking_tracer error : Main_errors.all)))
    in
    c_unit, deps


  let preprocess : import -> C_unit.t * meta_data * imports =
   fun ({ code_input; module_name; _ } as import) ->
    let syntax = syntax_of_code_input ~raise code_input in
    let meta = Ligo_compile.Of_source.extract_meta syntax in
    let c_unit, _ = preprocess_import ~raise ~meta ~options import in
    let file_name = Source_input.id_of_code_input code_input in
    let c_unit, imports = compile_to_core ~raise ~options ~meta file_name c_unit in
    c_unit, meta, imports
end

(** Compiles program and all its deps into Ast_core and aggregates it into single Ast_core.program *)
module Ast_core_target (Params : Params) = struct
  include M (Params)

  module AST = struct
    type t = Ast_core.program

    let link t1 t2 = t1 @ t2

    type interface = unit list

    let make_module_in_ast : t -> module_name * interface * t -> t =
     fun ast (module_binder, _, module_ast) ->
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
  end

  module Environment = struct
    type t = unit

    let init_env : t = ()
    let add_module : t -> C_unit.meta -> AST.interface -> t = fun _ _ _ -> ()
    let add_interface : t -> AST.interface -> t = fun _ _ -> ()
    let find_interface : t -> module_name -> AST.interface = fun () _ -> []
  end

  let std_lib : unit -> AST.t * AST.interface =
   fun () ->
    let std_lib = Stdlib.get ~options in
    std_lib.content_core, []


  let compile : C_unit.t -> C_unit.meta -> Environment.t -> AST.t * AST.interface =
   fun c_unit _ _ -> c_unit, []


  let postprocess : AST.t -> intfs:Environment.t -> AST.t =
   fun prg ~intfs ->
    let module_ =
      let rec f decl =
        let loc = decl.Location.location in
        match Location.unwrap decl with
        | Ast_core.D_import
            (Import_all_as
              { alias = import_name; module_str; import_attr; original_module_str = _ })
          ->
          let imported_module = Module_var.of_input_var ~loc module_str in
          (* Create module alias for imported module *)
          [ Location.wrap ~loc
            @@ Ast_core.D_module
                 { module_binder = import_name
                 ; module_ = Location.wrap ~loc @@ Module_expr.M_variable imported_module
                 ; module_attr = Type_or_module_attr.default_attributes
                 ; annotation = None
                 }
          ]
        | D_import
            (Import_selected
              { imported; module_str; import_attr; original_module_str = _ }) ->
          let imported_module = Module_var.of_input_var ~loc module_str in
          (* makes `let x = External_module_name.x` entry *)
          let make_value var =
            let binder = Binder.make var None in
            let expr =
              Ast_core.
                { expression_content =
                    Ast_core.E_module_accessor
                      { module_path = [ imported_module ]; element = var }
                ; location = Location.generated
                }
            in
            let attr =
              { Value_attr.default_attributes with public = import_attr.public }
            in
            Location.wrap ~loc @@ Ast_core.(D_value Value_decl.{ binder; expr; attr })
          in
          List.map imported ~f:make_value
        | D_module ({ module_; _ } as decl) ->
          let module_ =
            let loc = module_.location in
            let module_ = Location.unwrap module_ in
            match module_ with
            | M_struct module_ast ->
              let module_ast = List.concat_map module_ast ~f in
              Location.wrap ~loc @@ Module_expr.M_struct module_ast
            | _ -> Location.wrap ~loc module_
          in
          [ Location.wrap ~loc @@ Ast_core.D_module { decl with module_ } ]
        (* At this point all Import_rename decls must be replaced with the D_module ones *)
        | D_import _
        | D_value _
        | D_irrefutable_match _
        | D_type _
        | D_module_include _
        | D_signature _ -> [ decl ]
      in
      List.concat_map prg ~f
    in
    module_
end

module Cmi = Checking.Cmi

(** Compiles program and all its dependencies into Ast_typed and aggregates them into single Ast_typed.program *)
module Ast_typed_target (Params : Params) = struct
  include M (Params)

  let std_lib : unit -> Ast_typed.module_ * Ast_typed.signature =
   fun () ->
    let std_lib = Stdlib.get ~options in
    let typed = std_lib.content_typed in
    typed.pr_module, typed.pr_sig


  module AST = struct
    type t = Ast_typed.module_
    type interface = Ast_typed.signature

    let link m1 m2 = m1 @ m2

    let make_module_in_ast : t -> module_name * interface * t -> t =
     fun ast (module_binder, sig_, module_ast) ->
      let module_binder = Module_var.of_input_var ~loc module_binder in
      let new_decl =
        Location.wrap
          ~loc
          Ast_typed.(
            D_module
              { module_binder
              ; module_ =
                  { module_content = Module_expr.M_struct module_ast
                  ; signature = sig_
                  ; module_location = loc
                  }
              ; module_attr =
                  { Type_or_module_attr.default_attributes with hidden = true }
              ; annotation = ()
              })
      in
      new_decl :: ast
  end

  module Environment = struct
    type t = Checking.Persistent_env.t

    let init_env : t = Checking.Persistent_env.empty

    let add_module env C_unit.{ code_input; module_name = module_binder; imports; _ } sig_
      =
      let path = Source_input.id_of_code_input code_input in
      let module_binder = Module_var.of_input_var ~loc module_binder in
      let imports =
        List.map
          ~f:(fun { code_input; _ } -> Source_input.id_of_code_input code_input)
          imports
      in
      Checking.Persistent_env.add_signature env module_binder path imports sig_


    let add_interface : t -> AST.interface -> t =
     fun env intf -> Checking.Persistent_env.add_virtual env intf


    let find_interface : t -> module_name -> AST.interface =
     fun env module_binder ->
      let module_binder = Module_var.of_input_var ~loc module_binder in
      Checking.Persistent_env.(find_cmi env (Module module_binder))
      |> Tuple2.get1
      |> fun x -> x.Cmi.sign
  end

  let compile : C_unit.t -> C_unit.meta -> Environment.t -> AST.t * AST.interface =
   fun c_unit { meta = { syntax } as meta; imports; _ } env ->
    let Ligo_compile.Helpers.{ syntax } = meta in
    let options = Compiler_options.set_syntax options (Some syntax) in
    let prg =
      Ligo_compile.Of_core.typecheck_with_signature ~raise ~options ~context:env c_unit
    in
    prg.pr_module, prg.pr_sig


  let postprocess : AST.t -> intfs:Environment.t -> AST.t =
   fun prg ~intfs ->
    let module_ =
      let rec f decl =
        let loc = decl.Location.location in
        match Location.unwrap decl with
        | Ast_typed.D_import
            (Import_all_as
              { alias = import_name; module_str; import_attr; original_module_str = _ })
          ->
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
        | D_import
            (Import_selected
              { imported; module_str; import_attr; original_module_str = _ }) ->
          let imported_module = Module_var.of_input_var ~loc module_str in
          let Ast_typed.{ sig_items = intf; _ } =
            Checking.Persistent_env.find_signature intfs imported_module
          in
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
        | D_import _
        | D_value _
        | D_irrefutable_match _
        | D_type _
        | D_module_include _
        | D_signature _ -> [ decl ]
      in
      List.concat_map prg ~f
    in
    module_
end

module Build_typed (Params : Params) = BuildSystem.Make (Ast_typed_target (Params))
module Build_core (Params : Params) = BuildSystem.Make (Ast_core_target (Params))

type graph = Graph__Persistent.Digraph.Concrete(BuildSystem__Types.Node).t

let dependency_graph ~raise
    : options:Compiler_options.t -> Source_input.code_input -> graph
  =
 fun ~options code_input ->
  let open Build_core (struct
    let raise = raise
    let options = options

    let top_level_syntax =
      get_top_level_syntax
        ~options
        ~filename:(Source_input.id_of_code_input code_input)
        ()
  end) in
  fst @@ dependency_graph code_input


let module_deps ~raise
    : options:Compiler_options.t -> Source_input.code_input -> string String.Map.t
  =
 fun ~options code_input ->
  let filename = Source_input.id_of_code_input code_input in
  let module B =
    Build_core (struct
      let raise = raise
      let options = options
      let top_level_syntax = get_top_level_syntax ~options ~filename ()
    end)
  in
  let open B in
  (* Getting topsorted deps *)
  let sorted =
    Simple_utils.Ne_list.to_list
    @@ Trace.trace ~raise build_error_tracer
    @@ Trace.from_result
    @@ Fn.flip solve_graph filename
    @@ dependency_graph code_input
  in
  (* Dropping file itself, since we need only deps *)
  List.drop_last sorted
  |> Option.value ~default:[]
  (* Accumulation everything into map *)
  |> List.fold_left ~init:String.Map.empty ~f:(fun acc (file_name, vertex) ->
         let module_name = module_name_of_vertex vertex in
         match Map.add acc ~key:module_name ~data:file_name with
         | `Duplicate -> acc
         | `Ok added -> added)


(* unqualified usages : list-declaration ; print *)
let unqualified_core ~raise
    : options:Compiler_options.t -> Source_input.file_name -> Ast_core.program
  =
 fun ~options filename ->
  let open Build_core (struct
    let raise = raise
    let options = options
    let top_level_syntax = get_top_level_syntax ~options ~filename ()
  end) in
  Trace.trace ~raise build_error_tracer
  @@ Trace.from_result (build_unqualified (Source_input.From_file filename))


let qualified_core ~raise
    : options:Compiler_options.t -> Source_input.code_input -> Ast_core.program
  =
 fun ~options source ->
  let open Build_core (struct
    let raise = raise
    let options = options

    let top_level_syntax =
      match source with
      | HTTP uri -> get_top_level_syntax ~options ~filename:(Http_uri.get_filename uri) ()
      | Raw_input_lsp _ -> Syntax_types.CameLIGO
      | From_file filename -> get_top_level_syntax ~options ~filename ()
      | Raw _ -> Syntax_types.CameLIGO
  end) in
  let ast, _ =
    Trace.trace ~raise build_error_tracer @@ Trace.from_result (build_qualified source)
  in
  ast


let qualified_core_from_string ~raise
    : options:Compiler_options.t -> Source_input.raw_input -> Ast_core.program
  =
 fun ~options input ->
  let open Build_core (struct
    let raise = raise
    let options = options
    let top_level_syntax = get_top_level_syntax ~options ~filename:input.id ()
  end) in
  let ast, _ =
    Trace.trace ~raise build_error_tracer
    @@ Trace.from_result (build_qualified (Source_input.Raw input))
  in
  ast


let qualified_core_from_raw_input ~raise
    : options:Compiler_options.t -> string -> string -> Ast_core.program
  =
 fun ~options file code ->
  let open Build_core (struct
    let raise = raise
    let options = options
    let top_level_syntax = get_top_level_syntax ~options ~filename:file ()
  end) in
  let ast, _ =
    Trace.trace ~raise build_error_tracer
    @@ Trace.from_result (build_qualified (Source_input.Raw_input_lsp { file; code }))
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
    let top_level_syntax = top_level_syntax_of_code_input ~options source
  end) in
  let prg, env =
    Trace.trace ~raise build_error_tracer @@ Trace.from_result (build_qualified source)
  in
  let file_name = Source_input.id_of_code_input source in
  let intf, _ = Checking.Persistent_env.(find_cmi env (File file_name)) in
  Ast_typed.{ pr_module = prg; pr_sig = intf.sign }, env


let qualified_typed ~raise
    : options:Compiler_options.t -> Source_input.code_input -> Ast_typed.program
  =
 fun ~options source -> qualified_typed_with_env ~raise ~options source |> Tuple2.get1


let qualified_typed_str ~raise : options:Compiler_options.t -> string -> Ast_typed.program
  =
 fun ~options code ->
  let open Build_core (struct
    (* FIXME notice here idially should use Build_typed *)
    let raise = raise
    let options = options
    let top_level_syntax = get_top_level_syntax ~options ()
  end) in
  let id =
    match options.frontend.syntax with
    | Some s -> "from_build" ^ Syntax.to_ext s
    | None -> "from_build"
  in
  let s = Source_input.Raw { code; id } in
  let ast, _ =
    Trace.trace ~raise build_error_tracer @@ Trace.from_result (build_qualified s)
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
