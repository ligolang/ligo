open Simple_utils.Trace

(* Helpers *)

module ModResHelpers = Preprocessor.ModRes.Helpers

let get_declarations_core core_prg =
  (* Note: This hack is needed because when some file is `#import`ed the `module_binder` is
     the absolute file path, and the REPL prints an absolute file path which is confusing
     So we ignore the module declarations which which have their module_binder as some absolute path.
     The imported module name will still be printed by the REPL as it is added as a module alias.
     Reference: https://gitlab.com/ligolang/ligo/-/blob/c8ae194e97341dc717549c9f50c743bcea855a33/vendors/BuildSystem/BuildSystem.ml#L113-121
  *)
  let ignore_module_variable_which_is_absolute_path module_variable =
    let module_variable = try Ast_core.ModuleVar.to_name_exn module_variable with _ -> "" in
    not @@ Caml.Sys.file_exists module_variable in

  let func_declarations = List.map ~f:(fun a -> `Value a)  @@ Ligo_compile.Of_core.list_declarations core_prg in
  let type_declarations = List.map ~f:(fun a -> `Type a)   @@ Ligo_compile.Of_core.list_type_declarations core_prg in
  let mod_declarations  = Ligo_compile.Of_core.list_mod_declarations core_prg in
  let mod_declarations  = List.map ~f:(fun a -> `Module a) @@ List.filter mod_declarations ~f:ignore_module_variable_which_is_absolute_path in
  func_declarations @ type_declarations @ mod_declarations

let get_declarations_typed (typed_prg : Ast_typed.program) =
  List.filter_map ~f:Ast_typed.(fun (a : declaration) -> Simple_utils.Location.unwrap a |>
    (function Declaration_constant a when not a.attr.hidden -> Option.return @@ `Value a.binder.var
    | Declaration_type a when not a.type_attr.hidden -> Option.return @@`Type a.type_binder
    | Declaration_module a when not a.module_attr.hidden -> Option.return @@ `Module a.module_binder
    | _ -> None)) @@ typed_prg

let pp_declaration ppf = function
    `Value a  -> Ast_core.PP.expression_variable ppf a
  | `Type a   -> Ast_core.PP.type_variable       ppf a
  | `Module a -> Ast_core.PP.module_variable     ppf a


(* REPL logic *)

type repl_result =
    Expression_value of Ast_core.expression
  | Defined_values_core of Ast_core.module_
  | Defined_values_typed of Ast_typed.module_
  | Just_ok

open Simple_utils.Display

let repl_result_ppformat ~display_format f = function
    Expression_value expr ->
     (match display_format with
      | Human_readable | Dev -> Ast_core.PP.expression f expr)
  | Defined_values_core module_ ->
     (match display_format with
      | Human_readable | Dev -> Simple_utils.PP_helpers.list_sep_d
                                  pp_declaration f
                                  (get_declarations_core module_))
  | Defined_values_typed module' ->
     (match display_format with
      | Human_readable | Dev -> Simple_utils.PP_helpers.list_sep_d
                                  pp_declaration f
                                  (get_declarations_typed module'))
  | Just_ok -> Simple_utils.PP_helpers.string f "Done."

let repl_result_jsonformat = function
    Expression_value expr ->
     let value = Format.asprintf "%a" Ast_core.PP.expression expr in
     `Assoc [("value", `String value)]
  | Defined_values_core module_ ->
     let func_declarations  = Ligo_compile.Of_core.list_declarations module_ in
     let type_declarations  = Ligo_compile.Of_core.list_type_declarations module_ in
     let func_defs = List.map ~f:(fun n -> `Assoc [("name", Ast_core.ValueVar.to_yojson n)]) func_declarations in
     let type_defs = List.map ~f:(fun n -> `Assoc [("name", Ast_core.TypeVar.to_yojson n)]) type_declarations in
     `Assoc [("definitions", `List (func_defs @ type_defs))]
  | Defined_values_typed module' ->
     let func_declarations  = Ligo_compile.Of_typed.list_declarations module' in
     let type_declarations  = Ligo_compile.Of_typed.list_type_declarations module' in
     let func_defs = List.map ~f:(fun n -> `Assoc [("name", Ast_core.ValueVar.to_yojson n)]) func_declarations in
     let type_defs = List.map ~f:(fun n -> `Assoc [("name", Ast_core.TypeVar.to_yojson n)]) type_declarations in
     `Assoc [("definitions", `List (func_defs @ type_defs))]
  | Just_ok -> `Assoc []

let repl_result_format : 'a format = {
    pp = repl_result_ppformat ;
    to_json = repl_result_jsonformat ;
}

module Run = Ligo_run.Of_michelson
module Raw_options = Compiler_options.Raw_options

type state = { env : Environment.t; (* The repl should have its own notion of environment *)
               syntax : Syntax_types.t;
               protocol : Environment.Protocols.t;
               top_level : Ast_typed.program;
               dry_run_opts : Run.options;
               module_resolutions : Preprocessor.ModRes.t option;
              }

let try_eval ~raise ~raw_options state s =
  let options = Compiler_options.make ~raw_options ~syntax:state.syntax () in
  let options = Compiler_options.set_init_env options state.env in
  let typed_exp  = Ligo_compile.Utils.type_expression_string ~raise ~options:options state.syntax s @@ Environment.to_program state.env in
  let module_ = Ligo_compile.Of_typed.compile_program ~raise state.top_level in
  let aggregated_exp = Ligo_compile.Of_typed.compile_expression_in_context ~raise ~options:options.middle_end typed_exp module_ in
  let mini_c = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_exp in
  let compiled_exp = Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c in
  let options = state.dry_run_opts in
  let runres = Run.run_expression ~raise ~options:options compiled_exp.expr compiled_exp.expr_ty in
  let x = Decompile.Of_michelson.decompile_expression ~raise aggregated_exp.type_expression runres in
  match x with
  | Success expr ->
     let state = { state with top_level = state.top_level } in
     (state, Expression_value expr)
  | Fail _ ->
    raise.error `Repl_unexpected

let concat_modules ~declaration (m1 : Ast_typed.program) (m2 : Ast_typed.program) : Ast_typed.program =
  let () = if declaration then assert (List.length m2 = 1) in
  (m1 @ m2)

let try_declaration ~raise ~raw_options state s =
  let options = Compiler_options.make ~raw_options ~syntax:state.syntax () in
  let options = Compiler_options.set_init_env options state.env in
  try
    try_with (fun ~raise ~catch:_ ->
      let typed_prg,core_prg =
        Ligo_compile.Utils.type_program_string ~raise ~options:options state.syntax s in
      let env = Environment.append typed_prg state.env in
      let state = { state with env ; top_level = concat_modules ~declaration:true state.top_level typed_prg } in
      (state, Defined_values_core core_prg))
    (fun ~catch:_ -> function
        (`Parser_tracer _ : Main_errors.all)
      | (`Cit_jsligo_tracer _ : Main_errors.all)
      | (`Cit_pascaligo_tracer _ : Main_errors.all)
      | (`Cit_cameligo_tracer _ : Main_errors.all)
      | (`Cit_reasonligo_tracer _ : Main_errors.all) ->
         try_eval ~raise ~raw_options state s
      | e -> raise.error e)
  with
  | Failure _ ->
     raise.error `Repl_unexpected

let import_file ~raise ~raw_options state file_name module_name =
  let file_name = ModResHelpers.resolve_file_name file_name state.module_resolutions in
  let options = Compiler_options.make ~raw_options ~syntax:state.syntax ~protocol_version:state.protocol () in
  let options = Compiler_options.set_init_env options state.env in
  let module_ =
    let prg = Build.merge_and_type_libraries ~raise ~options file_name in
    Simple_utils.Location.wrap (Ast_typed.M_struct prg)
  in
  let module_ = Ast_typed.([Simple_utils.Location.wrap @@ Declaration_module {module_binder=Ast_typed.ModuleVar.of_input_var module_name;module_;module_attr={public=true;hidden=false}}]) in
  let env     = Environment.append module_ state.env in
  let state = { state with env = env; top_level = concat_modules ~declaration:true state.top_level module_ } in
  (state, Just_ok)

let use_file ~raise ~raw_options state file_name =
  let file_name = ModResHelpers.resolve_file_name file_name state.module_resolutions in
  let options = Compiler_options.make ~raw_options ~syntax:state.syntax ~protocol_version:state.protocol () in
  let options = Compiler_options.set_init_env options state.env in
  (* Missing typer environment? *)
  let module' = Build.merge_and_type_libraries ~raise ~options file_name in
  let env = Environment.append module' state.env in
  let state = { state with env = env;
                            top_level = concat_modules ~declaration:false state.top_level module'
                          } in
  (state, Defined_values_typed module')

(* REPL "parsing" *)

type repl_directive = Use of string
                    | Import of string * string
                    | Expr of string

let parse s =
  let whitespace = "[ \n\r\x0c\t]" in
  let re_use = "^" ^ (whitespace ^ "*") ^ "#use" ^ (whitespace ^ "+") ^ "\"\\(.*\\)\"" ^ (whitespace ^ "*") ^ "$" in
  let re_import = "^" ^ (whitespace ^ "*") ^ "#import" ^ (whitespace ^ "+") ^ "\"\\(.*\\)\"" ^ (whitespace ^ "+") ^ "\"\\(.*\\)\"" ^ (whitespace ^ "*") ^ "$" in
  if Str.(string_match (regexp re_use) s 0) then
    Use (Str.matched_group 1 s)
  else if Str.(string_match (regexp re_import) s 0)  then
    Import (Str.matched_group 1 s, Str.matched_group 2 s)
  else
    Expr s

(* REPL main and loop *)

let eval display_format state c =
  let (Ex_display_format t) = display_format in
  match to_stdlib_result c with
    Ok ((state, out),_w) ->
      let disp = (Displayable {value = out; format = repl_result_format }) in
      let out : string =
        match t with
        | Human_readable -> convert ~display_format:t disp ;
        | Dev -> convert ~display_format:t disp ;
        | Json -> Yojson.Safe.pretty_to_string @@ convert ~display_format:t disp in
      (1, state, out)
  | Error (e,_w) ->
      let disp = (Displayable {value = e; format = Main_errors.Formatter.error_format }) in
      let out : string =
        match t with
        | Human_readable -> convert ~display_format:t disp ;
        | Dev -> convert ~display_format:t disp ;
        | Json -> Yojson.Safe.pretty_to_string @@ convert ~display_format:t disp in
      (0, state, out)

let parse_and_eval ~raw_options display_format state s =
  let c = match parse s with
    | Use s -> use_file ~raw_options state s
    | Import (fn, mn) -> import_file state ~raw_options fn mn
    | Expr s -> try_declaration ~raw_options state s in
  eval display_format state c

let welcome_msg = "Welcome to LIGO's interpreter!
Included directives:
  #use \"file_path\";;
  #import \"file_path\" \"module_name\";;"

let make_initial_state syntax protocol dry_run_opts project_root options =
  let top_level = Build.Stdlib.typed ~options syntax in
  let env = Environment.append top_level @@ Environment.default protocol in
  {
    env;
    top_level;
    syntax = syntax;
    protocol = protocol;
    dry_run_opts = dry_run_opts;
    module_resolutions = Option.bind project_root ~f:Preprocessor.ModRes.make;
  }

let rec read_input prompt delim =
  let open Simple_utils.Option in
  match LNoise.linenoise prompt with
  | exception Sys.Break | None -> None
  | Some s -> LNoise.history_add s |> ignore;
              let result = Str.split_delim (Str.regexp delim) s in
              match result with
              | [] | [_] ->
                 let* i = read_input "" delim in
                 some @@ s ^ "\n" ^ i
              | hd :: _ -> some @@ hd

let rec loop ~raw_options display_format state n =
  let prompt = Format.sprintf "In  [%d]: " n in
  let s = read_input prompt ";;" in
  match s with
  | Some s ->
     let k, state, out = parse_and_eval ~raw_options display_format state s in
     let out = Format.sprintf "Out [%d]: %s" n out in
     print_endline out;
     loop ~raw_options display_format state (n + k)
  | None -> ()

let main (raw_options : Raw_options.t) display_format now amount balance sender source init_file () =
  let protocol = Environment.Protocols.protocols_to_variant raw_options.protocol_version in
  let syntax = Syntax.of_string_opt (Syntax_name raw_options.syntax) None in
  let dry_run_opts = Ligo_run.Of_michelson.make_dry_run_options {now ; amount ; balance ; sender ; source ; parameter_ty = None } in
  match protocol, Simple_utils.Trace.to_option syntax, Simple_utils.Trace.to_option dry_run_opts with
  | _, None, _ -> Error ("Please check syntax name.", "")
  | None, _, _ -> Error ("Please check protocol name.", "")
  | _, _, None -> Error ("Please check run options.", "")
  | Some protocol, Some syntax, Some dry_run_opts ->
    begin
      print_endline welcome_msg;
      let options = Compiler_options.make ~raw_options ~syntax () in
      let state = make_initial_state syntax protocol dry_run_opts raw_options.project_root options in
      let state = match init_file with
        | None -> state
        | Some file_name -> let c = use_file state ~raw_options file_name in
                            let _, state, _ = eval (Ex_display_format Dev) state c in
                            state in
      LNoise.set_multiline true;
      loop ~raw_options display_format state 1;
      Ok("","")
    end
