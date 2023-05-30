module Location = Simple_utils.Location
open Simple_utils.Trace
open Ligo_prim

(* Helpers *)

module ModRes = Preprocessor.ModRes

let loc = Location.repl

let get_declarations_core (core_prg : Ast_core.program) =
  (* Note: This hack is needed because when some file is `#import`ed the `module_binder` is
     the absolute file path, and the REPL prints an absolute file path which is confusing
     So we ignore the module declarations which which have their module_binder as some absolute path.
     The imported module name will still be printed by the REPL as it is added as a module alias.
     Reference: https://gitlab.com/ligolang/ligo/-/blob/c8ae194e97341dc717549c9f50c743bcea855a33/vendored-dune/BuildSystem/BuildSystem.ml#L113-121
  *)
  let ignore_module_variable_which_is_absolute_path module_variable =
    let module_variable =
      try Module_var.to_name_exn module_variable with
      | _ -> ""
    in
    not @@ Caml.Sys.file_exists module_variable
  in
  let func_declarations =
    List.map ~f:(fun a -> `Value a) @@ Ligo_compile.Of_core.list_declarations core_prg
  in
  let lhs_pattern_declarations =
    List.map ~f:(fun a -> `Value a)
    @@ Ligo_compile.Of_core.list_lhs_pattern_declarations core_prg
  in
  let type_declarations =
    List.map ~f:(fun a -> `Type a) @@ Ligo_compile.Of_core.list_type_declarations core_prg
  in
  let mod_declarations = Ligo_compile.Of_core.list_mod_declarations core_prg in
  let mod_declarations =
    List.map ~f:(fun a -> `Module a)
    @@ List.filter mod_declarations ~f:ignore_module_variable_which_is_absolute_path
  in
  func_declarations @ lhs_pattern_declarations @ type_declarations @ mod_declarations


let get_declarations_typed (typed_prg : Ast_typed.program) =
  List.concat
  @@ List.filter_map
       ~f:
         Ast_typed.(
           fun (a : declaration) ->
             Location.unwrap a
             |> function
             | D_value a when not a.attr.hidden ->
               Option.return @@ [ `Value (Binder.get_var a.binder) ]
             | D_irrefutable_match a when not a.attr.hidden ->
               let binders = Pattern.binders a.pattern in
               let values = List.map binders ~f:(fun b -> `Value (Binder.get_var b)) in
               Option.return @@ values
             | D_type a when not a.type_attr.hidden ->
               Option.return @@ [ `Type a.type_binder ]
             | D_module a when not a.module_attr.hidden ->
               Option.return @@ [ `Module a.module_binder ]
             | D_value _ | D_irrefutable_match _ | D_type _ | D_module _ -> None)
  @@ typed_prg


let pp_declaration ppf = function
  | `Value a -> Value_var.pp ppf a
  | `Type a -> Type_var.pp ppf a
  | `Module a -> Module_var.pp ppf a


(* REPL logic *)

type repl_result =
  | Expression_value of Ast_core.expression
  | Defined_values_core of Ast_core.program
  | Defined_values_typed of Ast_typed.program
  | Just_ok

open Simple_utils.Display

let repl_result_ppformat ~display_format ~no_colour:_ f = function
  | Expression_value expr ->
    (match display_format with
    | Human_readable | Dev -> Ast_core.PP.expression f expr)
  | Defined_values_core program ->
    (match display_format with
    | Human_readable | Dev ->
      Simple_utils.PP_helpers.list_sep_d pp_declaration f (get_declarations_core program))
  | Defined_values_typed program ->
    (match display_format with
    | Human_readable | Dev ->
      Simple_utils.PP_helpers.list_sep_d pp_declaration f (get_declarations_typed program))
  | Just_ok -> Simple_utils.PP_helpers.string f "Done."


let repl_result_jsonformat = function
  | Expression_value expr ->
    let value = Format.asprintf "%a" Ast_core.PP.expression expr in
    `Assoc [ "value", `String value ]
  | Defined_values_core module_ ->
    let func_declarations = Ligo_compile.Of_core.list_declarations module_ in
    let type_declarations = Ligo_compile.Of_core.list_type_declarations module_ in
    let func_defs =
      List.map ~f:(fun n -> `Assoc [ "name", Value_var.to_yojson n ]) func_declarations
    in
    let type_defs =
      List.map ~f:(fun n -> `Assoc [ "name", Type_var.to_yojson n ]) type_declarations
    in
    `Assoc [ "definitions", `List (func_defs @ type_defs) ]
  | Defined_values_typed module' ->
    let func_declarations = Ligo_compile.Of_typed.list_declarations false module' in
    let type_declarations = Ligo_compile.Of_typed.list_type_declarations module' in
    let func_defs =
      List.map ~f:(fun n -> `Assoc [ "name", Value_var.to_yojson n ]) func_declarations
    in
    let type_defs =
      List.map ~f:(fun n -> `Assoc [ "name", Type_var.to_yojson n ]) type_declarations
    in
    `Assoc [ "definitions", `List (func_defs @ type_defs) ]
  | Just_ok -> `Assoc []


let repl_result_format : 'a format =
  { pp = repl_result_ppformat; to_json = repl_result_jsonformat }


module Run = Ligo_run.Of_michelson
module Raw_options = Compiler_options.Raw_options

type state =
  { syntax : Syntax_types.t
  ; protocol : Environment.Protocols.t
  ; top_level : Ast_typed.program
  ; dry_run_opts : Run.options
  ; module_resolutions : Preprocessor.ModRes.t option
  }

let try_eval ~raise ~raw_options state s =
  let options = Compiler_options.make ~raw_options ~syntax:state.syntax () in
  let typed_exp =
    Ligo_compile.Utils.type_expression_string ~raise ~options state.syntax s
    @@ Ast_typed.Misc.to_signature state.top_level
  in
  let aggregated_exp =
    Ligo_compile.Of_typed.compile_expression_in_context
      ~raise
      ~options:options.middle_end
      state.top_level
      typed_exp
  in
  let expanded_exp =
    Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_exp
  in
  let mini_c = Ligo_compile.Of_expanded.compile_expression ~raise expanded_exp in
  let compiled_exp = Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c in
  let options = state.dry_run_opts in
  let runres =
    Run.run_expression ~raise ~options compiled_exp.expr compiled_exp.expr_ty
  in
  let x =
    Decompile.Of_michelson.decompile_expression
      ~raise
      aggregated_exp.type_expression
      runres
  in
  match x with
  | Success expr ->
    let state = { state with top_level = state.top_level } in
    state, Expression_value expr
  | Fail _ -> raise.error `Repl_unexpected


let concat_modules ~declaration (m1 : Ast_typed.program) (m2 : Ast_typed.program)
    : Ast_typed.program
  =
  let () = if declaration then assert (List.length m2 = 1) in
  m1 @ m2


let try_declaration ~raise ~raw_options state s =
  let options = Compiler_options.make ~raw_options ~syntax:state.syntax () in
  try
    try_with
      (fun ~raise ~catch:_ ->
        let typed_prg, core_prg =
          Ligo_compile.Utils.type_program_string
            ~raise
            ~options
            ~context:(Ast_typed.Misc.to_signature state.top_level)
            state.syntax
            s
        in
        let state =
          { state with
            top_level = concat_modules ~declaration:true state.top_level typed_prg
          }
        in
        state, Defined_values_core core_prg)
      (fun ~catch:_ -> function
        | (`Parser_tracer _ : Main_errors.all)
        | (`Nanopasses_tracer (`Small_passes_unsupported_top_level_statement _) :
            Main_errors.all) -> try_eval ~raise ~raw_options state s
        | e -> raise.error e)
  with
  | Failure _ -> raise.error `Repl_unexpected


let import_file ~raise ~raw_options state file_name module_name =
  let file_name = ModRes.Helpers.resolve ~file:file_name state.module_resolutions in
  let options =
    Compiler_options.make
      ~raw_options
      ~syntax:state.syntax
      ~protocol_version:state.protocol
      ()
  in
  let module_ =
    let prg, signature =
      Build.qualified_typed_with_signature
        ~raise
        ~options
        (Build.Source_input.From_file file_name)
    in
    Ast_typed.
      { module_content = Module_expr.M_struct prg; module_location = loc; signature }
  in
  let module_ =
    Ast_typed.
      [ Location.wrap ~loc
        @@ D_module
             { module_binder = Module_var.of_input_var ~loc module_name
             ; module_
             ; module_attr = { public = true; hidden = false }
             ; annotation = ()
             }
      ]
  in
  let state =
    { state with top_level = concat_modules ~declaration:true state.top_level module_ }
  in
  state, Just_ok


let use_file ~raise ~raw_options state file_name =
  let file_name = ModRes.Helpers.resolve ~file:file_name state.module_resolutions in
  let options =
    Compiler_options.make
      ~raw_options
      ~syntax:state.syntax
      ~protocol_version:state.protocol
      ()
  in
  (* Missing typer environment? *)
  let module' =
    Build.qualified_typed ~raise ~options (Build.Source_input.From_file file_name)
  in
  let state =
    { state with top_level = concat_modules ~declaration:false state.top_level module' }
  in
  state, Defined_values_typed module'


(* REPL "parsing" *)

type repl_directive =
  | Use of string
  | Import of string * string
  | Expr of string

let parse s =
  let whitespace = "[ \n\r\x0c\t]" in
  let re_use =
    "^"
    ^ (whitespace ^ "*")
    ^ "#use"
    ^ (whitespace ^ "+")
    ^ "\"\\(.*\\)\""
    ^ (whitespace ^ "*")
    ^ "$"
  in
  let re_import =
    "^"
    ^ (whitespace ^ "*")
    ^ "#import"
    ^ (whitespace ^ "+")
    ^ "\"\\(.*\\)\""
    ^ (whitespace ^ "+")
    ^ "\"\\(.*\\)\""
    ^ (whitespace ^ "*")
    ^ "$"
  in
  if Str.(string_match (regexp re_use) s 0)
  then Use (Str.matched_group 1 s)
  else if Str.(string_match (regexp re_import) s 0)
  then Import (Str.matched_group 1 s, Str.matched_group 2 s)
  else Expr s


(* REPL main and loop *)

let eval display_format no_colour state c =
  let (Ex_display_format t) = display_format in
  match to_stdlib_result c with
  | Ok ((state, out), _w) ->
    let disp = Displayable { value = out; format = repl_result_format } in
    let out : string =
      match t with
      | Human_readable -> convert ~display_format:t ~no_colour disp
      | Dev -> convert ~display_format:t ~no_colour disp
      | Json -> Yojson.Safe.pretty_to_string @@ convert ~display_format:t ~no_colour disp
    in
    1, state, out
  | Error (e, _w) ->
    let disp = Displayable { value = e; format = Main_errors.Formatter.error_format } in
    let out : string =
      match t with
      | Human_readable -> convert ~display_format:t ~no_colour disp
      | Dev -> convert ~display_format:t ~no_colour disp
      | Json -> Yojson.Safe.pretty_to_string @@ convert ~display_format:t ~no_colour disp
    in
    0, state, out


let parse_and_eval ~raw_options display_format state s =
  let c =
    match parse s with
    | Use s -> use_file ~raw_options state s
    | Import (fn, mn) -> import_file state ~raw_options fn mn
    | Expr s -> try_declaration ~raw_options state s
  in
  let no_colour = raw_options.no_colour in
  eval display_format no_colour state c


let welcome_msg =
  "Welcome to LIGO's interpreter!\n\
   Included directives:\n\
  \  #use \"file_path\";;\n\
  \  #import \"file_path\" \"module_name\";;"


let make_initial_state syntax protocol dry_run_opts project_root options =
  let lib = Build.Stdlib.get ~options in
  let top_level = Build.Stdlib.select_lib_typed syntax lib in
  { top_level
  ; syntax
  ; protocol
  ; dry_run_opts
  ; module_resolutions = Option.bind project_root ~f:Preprocessor.ModRes.make
  }


let make_prompt n =
  let open LTerm_text in
  let prompt = Printf.sprintf "In  [%d]: " n in
  eval [ S prompt ]


let make_output n out =
  let open LTerm_text in
  let output = Printf.sprintf "Out [%d]: %s" n out in
  eval [ S output ]


class read_phrase ~term ~history ~n =
  object (self)
    inherit LTerm_read_line.read_line ~history:(LTerm_history.contents history) ()
    inherit [Zed_string.t] LTerm_read_line.term term as super_term
    method! show_box = false

    method! exec ?(keys = []) =
      function
      | action :: actions
        when Caml.(React.S.value self#mode = LTerm_read_line.Edition)
             && Caml.(action = LTerm_read_line.Accept) ->
        Zed_macro.add self#macro action;
        let input = Zed_rope.to_string (Zed_edit.text self#edit) in
        let input_utf8 = Zed_string.to_utf8 input in
        let result = Str.split_delim (Str.regexp ";;") input_utf8 in
        (match result with
        | [] | [ _ ] ->
          self#insert (Uchar.of_char '\n');
          self#exec ~keys actions
        | hd :: _ ->
          LTerm_history.add history input;
          Lwt.return @@ LTerm_read_line.Result (Zed_string.of_utf8 hd))
      | actions -> super_term#exec actions

    initializer self#set_prompt (React.S.const (make_prompt n))
  end

let rec loop ~raw_options syntax display_format term history state n =
  let rl = new read_phrase ~term ~history ~n in
  let command = Lwt_main.run rl#run in
  let command_utf8 = Zed_string.to_utf8 command in
  let k, state, out = parse_and_eval ~raw_options display_format state command_utf8 in
  Lwt_main.run (LTerm.fprintls term (make_output n out));
  loop ~raw_options syntax display_format term history state (n + k)


let main
    (raw_options : Raw_options.t)
    display_format
    no_colour
    now
    amount
    balance
    sender
    source
    init_file
    ()
  =
  let protocol =
    Environment.Protocols.protocols_to_variant raw_options.protocol_version
  in
  let syntax =
    Syntax.of_string_opt
      ~support_pascaligo:raw_options.deprecated
      (Syntax_name raw_options.syntax)
      None
  in
  let dry_run_opts =
    Ligo_run.Of_michelson.make_dry_run_options
      { now; amount; balance; sender; source; parameter_ty = None }
  in
  match
    ( protocol
    , Simple_utils.Trace.to_option syntax
    , Simple_utils.Trace.to_option dry_run_opts )
  with
  | _, None, _ -> Error ("Please check syntax name.", "")
  | None, _, _ -> Error ("Please check protocol name.", "")
  | _, _, None -> Error ("Please check run options.", "")
  | Some protocol, Some syntax, Some dry_run_opts ->
    Lwt_main.run (LTerm_inputrc.load ());
    let term = Lwt_main.run (Lazy.force LTerm.stdout) in
    let history = LTerm_history.create [] in
    let options = Compiler_options.make ~raw_options ~syntax () in
    let state =
      make_initial_state syntax protocol dry_run_opts raw_options.project_root options
    in
    let state =
      match init_file with
      | None -> state
      | Some file_name ->
        let c = use_file state ~raw_options file_name in
        let _, state, _ = eval (Ex_display_format Dev) no_colour state c in
        state
    in
    Lwt_main.run (LTerm.fprintls term (LTerm_text.eval [ S welcome_msg ]));
    (try loop ~raw_options syntax display_format term history state 1 with
    | LTerm_read_line.Interrupt -> Ok ("", "")
    | Caml.Sys.Break -> Ok ("", ""))
