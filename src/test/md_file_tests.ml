open Simple_utils.Trace
open Test_helpers
open Main_errors

let () = Ligo_unix.putenv ~key:"LIGO_FORCE_NEW_TYPER" ~data:"false"

type syntax = string
type group_name = string

type lang =
  | Meta   (* Meta = ligo test framework code *)
  | Object (* Object = normal LIGO code *)
  | Shell  (* Shell = compilation commands and other sh code *)

module SnippetsGroup = Caml.Map.Make (struct
  type t = syntax * group_name * Environment.Protocols.t

  let compare a b = Caml.compare a b
end)

type snippetsmap = (lang * string) SnippetsGroup.t

let arg_to_string x =
  match x with
  | Md.Field s -> s
  | Md.NameValue (k, v) -> Format.asprintf "%s=%s" k v


let get_proto p =
  let opt =
    try Environment.Protocols.protocols_to_variant p with
    | _ -> None
  in
  match opt with
  | Some x -> x
  | None -> failwith (Format.asprintf "unknown protocol %s" p)

let current_proto = get_proto "current"
let in_use_proto = Environment.Protocols.in_use

let execute_command command : int * string =
  Printf.printf "\nExecute %s" command;
  let ic = Ligo_unix.open_process_in ("set -e; exec 2>&1;\n" ^ command) in
  let all_output = Buffer.create 1024 in
  let exit_code =
    try
      while true do
        let line = In_channel.input_line ic in
        match line with
        | Some line ->
          begin
          Buffer.add_string all_output line;
          Buffer.add_char all_output '\n'
          end
        | None -> raise End_of_file
      done;
      0
    with
    | End_of_file ->
      let status = Ligo_unix.close_process_in ic in
      (match status with
       | Ligo_unix.WEXITED code | Ligo_unix.WSIGNALED code | Ligo_unix.WSTOPPED code -> code)
  in
  exit_code, Buffer.contents all_output

(*
  Binds the snippets by (syntax, group_name).
  Syntax and group_name being retrieved from the .md file header & arguments
  e.g. in the .md file:
    ```syntax group=group_name
      <some code>
    ```
*)
let get_groups md_file : snippetsmap =
  let channel = In_channel.create md_file in
  let lexbuf = Lexing.from_channel channel in
  let code_blocks = Md.token lexbuf in
  let aux : (int * snippetsmap) -> Md.block -> (int * snippetsmap) =
   fun (nb_shell_blocks, grp_map) el ->
    match el.header with
    | Some "shell" ->
      let () =
        (*sanity check*)
        List.iter
          ~f:(fun arg ->
            match arg with
            | Md.Field ""
            | Md.Field "skip"
            | Md.Field "run" -> ()
            (*| Md.NameValue ("group", _)
            | Md.Field "test-ligo"
            | Md.NameValue ("protocol", _)*)
            | Md.Field f ->
              failwith
                (Format.asprintf
                  "unknown Field '%s' in code block at line %d of file %s (%s)"
                  f
                  el.line
                  el.file
                  md_file)
            | Md.NameValue (k, v) ->
              failwith
                (Format.asprintf
                  "unknown argument '%s=%s' in code block at line %d of file %s (%s)"
                  k v
                  el.line
                  el.file
                  md_file))
          el.arguments
      in
      (
        match el.arguments with
        (* Run everything by default, some code below tests for GITLAB_CI to only actually run things on the CI *)
        | [ Md.Field "" ]
        | [ Md.Field "run" ] ->
          nb_shell_blocks + 1,
          SnippetsGroup.update
            ("shell", Format.sprintf "run_shell_%d" nb_shell_blocks, current_proto)
            (function
              (* TODO: should use String.concat only once at the top *)
              | Some (Shell, sh) -> Some (Shell, String.concat ~sep:"\n" (sh :: el.contents))
              | None             -> Some (Shell, String.concat ~sep:"\n" el.contents)
              | Some (_, sh) -> failwith "internal test error: group name shouldn't be 'run_shell_xxx'")
            grp_map
        | [ Md.Field "skip" ] | _ -> nb_shell_blocks, grp_map
      )
    | Some ("cameligo" as s) | Some ("jsligo" as s) ->
      nb_shell_blocks,
      let () =
        (*sanity check*)
        List.iter
          ~f:(fun arg ->
            match arg with
            | Md.Field ""
            | Md.Field "skip"
            | Md.NameValue ("group", _)
            | Md.Field "test-ligo"
            | Md.NameValue ("protocol", _) -> ()
            | Md.Field _ | Md.NameValue (_, _) ->
              failwith
                (Format.asprintf
                   "unknown argument '%s' in code block at line %d of file %s"
                   (arg_to_string arg)
                   el.line
                   el.file))
          el.arguments
      in
      (match el.arguments with
      | [ Md.Field "" ] ->
        SnippetsGroup.update
          (s, "ungrouped", current_proto)
          (fun arg_content ->
            match arg_content with
            | Some (lang, ct) -> Some (lang, String.concat ~sep:"\n" (ct :: el.contents))
            | None -> Some (Object, String.concat ~sep:"\n" el.contents))
          grp_map
      | [ Md.Field "skip" ] -> grp_map
      | [ Md.Field "test-ligo"; Md.NameValue ("group", name) ] ->
        let lang = Meta in
        SnippetsGroup.update
          (s, name, in_use_proto)
          (fun arg_content ->
            match arg_content with
            | Some (lang', ct) when Caml.( = ) lang lang' ->
              Some (lang, String.concat ~sep:"\n" (ct :: el.contents))
            | _ -> Some (lang, String.concat ~sep:"\n" el.contents))
          grp_map
      | [ Md.NameValue ("group", name); Md.NameValue ("protocol", x) ] ->
        let lang = Object in
        SnippetsGroup.update
          (s, name, get_proto x)
          (fun arg_content ->
            match arg_content with
            | Some (lang', ct) when Caml.( = ) lang lang' ->
              Some (lang, String.concat ~sep:"\n" (ct :: el.contents))
            | _ -> Some (lang, String.concat ~sep:"\n" el.contents))
          grp_map
      | [ Md.NameValue ("group", name) ] ->
        let lang = Object in
        SnippetsGroup.update
          (s, name, current_proto)
          (fun arg_content ->
            match arg_content with
            | Some (lang', ct) when Caml.( = ) lang lang' ->
              Some (lang, String.concat ~sep:"\n" (ct :: el.contents))
            | _ -> Some (lang, String.concat ~sep:"\n" el.contents))
          grp_map
      | args ->
        let () =
          List.iter
            ~f:(function
              | Md.NameValue (x, y) -> Format.printf "NamedValue %s %s\n" x y
              | Md.Field x -> Format.printf "%s\n" x)
            args
        in
        failwith "Block arguments (above) not supported")
    | None | Some _ -> nb_shell_blocks, grp_map
  in
  let ((_nb_shell_blocks : int), grp_map) = List.fold_left ~f:aux ~init:(0, SnippetsGroup.empty) code_blocks in
  grp_map

(** Write contents to filename if the file doesn't already have the same contents.
    This avoids floppy disk wear and spurious updates of timestamps,
    it also avoids unnecessary attempts to write when building a read-only copy. *)
let write_if_different (filename : string) ~(contents : string) : bool =
  let old_contents, same =
    try
      let old_contents = Caml.In_channel.with_open_bin filename Caml.In_channel.input_all in
      old_contents, String.equal old_contents contents
    with _ -> "<error>", false (* if there is any error while reading, assume we need to write. *)
  in
  if same then
    false
  else
    let () = Out_channel.write_all filename ~data:contents in
    true

let write_to_files ~raise md_filename grp_list : bool =
  let aux
    : (syntax * group_name * Environment.Protocols.t) * (lang * string) -> bool
    = fun ((syntax, grp, protocol_version), (lang, contents)) ->
      if String.equal syntax "shell" then
        false
      else
        let root_dir =
          if Caml.Sys.file_exists "../../dune-project" then "../.."
          else if Caml.Sys.file_exists "../../../dune-project" then "../../.."
          else failwith "Could not find root_dir, please fix src/test/md_file_tsts.ml"
        in
        let syntax = Syntax.of_string_opt ~raise (Syntax_name syntax) None in
        let dirname =
          if String.equal ".md" (String.sub md_filename ~pos:(String.length md_filename - 3) ~len:3)
          then
            let d, b = Filename.split (String.sub md_filename ~pos:0 ~len:(String.length md_filename - 3)) in
            Filename.of_parts [d; "src"; b]
          else failwith "Unexpected Markdown filename: does not end with .md" in
        let extension = match syntax with CameLIGO -> ".mligo" | JsLIGO -> ".jsligo" in
        let output_filename = Filename.of_parts [dirname; grp ^ extension] in

        (* Create directory both in _build and in the repository (otherwise the _build uses the old version) *)
        (*let () = Ligo_unix.mkdir_p ~perm:0o777 dirname in*)
        let () = Ligo_unix.mkdir_p ~perm:0o777 (Filename.of_parts [root_dir; dirname]) in

        (* Write file both in _build and in the repository (otherwise the _build uses the old version) *)
        (* let () = write_if_different output_filename ~contents *)
        write_if_different (Filename.of_parts [root_dir; output_filename]) ~contents
  in
  List.length (List.filter ~f:aux grp_list) > 0

(**
  if Meta : run the ligo test framework
  if Object : evaluate each expression in each programs from the snippets group map
  if Shell : run the shell commands
**)
let compile_groups ~raise filename grp_list =
  Lwt_main.run
  @@
  let open Lwt.Let_syntax in
  let aux
      : (syntax * group_name * Environment.Protocols.t) * (lang * string) -> unit Lwt.t
    =
   fun ((syntax, grp, protocol_version), (lang, contents)) ->
    trace ~raise (test_md_file filename syntax grp contents)
    @@ fun ~raise ->
    let options syntax =
      Compiler_options.make
        ~syntax:(Syntax.of_string_opt ~raise (Syntax_name syntax) None)
        ~raw_options:(Raw_options.make ())
        ~protocol_version
        ()
    in
    match lang with
    | Meta ->
      let options = options syntax in
      let options = Compiler_options.set_test_flag options true in
      let typed = Build.qualified_typed_str ~raise ~options contents in
      let%map (_ : bool * (group_name * Ligo_interpreter.Types.value) list) =
        Interpreter.eval_test ~options ~raise ~steps:5000 typed
      in
      ()
    | Object ->
      let options = options syntax in
      let typed = Build.qualified_typed_str ~raise ~options contents in
      let aggregated_with_unit =
        Ligo_compile.Of_typed.compile_expression_in_context
          ~raise
          ~options:options.middle_end
          None
          typed
          (Ast_typed.e_a_unit ~loc ())
      in
      let expanded =
        Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_with_unit
      in
      let mini_c = Ligo_compile.Of_expanded.compile_expression ~raise expanded in
      (* Format.printf "Mini_c AST: %a\n" (Mini_c.PP.expression) mini_c; *)
      let%map _michelson : Stacking__Compiler_program.compiled_expression Lwt.t =
        Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c
      in
      ()
    | Shell ->
      Lwt.return @@
      let running_in_CI = match Sys.getenv "GITLAB_CI" with Some "true" -> true | _ -> false in
      if running_in_CI then 
        let () = Format.eprintf "\nWill run shell command from %s:\n%s\n\n" filename contents in
        let exit_code, output = execute_command contents in
        if exit_code = 0 then
          Format.eprintf "The shell command exited successfully"
        else
          let () = Format.eprintf "The shell command exited with code %d, output:\n%s\n" exit_code output in
          failwith "Command failed"
      else
        Format.eprintf "Would run shell command if env var GITLAB_CI was true: %s\n" contents
  in
  let%map () = Lwt_list.iter_s aux grp_list in
  ()

let write_all_to_files ~raise md_filenames () =
  let aux filename =
    (* Tradeoff: This is duplicated with the "compile" function below, but
       having it duplicated allows having it inside a proper "test". *)
    let groups = get_groups filename in
    let groups_map = SnippetsGroup.bindings groups in

    write_to_files ~raise filename groups_map
  in
  let updates = List.filter ~f:aux md_filenames in
  if List.length updates > 0 then
    failwith ("Some documentation snippets from the following .md were written to external files; please simply re-run `dune runtest'.\n"
              ^
              String.concat ~sep:"\n" updates)
  else
    ()

let compile ~raise filename () =
  (* Format.printf "[compile] Filename: %s@." filename; *)
  let groups = get_groups filename in
  let groups_map = SnippetsGroup.bindings groups in
  let _ : bool = write_to_files ~raise filename groups_map in
  let () = compile_groups ~raise filename groups_map in
  ()

let get_all_md_files () =
  let exclude_files = [ "./gitlab-pages/docs/demo/ligo-snippet.md" ] in
  let ic = Ligo_unix.open_process_in "find ./gitlab-pages/docs -iname \"*.md\"" in
  let all_input = ref [] in
  let () =
    try
      while true do
        let md_file = In_channel.input_line_exn ic in
        if not (List.exists ~f:(String.equal md_file) exclude_files)
        then (
          let grp = get_groups md_file in
          if not (SnippetsGroup.is_empty grp) then all_input := md_file :: !all_input)
      done
    with
    | End_of_file -> In_channel.close ic
  in
  !all_input  

let main =
  Caml.Sys.chdir "../..";
  test_suite "Markdown files"
  @@ let all_md_files = get_all_md_files () in
     test ("Update files") (write_all_to_files all_md_files)
     ::
     List.map
       ~f:(fun md_file -> test ("File : " ^ md_file ^ "\"") (compile md_file))
       all_md_files