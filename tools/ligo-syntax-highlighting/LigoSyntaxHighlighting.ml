open Cmdliner

let vscode_directory =
  let doc = "Output VSCode files at given directory." in
  Arg.(required & opt (some string) None & info ["vscode"] ~doc)

let vim_directory =
  let doc = "Output VIM files at given directory." in
  Arg.(required & opt (some string) None & info ["vim"] ~doc)

let emacs_directory =
  let doc = "Output Emacs files at given directory." in
  Arg.(required & opt (some string) None & info ["emacs"] ~doc)

let textmate_directory =
  let doc = "Output TextMate files at given directory." in
  Arg.(required & opt (some string) None & info ["textmate"] ~doc)

let output_file output_directory file s =
  let oc = (open_out @@ Filename.concat output_directory file) in
  let fmt = Format.formatter_of_out_channel oc in
  let () = Format.fprintf fmt "%s" s in
  close_out oc

let vim_syntax_highlighting dir syntaxes =
  let write_file file_name subdir_name contents =
    let subdir = Filename.concat dir subdir_name in
    if not (Sys.file_exists subdir) then
      Ligo_unix.mkdir subdir ~perm:0o777;
    output_file subdir file_name contents
  in
  let vim_file_name = "ligo.vim" in
  let open SyntaxHighlighting.VIM in
  let vim_ftdetect = vim_ftdetect syntaxes in
  write_file vim_file_name "ftdetect" vim_ftdetect;
  let vim_plugin = vim_plugin syntaxes in
  write_file vim_file_name "plugin" vim_plugin;
  Fun.flip List.iter syntaxes (fun (name, syntax) ->
    let vim_syntax = vim_syntax (name, syntax) in
    write_file (name ^ ".vim") "syntax" vim_syntax);
  `Ok ()

let textmate_syntax_highlighting_impl (generate_configuration: bool) (dir: string) (syntaxes: (string * SyntaxHighlighting.Core.t) list): unit Term.ret =
  let go (syntax, textmate) = function
    | Ok () -> (
      match SyntaxHighlighting.Textmate.to_jsons syntax textmate with
      | Ok (syntax_highlighting_json, language_conf_json) ->
          let s = Yojson.Safe.pretty_to_string syntax_highlighting_json in
          let syntax_file = syntax ^ ".tmLanguage.json" in
          output_file dir syntax_file s;
          (if generate_configuration then
            let s = Yojson.Safe.pretty_to_string language_conf_json in
            let language_file = syntax ^ ".configuration.json" in
            output_file dir language_file s);
          Ok ()
      | Error SyntaxHighlighting.Core.Referenced_rule_does_not_exist s -> Error (Format.sprintf "Referenced rule '%s' does not exist." s)
      | Error Meta_name_some_but_empty s -> Error (Format.sprintf "%s.name has no value, but is expected to." s)
      | Error Begin_cant_be_empty s -> Error (Format.sprintf "%s.begin_ can't be empty." s)
      | Error End_cant_be_empty s -> Error (Format.sprintf "%s.end_ can't be empty" s))
    | Error s -> Error s
  in
  match List.fold_left (Fun.flip go) (Ok ()) syntaxes with
  | Ok () -> `Ok ()
  | Error s -> `Error (false, s)

let vscode_syntax_highlighting: string -> (string * SyntaxHighlighting.Core.t) list -> unit Term.ret =
  textmate_syntax_highlighting_impl true

let textmate_syntax_highlighting: string -> (string * SyntaxHighlighting.Core.t) list -> unit Term.ret =
  textmate_syntax_highlighting_impl false

let emacs_syntax_highlighting dir syntaxes =
  let buffer = Buffer.create 1000 in
  let open Format in
  let module Print = SyntaxHighlighting.Emacs.Print in
  let fmt = formatter_of_buffer buffer in
  Print.print_pre fmt;
  Print.print_faces fmt;
  Print.print_customatizable_options fmt;
  Print.print_lsp fmt;
  List.iter (fun ((name, t): (string * SyntaxHighlighting.Core.t)) ->
    Print.print fmt name t.alt_name t;
  ) syntaxes;
  Print.print_footer fmt;
  let emacs_output = Buffer.contents buffer in
  let () = output_file dir "ligo-mode.el" emacs_output in
  `Ok ()

let ( let* ) o f : _ Term.ret  =
  match o with
  | `Error _ as e -> e
  | `Help _ as h -> h
  | `Ok x -> f x

let output: string -> string -> string -> string -> _ Term.ret = fun vscode_directory vim_directory emacs_directory textmate_directory ->
  if not (Sys.is_directory vscode_directory) then
    `Error (false, "Not a valid directory to output Visual Studio Code files")
  else if not (Sys.is_directory vim_directory) then
    `Error (false, "Not a valid directory to output Vim files")
  else if not (Sys.is_directory emacs_directory) then
    `Error (false, "Not a valid directory to output Emacs files")
  else if not (Sys.is_directory textmate_directory) then
    `Error (false, "Not a valid directory to output TextMate files")
  else (
    let syntaxes_without_jsligo = [
      ("mligo", CameLIGO.syntax_highlighting);
    ] in
    let syntaxes = ("jsligo", JsLIGO.syntax_highlighting) :: syntaxes_without_jsligo in

    let* _ = vscode_syntax_highlighting vscode_directory syntaxes in
    print_endline "Success (Visual Studio Code)";
    let* _ = textmate_syntax_highlighting textmate_directory syntaxes in
    print_endline "Success (TextMate)";
    let* _ = vim_syntax_highlighting vim_directory syntaxes_without_jsligo in
    print_endline "Success (Vim)";
    let* _ = emacs_syntax_highlighting emacs_directory syntaxes_without_jsligo in
    print_endline "Success (Emacs)";
    let () = print_endline "Successfully generated syntaxes" in
    `Ok ()
  )

let generate_syntax_highlighting : unit Cmd.t =
  let doc = "generate syntax highlighting" in
  let exits = Cmd.Exit.defaults in
  let info = Cmd.info "LigoSyntaxHighlighting" ~exits ~doc in
  let terms = Term.(ret (const output $ vscode_directory $ vim_directory $ emacs_directory $ textmate_directory)) in
  Cmd.v info terms

let () = Stdlib.exit @@ Cmd.eval generate_syntax_highlighting
