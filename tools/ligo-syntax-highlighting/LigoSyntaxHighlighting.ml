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

let vim_syntax_highlighting dir file textmate =
  let vim_output = SyntaxHighlighting.VIM.to_vim textmate in
  let () = output_file dir file vim_output in
  let () = print_endline "Success" in
  `Ok ()

let vscode_syntax_highlighting: string -> string -> string -> string -> SyntaxHighlighting.Core.t -> unit Term.ret = fun dir syntax_file language_file syntax textmate ->
  let jsons = SyntaxHighlighting.Textmate.to_jsons syntax textmate in
  match jsons with
    Ok (syntax_highlighting_json, language_conf_json) ->
      let s = Yojson.Safe.pretty_to_string syntax_highlighting_json in
      let () = output_file dir syntax_file s in
      let s = Yojson.Safe.pretty_to_string language_conf_json in
      let () = output_file dir language_file s in
      let () = print_endline "Success" in
      `Ok ()
  | Error SyntaxHighlighting.Core.Referenced_rule_does_not_exist s -> `Error (false, Format.sprintf "Referenced rule '%s' does not exist." s)
  | Error Meta_name_some_but_empty s -> `Error (false, Format.sprintf  "%s.name has no value, but is expected to." s)
  | Error Begin_cant_be_empty s -> `Error (false, Format.sprintf  "%s.begin_ can't be empty." s)
  | Error End_cant_be_empty s -> `Error (false, Format.sprintf  "%s.end_ can't be empty" s)

let textmate_syntax_highlighting: string -> string -> string -> SyntaxHighlighting.Core.t -> unit Term.ret = fun dir file syntax textmate ->
  let jsons = SyntaxHighlighting.Textmate.to_jsons syntax textmate in
  match jsons with
    Ok (syntax_highlighting_json, _) ->
      let s = Yojson.Safe.pretty_to_string syntax_highlighting_json in
      let () = output_file dir file s in
      let () = print_endline "Success" in
      `Ok ()
  | Error SyntaxHighlighting.Core.Referenced_rule_does_not_exist s -> `Error (false, Format.sprintf "Referenced rule '%s' does not exist." s)
  | Error Meta_name_some_but_empty s -> `Error (false, Format.sprintf  "%s.name has no value, but is expected to." s)
  | Error Begin_cant_be_empty s -> `Error (false, Format.sprintf  "%s.begin_ can't be empty." s)
  | Error End_cant_be_empty s -> `Error (false, Format.sprintf  "%s.end_ can't be empty" s)

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
  let emacs_output = Buffer.contents buffer in
  let () = output_file dir "ligo-mode.el" emacs_output in
  let () = print_endline "Success" in
  `Ok ()

let ( let* ) o f : _ Term.ret  =
  match o with
  | `Error _ as e -> e
  | `Help _ as h -> h
  | `Ok x -> f x

let output: string -> string -> string -> string -> _ Term.ret = fun vscode_directory vim_directory emacs_directory textmate_directory ->
  if not (Sys.is_directory vscode_directory) then
    `Error (false, "Not a valid directory to output VSCode files")
  else if not (Sys.is_directory vim_directory) then
    `Error (false, "Not a valid directory to output VIM files")
  else if not (Sys.is_directory emacs_directory) then
    `Error (false, "Not a valid directory to output EMacs files")
  else if not (Sys.is_directory textmate_directory) then
    `Error (false, "Not a valid directory to output TextMate files")
  else (
    let* _ = vscode_syntax_highlighting vscode_directory "ligo.tmLanguage.json" "ligo.configuration.json" "ligo" PascaLIGO.syntax_highlighting in
    let* _ = textmate_syntax_highlighting textmate_directory "ligo.tmLanguage.json" "ligo" PascaLIGO.syntax_highlighting in
    let* _ = vim_syntax_highlighting vim_directory "ligo.vim" PascaLIGO.syntax_highlighting in
    let* _ = vscode_syntax_highlighting vscode_directory "mligo.tmLanguage.json" "mligo.configuration.json" "mligo" CameLIGO.syntax_highlighting in
    let* _ = textmate_syntax_highlighting textmate_directory "mligo.tmLanguage.json" "mligo" CameLIGO.syntax_highlighting in
    let* _ = vim_syntax_highlighting vim_directory "mligo.vim" CameLIGO.syntax_highlighting in
    let* _ = vscode_syntax_highlighting vscode_directory "religo.tmLanguage.json" "religo.configuration.json" "religo" ReasonLIGO.syntax_highlighting in
    let* _ = textmate_syntax_highlighting textmate_directory "religo.tmLanguage.json" "religo" ReasonLIGO.syntax_highlighting in
    let* _ = vim_syntax_highlighting vim_directory "religo.vim" ReasonLIGO.syntax_highlighting in
    let* _ = vscode_syntax_highlighting vscode_directory "jsligo.tmLanguage.json" "jsligo.configuration.json" "jsligo" JsLIGO.syntax_highlighting in
    let* _ = textmate_syntax_highlighting textmate_directory "jsligo.tmLanguage.json" "jsligo" JsLIGO.syntax_highlighting in
    let* _ = emacs_syntax_highlighting emacs_directory [("ligo", PascaLIGO.syntax_highlighting);
                                                        ("mligo", CameLIGO.syntax_highlighting);
                                                        ("religo", ReasonLIGO.syntax_highlighting)]
    in
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
