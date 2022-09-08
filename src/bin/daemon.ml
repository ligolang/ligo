let main () =
  let () = Sys.catch_break true in
  let stdin = In_channel.stdin in
  let exception Break in
  try
    let () = while true do
    let x = Stdio.In_channel.input_line stdin in
    let x = match x with
      Some s ->
        let args = Result.ok_or_failwith (Parse_argv.parse s) in
        let args = Array.of_list args in
        let () = match args with
          [| "exit" |] -> raise Break
        | args ->
          let cmd = ("", Array.concat [[| "ligo" |] ; args]) in
          let _ = Cli_helpers.run_command cmd in
          ()
        in
        ""
    | None -> "Error: unable to read from stdin." in
    print_endline x;
    done in Ok("", "")
  with 
    Break
  | Sys.Break -> Ok("", "") 
  | _ -> Error ("","Unexpected error")