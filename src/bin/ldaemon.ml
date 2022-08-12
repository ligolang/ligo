let main () =
  let stdin = In_channel.stdin in
  while true do    
    let x = Stdio.In_channel.input_line stdin in
    let x = match x with 
      Some s ->
        let args = Result.ok_or_failwith (Parse_argv.parse s) in
        let args = Array.of_list args in
        let cmd = ("", Array.concat [[| "ligo" |] ; args]) in
        let _ = Cli_helpers.run_command cmd in
        ""
    | None -> "could not read" in
    print_endline x;
  done;
  Ok ("","")


