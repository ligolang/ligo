let output_null_char () =
  let () = Out_channel.output_char stdout '\x00' in
  let () = Out_channel.output_char stderr '\x00' in
  let () = Out_channel.flush stdout in
  Out_channel.flush stderr


let main ~ligo_bin_path () =
  let () = Caml.Sys.catch_break true in
  let stdin = In_channel.stdin in
  let exception Break in
  try
    let () =
      while true do
        let e =
          match Stdio.In_channel.input_line stdin with
          | Some s ->
            let args = Result.ok_or_failwith (Parse_argv.parse s) in
            let args = Array.of_list args in
            let () =
              match args with
              | [| "exit" |] -> raise Break
              | args ->
                let cmd = "", Array.concat [ [| ligo_bin_path |]; args ] in
                let _ = Cli_helpers.run_command cmd in
                ()
            in
            ""
          | None -> "Error: unable to read from stdin."
        in
        let () = print_string e in
        output_null_char ()
      done
    in
    Ok ("\x00", "\x00")
  with
  | Break | Caml.Sys.Break ->
    output_null_char ();
    Ok ("\x00", "\x00")
  | _ ->
    output_null_char ();
    Error ("\x00", "Unexpected error\x00")
