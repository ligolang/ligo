let run_ligo ~reset_return ~(run : ?argv:string list -> unit -> int) args =
  Ast_core.ValueVar.reset_counter ();
  Ast_core.TypeVar.reset_counter ();
  Ast_core.ModuleVar.reset_counter ();
  Self_ast_aggregated.reset_counter ();
  reset_return ();
  let argv = ("ligo" :: args) in
  let result = run ~argv () in
  result


(* let _maybe_useful = "\x0" *)
let main ~reset_return ~(run : ?argv:string list -> unit -> int) () =
  let stdin = In_channel.stdin in
  while true do    
    let x = Stdio.In_channel.input_line stdin in
    (* get_char '\0' *)
    (* \0 *)
    (* In_channel.really_input_exn *)
    let x = match x with 
      Some s ->
          (* TODO: handle result Error *)
        let args = Result.ok_or_failwith (Parse_argv.parse s) in
        (* let () = List.iter args ~f:print_endline in *)
        let _ = run_ligo args ~reset_return ~run in
        ""
    | None -> "could not read" in
    print_endline x;
  done;
  Ok ("","")


