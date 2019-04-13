open Trace

let toplevel x =
  match x with
  | Trace.Ok () -> ()
  | Errors ss ->
      Format.printf "Errors: %a\n%!" errors_pp @@ List.map (fun f -> f()) ss

let main () =
  let l = Array.length Sys.argv in
  let%bind () =
    if l < 2
    then simple_fail "Pass a command"
    else ok () in
  let command = Sys.argv.(1) in
  (* Format.printf "Processing command %s (%d)\n" command l ; *)
  match command with
  | "compile" -> (
      let%bind () =
        if l <> 4
        then simple_fail "Bad number of argument to compile"
        else ok () in
      let source = Sys.argv.(2) in
      let entry_point = Sys.argv.(3) in
      (* Format.printf "Compiling %s from %s\n%!" entry_point source ; *)
      let%bind michelson =
        trace (simple_error "compile michelson") @@
        Ligo.compile_file source entry_point in
      Format.printf "Program : %a\n" Micheline.Michelson.pp michelson ;
      ok ()
    )
    | _ -> simple_fail "Bad command"

let () = toplevel @@ main ()
