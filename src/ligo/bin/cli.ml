open Trace

let () =
  let l = Array.length Sys.argv in
  if l < 2
  then raise (Failure "Pass a command") ;
  Format.printf "Toto %d\n%!" (Array.length Sys.argv) ;
  let command = Sys.argv.(1) in
  let _ =
    match command with
    | "compile" -> (
        let%bind () =
          if l <> 4
          then simple_fail "Bad number of argument to compile"
          else ok () in
        let source = Sys.argv.(2) in
        let entry_point = Sys.argv.(3) in
        let%bind michelson = Ligo.compile_file source entry_point in
        Format.printf "%a" Micheline.Michelson.pp michelson ;
        ok ()
      )
    | _ -> simple_fail "Bad command"
  in
  ()
