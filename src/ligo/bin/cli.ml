open Trace

let toplevel x =
  match x with
  | Trace.Ok ((), annotations) -> ignore annotations; ()
  | Errors ss ->
      Format.printf "Errors: %a\n%!" errors_pp @@ List.map (fun f -> f()) ss

let main () =
  let l = Array.length Sys.argv in
  let%bind () =
    if l < 2
    then simple_fail "Pass a command"
    else ok () in
  let command = Sys.argv.(1) in
  match command with
  | "compile" -> (
      let sub_command = Sys.argv.(2) in
      match sub_command with
      | "file" -> (
          let%bind () =
            trace_strong (simple_error "bad number of args") @@
            Assert.assert_equal_int 5 l in
          let source = Sys.argv.(3) in
          let entry_point = Sys.argv.(4) in
          let%bind contract =
            trace (simple_error "compile michelson") @@
            Ligo.Contract.compile_contract_file source entry_point in
          Format.printf "Contract:\n%s\n" contract ;
          ok ()
        )
      | "expression" -> (
          let%bind () =
            trace_strong (simple_error "bad number of args") @@
            Assert.assert_equal_int 6 l in
          let source = Sys.argv.(3) in
          let entry_point = Sys.argv.(4) in
          let expression = Sys.argv.(5) in
          let%bind value =
            trace (simple_error "compile expression") @@
            Ligo.Contract.compile_contract_parameter source entry_point expression in
          Format.printf "Input:\n%s\n" value;
          ok ()
        )
      | _ -> simple_fail "Bad sub-command"
    )
  | _ -> simple_fail "Bad command"

let () = toplevel @@ main ()
