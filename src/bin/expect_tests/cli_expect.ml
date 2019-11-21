open Cmdliner

(* exit <> 0 but expected exit = 0 *)
exception Should_exit_good

(* exit = 0 but expected exit <> 0 *)
exception Should_exit_bad

(* ugh, can we avoid this? *)
let () = Unix.putenv "TERM" "dumb"

let run_ligo args =
  let argv = Array.of_list ("ligo" :: args) in
  let result = Cli.run ~argv () in
  Term.exit_status_of_result result

let run_ligo_good args =
  let exit_code = run_ligo args in
  if (exit_code <> 0)
  then raise Should_exit_good
  else ()

let run_ligo_bad args =
  let exit_code = run_ligo args in
  if (exit_code = 0)
  then raise Should_exit_bad
  else ()
