open! Trace

let test name f =
  Alcotest.test_case name `Quick @@ fun () ->
  let result =
    trace (fun () -> error (thunk "running test") (fun () -> name) ()) @@
    f () in
  match result with
  | Ok ((), annotations) -> ignore annotations; ()
  | Errors errs ->
      Format.printf "Errors : {\n%a}\n%!" errors_pp (List.rev (List.rev_map (fun f -> f ()) errs)) ;
      raise Alcotest.Test_error
