open! Trace

let test name f =
  Alcotest.test_case name `Quick @@ fun () ->
  let result =
    trace (error "running test" name) @@
    f () in
  match result with
  | Ok () -> ()
  | Errors errs ->
      Format.printf "Errors : {\n%a}\n%!" errors_pp errs ;
      raise Alcotest.Test_error
