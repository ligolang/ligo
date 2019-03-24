open Ligo_helpers.Trace

let test name f =
  Alcotest.test_case name `Quick @@ fun () ->
  match f () with
  | Ok () -> ()
  | Errors errs ->
      Format.printf "Errors : {\n%a}\n%!" errors_pp errs ;
      raise Alcotest.Test_error
