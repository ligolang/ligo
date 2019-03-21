(* -*- compile-command: "cd .. ; dune runtest" -*- *)

open Ligo_helpers.Trace
open Ligo

let test name f =
  Alcotest.test_case name `Quick @@ fun _sw ->
  match f () with
  | Ok () -> ()
  | Errors errs ->
      Format.printf "Errors : {\n%a}\n%!" errors_pp errs ;
      raise Alcotest.Test_error

module Ligo = struct
  let pass (source:string) : unit result =
    let%bind raw =
      trace (simple_error "parsing") @@
      parse_file source in
    let%bind simplified =
      trace (simple_error "simplifying") @@
      simplify raw in
    let%bind typed =
      trace (simple_error "typing") @@
      type_ simplified in
    let%bind _mini_c =
      trace (simple_error "transpiling") @@
      transpile typed in
    ok ()

  let basic () : unit result =
    Format.printf "basic test" ;
    pass "./contracts/toto.ligo"

  (* let display_basic () : unit result =
   *   parse_file "./contracts/toto.ligo" >>? fun program_ast ->
   *   Ligo.Typecheck.typecheck_program program_ast >>? fun typed_program ->
   *   Ligo.Transpile.program_to_michelson typed_program >>? fun node ->
   *   let node = Tezos_utils.Cast.flatten_node node in
   *   let str = Tezos_utils.Cast.node_to_string node in
   *   Format.printf "Program:\n%s\n%!" str ;
   *   ok () *)

  let main = "Ligo", [
      test "basic" basic ;
    ]
end

let () =
  (* Printexc.record_backtrace true ; *)
  Alcotest.run "LIGO" [
    Ligo.main ;
  ] ;
  ()
