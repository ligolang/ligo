open Test_helpers

let test_build_test_contracts ~raise () =
  let test () =
    let options =
      let options = Test_helpers.options in
      let options = Compiler_options.set_syntax options None in
      Compiler_options.set_test_flag options false
    in
    let f = "./contracts/build/D.mligo" in
    let (_, env) : Ast_typed.program * Checking.Persistent_env.t =
      Build.qualified_typed_with_env ~raise ~options (Build.Source_input.From_file f)
    in
    Checking.(
      Map.iteri env.cmis ~f:(fun ~key:path ~data:(expected_cmi, expected_crc) ->
          Cmi.Serialized.output expected_cmi;
          match Cmi.Serialized.input path with
          | None ->
            Core.raise @@ Failure ("Failed to deserialize " ^ path ^ ": can't read back")
          | Some (cmi, crc) ->
            (match
               ( Md5.equal crc expected_crc
               , Ast_typed.equal_signature cmi.sign expected_cmi.sign )
             with
            | false, _ ->
              Core.raise
              @@ Failure
                   (sprintf
                      "Failed to deserialize %s: crc mismatch\nExpected: %s\nGot: %s"
                      path
                      (Md5.to_hex expected_crc)
                      (Md5.to_hex crc))
            | true, false ->
              Core.raise @@ Failure "Failed to deserialize: read value is different"
            | true, true -> ())))
  in
  Alcotest.(check unit)
    "test build test contracts serialization/deseriazation roundtrip"
    (test ())
    ()


let main =
  test_suite
    "Serialization/Deserialization roundtrip tests"
    [ test "Build test contracts" test_build_test_contracts ]
