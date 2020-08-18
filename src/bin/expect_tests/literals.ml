open Cli_expect

let%expect_test _ =
  run_ligo_good ["interpret" ; "(\"edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7\":signature)" ; "--syntax=pascaligo"] ;
  [%expect {| Signature edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7 |}]

let%expect_test _ =
  run_ligo_bad ["interpret" ; "(\"thisisnotasignature\":signature)" ; "--syntax=pascaligo"] ;
  [%expect {|
    ligo: error
          in file "", line 0, characters 0-33
          Ill-formed literal "Signature thisisnotasignature".
          In the case of an address, a string is expected prefixed by either tz1, tz2, tz3 or KT1 and followed by a Base58 encoded hash and terminated by a 4-byte checksum.
          In the case of a key_hash, signature, or key a Base58 encoded hash is expected.


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_good ["interpret" ; "(\"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav\":key)" ; "--syntax=pascaligo"] ;
  [%expect {| key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav |}]

let%expect_test _ =
  run_ligo_bad ["interpret" ; "(\"thisisnotapublickey\":key)" ; "--syntax=pascaligo"] ;
  [%expect {|
    ligo: error
          in file "", line 0, characters 0-27
          Ill-formed literal "key thisisnotapublickey".
          In the case of an address, a string is expected prefixed by either tz1, tz2, tz3 or KT1 and followed by a Base58 encoded hash and terminated by a 4-byte checksum.
          In the case of a key_hash, signature, or key a Base58 encoded hash is expected.


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}]
