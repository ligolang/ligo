open Cli_expect

let test basename = "../../test/contracts/contract_metadata/" ^ basename

(* ========================================================================== *)
(* Check of storage type *)

(* -------------------------------------------------------------------------- *)
(* Contracts without a 'metadata' field should not be checked. *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; test "no_metadata.mligo" ];
  [%expect {| { parameter int ; storage int ; code { CDR ; NIL operation ; PAIR } } |}];
  let storage = "42" in
  run_ligo_good [ "compile"; "storage"; test "no_metadata.mligo"; storage ];
  [%expect {| 42 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; test "no_metadata.jsligo" ];
  [%expect {| { parameter int ; storage int ; code { CDR ; NIL operation ; PAIR } } |}];
  let storage = "42" in
  run_ligo_good [ "compile"; "storage"; test "no_metadata.jsligo"; storage ];
  [%expect {| 42 |}]

(* -------------------------------------------------------------------------- *)
(* Contracts with 'metadata' of invalid type should raise warning *)
let%expect_test _ =
  let entrypoint = "Entry_invalid_metadata_1" in
  run_ligo_good [ "compile"; "contract"; test "metadata_tzip16.mligo"; "-m"; entrypoint ];
  [%expect{|
    File "../../test/contracts/contract_metadata/metadata_tzip16.mligo", line 20, characters 16-19:
     19 |      data : int;
     20 |      metadata : nat
                          ^^^
     21 |     }
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      (string, bytes) big_map
    You can disable this warning with the '--no-metadata-check' flag.

    { parameter int ;
      storage (pair (int %data) (nat %metadata)) ;
      code { CDR ; NIL operation ; PAIR } } |}];
  let storage = "{ data  = 42; metadata = 33n}" in
  run_ligo_good
    [ "compile"; "storage"; test "metadata_tzip16.mligo"; "-m"; entrypoint; storage ];
  [%expect{|
    File "../../test/contracts/contract_metadata/metadata_tzip16.mligo", line 20, characters 16-19:
     19 |      data : int;
     20 |      metadata : nat
                          ^^^
     21 |     }
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      (string, bytes) big_map
    You can disable this warning with the '--no-metadata-check' flag.


    Warning: Cannot parse metadata big-map.
    (Pair 42 33) |}]

let%expect_test _ =
  let entrypoint = "Entry_invalid_metadata_1" in
  run_ligo_good [ "compile"; "contract"; test "metadata_tzip16.jsligo"; "-m"; entrypoint ];
  [%expect{|
    File "../../test/contracts/contract_metadata/metadata_tzip16.jsligo", line 18, characters 14-17:
     17 |     data: int,
     18 |     metadata: nat
                        ^^^
     19 |   };
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      big_map<string, bytes>
    You can disable this warning with the '--no-metadata-check' flag.

    { parameter int ;
      storage (pair (int %data) (nat %metadata)) ;
      code { CDR ; NIL operation ; PAIR } } |}];
  let storage = "{ data  : 42, metadata : (33 as nat)}" in
  run_ligo_good
    [ "compile"; "storage"; test "metadata_tzip16.jsligo"; "-m"; entrypoint; storage ];
  [%expect{|
    File "../../test/contracts/contract_metadata/metadata_tzip16.jsligo", line 18, characters 14-17:
     17 |     data: int,
     18 |     metadata: nat
                        ^^^
     19 |   };
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      big_map<string, bytes>
    You can disable this warning with the '--no-metadata-check' flag.


    Warning: Cannot parse metadata big-map.
    (Pair 42 33) |}]

(* -------------------------------------------------------------------------- *)
(* Contracts with 'metadata' of invalid type should raise warning *)
let%expect_test _ =
  let entrypoint = "Entry_invalid_metadata_2" in
  run_ligo_good [ "compile"; "contract"; test "metadata_tzip16.mligo"; "-m"; entrypoint ];
  [%expect{|
    File "../../test/contracts/contract_metadata/metadata_tzip16.mligo", line 34, characters 16-31:
     33 |      data : int;
     34 |      metadata : (bytes, string) big_map
                          ^^^^^^^^^^^^^^^
     35 |     }
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      (string, bytes) big_map
    You can disable this warning with the '--no-metadata-check' flag.

    { parameter int ;
      storage (pair (int %data) (big_map %metadata bytes string)) ;
      code { CDR ; NIL operation ; PAIR } } |}];
  let storage =
    "{ data  = 42 ; metadata = Big_map.literal [ (0x42, \"toto\") ; (0x24, \"titi\") ] }"
  in
  run_ligo_good
    [ "compile"; "storage"; test "metadata_tzip16.mligo"; "-m"; entrypoint; storage ];
  [%expect{|
    File "../../test/contracts/contract_metadata/metadata_tzip16.mligo", line 34, characters 16-31:
     33 |      data : int;
     34 |      metadata : (bytes, string) big_map
                          ^^^^^^^^^^^^^^^
     35 |     }
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      (string, bytes) big_map
    You can disable this warning with the '--no-metadata-check' flag.


    Warning: Cannot parse metadata big-map.
    (Pair 42 { Elt 0x24 "titi" ; Elt 0x42 "toto" }) |}]

let%expect_test _ =
  let entrypoint = "Entry_invalid_metadata_2" in
  run_ligo_good [ "compile"; "contract"; test "metadata_tzip16.jsligo"; "-m"; entrypoint ];
  [%expect{|
    File "../../test/contracts/contract_metadata/metadata_tzip16.jsligo", line 30, characters 14-36:
     29 |     data: int,
     30 |     metadata: big_map<bytes, string>
                        ^^^^^^^^^^^^^^^^^^^^^^
     31 |   };
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      big_map<string, bytes>
    You can disable this warning with the '--no-metadata-check' flag.

    { parameter int ;
      storage (pair (int %data) (big_map %metadata bytes string)) ;
      code { CDR ; NIL operation ; PAIR } } |}];
  let storage =
    "{ data : 42, metadata : Big_map.literal (list([[0x42, \"toto\"],[0x24, \"titi\"]]))}"
  in
  run_ligo_good
    [ "compile"; "storage"; test "metadata_tzip16.jsligo"; "-m"; entrypoint; storage ];
  [%expect{|
    File "../../test/contracts/contract_metadata/metadata_tzip16.jsligo", line 30, characters 14-36:
     29 |     data: int,
     30 |     metadata: big_map<bytes, string>
                        ^^^^^^^^^^^^^^^^^^^^^^
     31 |   };
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      big_map<string, bytes>
    You can disable this warning with the '--no-metadata-check' flag.


    Warning: Cannot parse metadata big-map.
    (Pair 42 { Elt 0x24 "titi" ; Elt 0x42 "toto" }) |}]

(* -------------------------------------------------------------------------- *)
(* Contracts with 'metadata' annotation of invalid type should raise warning *)
let%expect_test _ =
  let entrypoint = "Entry_invalid_metadata_3" in
  run_ligo_good [ "compile"; "contract"; test "metadata_tzip16.mligo"; "-m"; entrypoint ];
  [%expect{|
    File "../../test/contracts/contract_metadata/metadata_tzip16.mligo", line 48, characters 33-48:
     47 |      data : int;
     48 |      [@annot metadata] notdata : (bytes, string) big_map
                                           ^^^^^^^^^^^^^^^
     49 |     }
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      (string, bytes) big_map
    You can disable this warning with the '--no-metadata-check' flag.

    { parameter int ;
      storage (pair (int %data) (big_map %metadata bytes string)) ;
      code { CDR ; NIL operation ; PAIR } } |}];
  let storage =
    "{ data  = 42 ; notdata = Big_map.literal [ (0x42, \"toto\") ; (0x24, \"titi\") ] }"
  in
  run_ligo_good
    [ "compile"; "storage"; test "metadata_tzip16.mligo"; "-m"; entrypoint; storage ];
  [%expect{|
    File "../../test/contracts/contract_metadata/metadata_tzip16.mligo", line 48, characters 33-48:
     47 |      data : int;
     48 |      [@annot metadata] notdata : (bytes, string) big_map
                                           ^^^^^^^^^^^^^^^
     49 |     }
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      (string, bytes) big_map
    You can disable this warning with the '--no-metadata-check' flag.


    Warning: Cannot parse metadata big-map.
    (Pair 42 { Elt 0x24 "titi" ; Elt 0x42 "toto" }) |}]

(* -------------------------------------------------------------------------- *)
(* Contracts with valid 'metadata' type should pass *)
let%expect_test _ =
  let entrypoint = "Entry_valid_metadata" in
  run_ligo_good
    [ "compile"
    ; "storage"
    ; test "metadata_tzip16.mligo"
    ; "-m"
    ; entrypoint
    ; "good_storage"
    ];
  [%expect{|
    (Pair 42
          { Elt "" 0x74657a6f732d73746f726167653a68656c6c6f253246776f726c64 ;
            Elt "hello/world"
                0x207b0a2020226e616d65223a22464132204e4654204d61726b6574706c616365222c0a2020226465736372697074696f6e223a224578616d706c65206f662046413220696d706c656d656e746174696f6e222c0a20202276657273696f6e223a22302e302e31222c0a2020226c6963656e7365223a7b226e616d65223a224d4954227d2c0a202022617574686f7273223a5b224d617269676f6c643c636f6e74616374406d617269676f6c642e6465763e225d2c0a202022686f6d6570616765223a2268747470733a2f2f6d617269676f6c642e646576222c0a202022736f75726365223a7b0a202022746f6f6c73223a5b224c69676f225d2c0a2020226c6f636174696f6e223a2268747470733a2f2f6769746875622e636f6d2f6c69676f6c616e672f636f6e74726163742d636174616c6f6775652f747265652f6d61696e2f6c69622f666132227d2c0a202022696e7465726661636573223a5b22545a49502d303132225d2c0a2020226572726f7273223a205b5d2c0a2020227669657773223a205b0a202020207b0a202020202020226e616d65223a20226765742d616c6c6f77616e63652d666f722d75736572222c0a202020202020226465736372697074696f6e223a2022476574207468652063757272656e7420616c6c6f77616e636520666f7220612075736572206f662074686520636f6e74726163742e222c0a2020202020202270757265223a202274727565222c0a20202020202022696d706c656d656e746174696f6e73223a205b0a2020202020202020207b20226d696368656c736f6e53746f726167655669657722203a207b22706172616d65746572223a7b227072696d223a22696e74227d2c2272657475726e54797065223a7b227072696d223a22696e74227d2c22636f6465223a5b7b227072696d223a22554e50414952227d2c7b227072696d223a2253574150227d2c7b227072696d223a22434152227d2c7b227072696d223a22414444227d5d7d207d0a20202020202020205d0a202020207d0a202020205d0a20207d0a2020 }) |}];
  run_ligo_good
    [ "compile"
    ; "storage"
    ; test "metadata_tzip16.mligo"
    ; "-m"
    ; entrypoint
    ; "ipfs_storage"
    ];
  [%expect{|
    Warning: Metadata in storage points to an IPFS document.
    Hint: If you want to allow download and check it, pass `--allow-json-download`. To prevent this message from appearing, pass `--disallow-json-download`.

    (Pair 42
          { Elt ""
                0x697066733a2f2f516d534263385175796e55376241725547746a774352685a55624a795a51417272637a4b6e714d37685a50746656 }) |}];
  run_ligo_good
    [ "compile"
    ; "storage"
    ; test "metadata_tzip16.mligo"
    ; "-m"
    ; entrypoint
    ; "good_http_storage"
    ];
  [%expect{|
    Warning: Metadata in storage points to an HTTP document.
    Hint: If you want to allow download and check it, pass `--allow-json-download`. To prevent this message from appearing, pass `--disallow-json-download`.

    (Pair 42
          { Elt ""
                0x68747470733a2f2f697066732e696f2f697066732f516d534263385175796e55376241725547746a774352685a55624a795a51417272637a4b6e714d37685a50746656 }) |}];
  run_ligo_good
    [ "compile"
    ; "storage"
    ; test "metadata_tzip16.mligo"
    ; "-m"
    ; entrypoint
    ; "good_http_storage"
    ; "--disallow-json-download"
    ];
  [%expect{|
    (Pair 42
          { Elt ""
                0x68747470733a2f2f697066732e696f2f697066732f516d534263385175796e55376241725547746a774352685a55624a795a51417272637a4b6e714d37685a50746656 }) |}];
  run_ligo_good
    [ "compile"
    ; "storage"
    ; test "metadata_tzip16.mligo"
    ; "-m"
    ; entrypoint
    ; "good_http_sha256_storage"
    ; "--disallow-json-download"
    ];
  [%expect{|
    (Pair 42
          { Elt ""
                0x7368613235363a2f2f3078316637633766646533393162633436613238636439386137373733336662653539323731393064626535343933666538313234346565323964633632343537372f68747470733a253246253246697066732e696f25324669706673253246516d534263385175796e55376241725547746a774352685a55624a795a51417272637a4b6e714d37685a50746656 }) |}];
  run_ligo_good
    [ "compile"
    ; "storage"
    ; test "metadata_tzip16.mligo"
    ; "-m"
    ; entrypoint
    ; "bad_http_sha256_storage"
    ; "--disallow-json-download"
    ];
  [%expect{|
    (Pair 42
          { Elt ""
                0x7368613235363a2f2f3078666637633766646533393162633436613238636439386137373733336662653539323731393064626535343933666538313234346565323964633632343537372f68747470733a253246253246697066732e696f25324669706673253246516d534263385175796e55376241725547746a774352685a55624a795a51417272637a4b6e714d37685a50746656 }) |}];
  run_ligo_good
    [ "compile"
    ; "storage"
    ; test "metadata_tzip16.mligo"
    ; "-m"
    ; entrypoint
    ; "good_tezos_sha256_storage"
    ];
  [%expect{|
    (Pair 42
          { Elt ""
                0x7368613235363a2f2f3078316637633766646533393162633436613238636439386137373733336662653539323731393064626535343933666538313234346565323964633632343537372f74657a6f732d73746f726167653a6d ;
            Elt "m"
                0x7b22617574686f7273223a5b224068696365746e756e6332303030203c68696365746e756e63323030304070726f746f6e6d61696c2e636f6d3e225d2c226465736372697074696f6e223a224f424a4b54732046413220636f6c6c65637469626c6573222c22686f6d6570616765223a2268747470733a2f2f68696365746e756e632e78797a222c22696e7465726661636573223a5b22545a49502d3132225d2c226c6963656e7365223a7b226e616d65223a224d4954227d2c226e616d65223a224f424a4b5473222c227265706f7369746f7279223a2268747470733a2f2f6769746875622e636f6d2f686963326e6332303030222c2276657273696f6e223a22322e302e30227d }) |}];
  run_ligo_good
    [ "compile"
    ; "storage"
    ; test "metadata_tzip16.mligo"
    ; "-m"
    ; entrypoint
    ; "bad_tezos_sha256_storage"
    ];
  [%expect{|
    Warning: Hash mismatch in metadata's JSON document: got 1f7c7fde391bc46a28cd98a77733fbe5927190dbe5493fe81244ee29dc624577, when given ff7c7fde391bc46a28cd98a77733fbe5927190dbe5493fe81244ee29dc624577.
    (Pair 42
          { Elt ""
                0x7368613235363a2f2f3078666637633766646533393162633436613238636439386137373733336662653539323731393064626535343933666538313234346565323964633632343537372f74657a6f732d73746f726167653a6d ;
            Elt "m"
                0x7b22617574686f7273223a5b224068696365746e756e6332303030203c68696365746e756e63323030304070726f746f6e6d61696c2e636f6d3e225d2c226465736372697074696f6e223a224f424a4b54732046413220636f6c6c65637469626c6573222c22686f6d6570616765223a2268747470733a2f2f68696365746e756e632e78797a222c22696e7465726661636573223a5b22545a49502d3132225d2c226c6963656e7365223a7b226e616d65223a224d4954227d2c226e616d65223a224f424a4b5473222c227265706f7369746f7279223a2268747470733a2f2f6769746875622e636f6d2f686963326e6332303030222c2276657273696f6e223a22322e302e30227d }) |}];
  run_ligo_good
    [ "compile"
    ; "storage"
    ; test "metadata_tzip16.mligo"
    ; "-m"
    ; entrypoint
    ; "bad_storage0"
    ];
  [%expect{|
    Warning: Slash ('/') not in a valid position in URI: "hello/invalid_not_http", use instead "%2F".
    (Pair 42
          { Elt "" 0x74657a6f732d73746f726167653a68656c6c6f2f696e76616c69645f6e6f745f68747470 ;
            Elt "hello/world" 0x4a534f4e3f ;
            Elt "invalid_not_http" 0x68747470733a2f2f7777772e6578616d706c652e636f6d ;
            Elt "invalid_trailing_slash"
                0x697066733a2f2f516d577169337542684251354b55367352314c704c714a5472344778755066454b3755447976364763633366484c2f ;
            Elt "invalid_wrong_hash" 0x697066733a2f2f696e76616c69642d68617368 }) |}];
  run_ligo_good
    [ "compile"
    ; "storage"
    ; test "metadata_tzip16.mligo"
    ; "-m"
    ; entrypoint
    ; "bad_storage1"
    ];
  [%expect{|
    Warning: Could not find a valid URI haha in storage's metadata empty key.
    (Pair 42
          { Elt "" 0x68616861 ;
            Elt "hello/world" 0x687474703a2f2f7777772e6578616d706c652e636f6d }) |}];
  run_ligo_good
    [ "compile"
    ; "storage"
    ; test "metadata_tzip16.mligo"
    ; "-m"
    ; entrypoint
    ; "bad_storage2"
    ];
  [%expect{|
    Warning: Could not find key haha in storage's metadata.
    (Pair 42
          { Elt "" 0x74657a6f732d73746f726167653a68616861 ;
            Elt "hello/world" 0x687474703a2f2f7777772e6578616d706c652e636f6d }) |}];
  run_ligo_good
    [ "compile"
    ; "storage"
    ; test "metadata_tzip16.mligo"
    ; "-m"
    ; entrypoint
    ; "bad_storage3"
    ];
  [%expect{|
    Warning: Could not parse JSON in storage's metadata: "Line 1, bytes 0-7:
    Invalid token 'nojson!'".
    (Pair 42
          { Elt "" 0x74657a6f732d73746f726167653a68616861 ; Elt "haha" 0x6e6f6a736f6e21 }) |}];
  run_ligo_good
    [ "compile"
    ; "storage"
    ; test "metadata_tzip16.mligo"
    ; "-m"
    ; entrypoint
    ; "bad_storage4"
    ];
  [%expect{|
    Warning: Error in JSON in storage's metadata: could not successfully typecheck the view "get-allowance-for-user" w.r.t. to parameter, returntType and storage of the contract.
    (Pair 42
          { Elt "" 0x74657a6f732d73746f726167653a68656c6c6f253246776f726c64 ;
            Elt "hello/world"
                0x207b0a2020226e616d65223a22464132204e4654204d61726b6574706c616365222c0a2020226465736372697074696f6e223a224578616d706c65206f662046413220696d706c656d656e746174696f6e222c0a20202276657273696f6e223a22302e302e31222c0a2020226c6963656e7365223a7b226e616d65223a224d4954227d2c0a202022617574686f7273223a5b224d617269676f6c643c636f6e74616374406d617269676f6c642e6465763e225d2c0a202022686f6d6570616765223a2268747470733a2f2f6d617269676f6c642e646576222c0a202022736f75726365223a7b0a202022746f6f6c73223a5b224c69676f225d2c0a2020226c6f636174696f6e223a2268747470733a2f2f6769746875622e636f6d2f6c69676f6c616e672f636f6e74726163742d636174616c6f6775652f747265652f6d61696e2f6c69622f666132227d2c0a202022696e7465726661636573223a5b22545a49502d303132225d2c0a2020226572726f7273223a205b5d2c0a2020227669657773223a205b0a202020207b0a202020202020226e616d65223a20226765742d616c6c6f77616e63652d666f722d75736572222c0a202020202020226465736372697074696f6e223a2022476574207468652063757272656e7420616c6c6f77616e636520666f7220612075736572206f662074686520636f6e74726163742e222c0a2020202020202270757265223a202274727565222c0a20202020202022696d706c656d656e746174696f6e73223a205b0a2020202020202020207b20226d696368656c736f6e53746f726167655669657722203a207b22706172616d65746572223a7b227072696d223a22737472696e67227d2c2272657475726e54797065223a7b227072696d223a22696e74227d2c22636f6465223a5b7b227072696d223a22554e50414952227d2c7b227072696d223a2253574150227d2c7b227072696d223a22434152227d2c7b227072696d223a22414444227d5d7d207d0a20202020202020205d0a202020207d0a202020205d0a20207d0a2020 }) |}]

let%expect_test _ =
  let entrypoint = "Entry_valid_metadata" in
  run_ligo_good [ "compile"; "contract"; test "metadata_tzip16.mligo"; "-m"; entrypoint ];
  [%expect{|
    { parameter int ;
      storage (pair (int %data) (big_map %metadata string bytes)) ;
      code { CDR ; NIL operation ; PAIR } } |}];
  let storage =
    "{ data  = 42 ; metadata = Big_map.literal [ (\"toto\", 0x42) ; (\"titi\", 0x24) ] }"
  in
  run_ligo_good
    [ "compile"; "storage"; test "metadata_tzip16.mligo"; "-m"; entrypoint; storage ];
  [%expect{|
    Warning: Empty key in metadata big-map is mandatory.
    (Pair 42 { Elt "titi" 0x24 ; Elt "toto" 0x42 }) |}]

let%expect_test _ =
  let entrypoint = "Entry_valid_metadata" in
  run_ligo_good [ "compile"; "contract"; test "metadata_tzip16.jsligo"; "-m"; entrypoint ];
  [%expect{|
    { parameter int ;
      storage (pair (int %data) (big_map %metadata string bytes)) ;
      code { CDR ; NIL operation ; PAIR } } |}];
  let storage =
    "{ data : 42, metadata : Big_map.literal (list([[\"toto\", 0x42],[\"titi\", 0x24]]))}"
  in
  run_ligo_good
    [ "compile"; "storage"; test "metadata_tzip16.jsligo"; "-m"; entrypoint; storage ];
  [%expect{|
    Warning: Empty key in metadata big-map is mandatory.
    (Pair 42 { Elt "titi" 0x24 ; Elt "toto" 0x42 }) |}]

(* -------------------------------------------------------------------------- *)
(* Contracts with invalid 'metadata' should pass when the waiver flag is enabled *)

let%expect_test _ =
  let entrypoint = "Entry_invalid_metadata_1" in
  run_ligo_good
    [ "compile"
    ; "contract"
    ; test "metadata_tzip16.mligo"
    ; "--no-metadata-check"
    ; "-m"
    ; entrypoint
    ];
  [%expect{|
    { parameter int ;
      storage (pair (int %data) (nat %metadata)) ;
      code { CDR ; NIL operation ; PAIR } } |}];
  let storage = "{ data  = 42; metadata = 33n}" in
  run_ligo_good
    [ "compile"
    ; "storage"
    ; test "metadata_tzip16.mligo"
    ; "--no-metadata-check"
    ; "-m"
    ; entrypoint
    ; storage
    ];
  [%expect{| (Pair 42 33) |}]

let%expect_test _ =
  let entrypoint = "Entry_invalid_metadata_1" in
  run_ligo_good
    [ "compile"
    ; "contract"
    ; test "metadata_tzip16.jsligo"
    ; "--no-metadata-check"
    ; "-m"
    ; entrypoint
    ];
  [%expect{|
    { parameter int ;
      storage (pair (int %data) (nat %metadata)) ;
      code { CDR ; NIL operation ; PAIR } } |}];
  let storage = "{ data  : 42, metadata : (33 as nat)}" in
  run_ligo_good
    [ "compile"
    ; "storage"
    ; "--no-metadata-check"
    ; test "metadata_tzip16.jsligo"
    ; "-m"
    ; entrypoint
    ; storage
    ];
  [%expect{| (Pair 42 33) |}]
