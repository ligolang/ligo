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
  let entrypoint = "entry_invalid_metadata_1" in
  run_ligo_good [ "compile"; "contract"; test "metadata_tzip16.mligo"; "-e"; entrypoint ];
  [%expect
    {|
    File "../../test/contracts/contract_metadata/metadata_tzip16.mligo", line 16, characters 15-18:
     15 |   { data     : int
     16 |   ; metadata : nat
                         ^^^
     17 |   }
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
    [ "compile"; "storage"; test "metadata_tzip16.mligo"; "-e"; entrypoint; storage ];
  [%expect
    {|
    File "../../test/contracts/contract_metadata/metadata_tzip16.mligo", line 16, characters 15-18:
     15 |   { data     : int
     16 |   ; metadata : nat
                         ^^^
     17 |   }
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      (string, bytes) big_map
    You can disable this warning with the '--no-metadata-check' flag.

    (Pair 42 33) |}]

let%expect_test _ =
  let entrypoint = "entry_invalid_metadata_1" in
  run_ligo_good [ "compile"; "contract"; test "metadata_tzip16.jsligo"; "-e"; entrypoint ];
  [%expect
    {|
    File "../../test/contracts/contract_metadata/metadata_tzip16.jsligo", line 17, characters 12-15:
     16 |   data: int,
     17 |   metadata: nat,
                      ^^^
     18 | };
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
    [ "compile"; "storage"; test "metadata_tzip16.jsligo"; "-e"; entrypoint; storage ];
  [%expect
    {|
    File "../../test/contracts/contract_metadata/metadata_tzip16.jsligo", line 17, characters 12-15:
     16 |   data: int,
     17 |   metadata: nat,
                      ^^^
     18 | };
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      big_map<string, bytes>
    You can disable this warning with the '--no-metadata-check' flag.

    (Pair 42 33) |}]

(* -------------------------------------------------------------------------- *)
(* Contracts with 'metadata' of invalid type should raise warning *)
let%expect_test _ =
  let entrypoint = "entry_invalid_metadata_2" in
  run_ligo_good [ "compile"; "contract"; test "metadata_tzip16.mligo"; "-e"; entrypoint ];
  [%expect
    {|
    File "../../test/contracts/contract_metadata/metadata_tzip16.mligo", line 25, characters 15-30:
     24 |   { data     : int
     25 |   ; metadata : (bytes, string) big_map
                         ^^^^^^^^^^^^^^^
     26 |   }
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
    [ "compile"; "storage"; test "metadata_tzip16.mligo"; "-e"; entrypoint; storage ];
  [%expect
    {|
    File "../../test/contracts/contract_metadata/metadata_tzip16.mligo", line 25, characters 15-30:
     24 |   { data     : int
     25 |   ; metadata : (bytes, string) big_map
                         ^^^^^^^^^^^^^^^
     26 |   }
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      (string, bytes) big_map
    You can disable this warning with the '--no-metadata-check' flag.

    (Pair 42 { Elt 0x24 "titi" ; Elt 0x42 "toto" }) |}]

let%expect_test _ =
  let entrypoint = "entry_invalid_metadata_2" in
  run_ligo_good [ "compile"; "contract"; test "metadata_tzip16.jsligo"; "-e"; entrypoint ];
  [%expect
    {|
    File "../../test/contracts/contract_metadata/metadata_tzip16.jsligo", line 29, characters 12-34:
     28 |   data: int,
     29 |   metadata: big_map<bytes, string>;
                      ^^^^^^^^^^^^^^^^^^^^^^
     30 | };
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
    [ "compile"; "storage"; test "metadata_tzip16.jsligo"; "-e"; entrypoint; storage ];
  [%expect
    {|
    File "../../test/contracts/contract_metadata/metadata_tzip16.jsligo", line 29, characters 12-34:
     28 |   data: int,
     29 |   metadata: big_map<bytes, string>;
                      ^^^^^^^^^^^^^^^^^^^^^^
     30 | };
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      big_map<string, bytes>
    You can disable this warning with the '--no-metadata-check' flag.

    (Pair 42 { Elt 0x24 "titi" ; Elt 0x42 "toto" }) |}]

(* -------------------------------------------------------------------------- *)
(* Contracts with 'metadata' annotation of invalid type should raise warning *)
let%expect_test _ =
  let entrypoint = "entry_invalid_metadata_3" in
  run_ligo_good [ "compile"; "contract"; test "metadata_tzip16.mligo"; "-e"; entrypoint ];
  [%expect
    {|
    File "../../test/contracts/contract_metadata/metadata_tzip16.mligo", line 34, characters 32-47:
     33 |   { data     : int
     34 |   ; [@annot metadata] notdata : (bytes, string) big_map
                                          ^^^^^^^^^^^^^^^
     35 |   }
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
    [ "compile"; "storage"; test "metadata_tzip16.mligo"; "-e"; entrypoint; storage ];
  [%expect
    {|
    File "../../test/contracts/contract_metadata/metadata_tzip16.mligo", line 34, characters 32-47:
     33 |   { data     : int
     34 |   ; [@annot metadata] notdata : (bytes, string) big_map
                                          ^^^^^^^^^^^^^^^
     35 |   }
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      (string, bytes) big_map
    You can disable this warning with the '--no-metadata-check' flag.

    (Pair 42 { Elt 0x24 "titi" ; Elt 0x42 "toto" }) |}]

(* -------------------------------------------------------------------------- *)
(* Contracts with valid 'metadata' type should pass *)
let%expect_test _ =
  let entrypoint = "entry_valid_metadata" in
  run_ligo_good [ "compile"; "contract"; test "metadata_tzip16.mligo"; "-e"; entrypoint ];
  [%expect
    {|
    { parameter int ;
      storage (pair (int %data) (big_map %metadata string bytes)) ;
      code { CDR ; NIL operation ; PAIR } } |}];
  let storage =
    "{ data  = 42 ; metadata = Big_map.literal [ (\"toto\", 0x42) ; (\"titi\", 0x24) ] }"
  in
  run_ligo_good
    [ "compile"; "storage"; test "metadata_tzip16.mligo"; "-e"; entrypoint; storage ];
  [%expect {| (Pair 42 { Elt "titi" 0x24 ; Elt "toto" 0x42 }) |}]

let%expect_test _ =
  let entrypoint = "entry_valid_metadata" in
  run_ligo_good [ "compile"; "contract"; test "metadata_tzip16.jsligo"; "-e"; entrypoint ];
  [%expect
    {|
    { parameter int ;
      storage (pair (int %data) (big_map %metadata string bytes)) ;
      code { CDR ; NIL operation ; PAIR } } |}];
  let storage =
    "{ data : 42, metadata : Big_map.literal (list([[\"toto\", 0x42],[\"titi\", 0x24]]))}"
  in
  run_ligo_good
    [ "compile"; "storage"; test "metadata_tzip16.jsligo"; "-e"; entrypoint; storage ];
  [%expect {| (Pair 42 { Elt "titi" 0x24 ; Elt "toto" 0x42 }) |}]

(* -------------------------------------------------------------------------- *)
(* Contracts with invalid 'metadata' should pass when the waiver flag is enabled *)

let%expect_test _ =
  let entrypoint = "entry_invalid_metadata_1" in
  run_ligo_good
    [ "compile"
    ; "contract"
    ; test "metadata_tzip16.mligo"
    ; "--no-metadata-check"
    ; "-e"
    ; entrypoint
    ];
  [%expect
    {|
    { parameter int ;
      storage (pair (int %data) (nat %metadata)) ;
      code { CDR ; NIL operation ; PAIR } } |}];
  let storage = "{ data  = 42; metadata = 33n}" in
  run_ligo_good
    [ "compile"
    ; "storage"
    ; test "metadata_tzip16.mligo"
    ; "--no-metadata-check"
    ; "-e"
    ; entrypoint
    ; storage
    ];
  [%expect {| (Pair 42 33) |}]

let%expect_test _ =
  let entrypoint = "entry_invalid_metadata_1" in
  run_ligo_good
    [ "compile"
    ; "contract"
    ; test "metadata_tzip16.jsligo"
    ; "--no-metadata-check"
    ; "-e"
    ; entrypoint
    ];
  [%expect
    {|
    { parameter int ;
      storage (pair (int %data) (nat %metadata)) ;
      code { CDR ; NIL operation ; PAIR } } |}];
  let storage = "{ data  : 42, metadata : (33 as nat)}" in
  run_ligo_good
    [ "compile"
    ; "storage"
    ; "--no-metadata-check"
    ; test "metadata_tzip16.jsligo"
    ; "-e"
    ; entrypoint
    ; storage
    ];
  [%expect {| (Pair 42 33) |}]
