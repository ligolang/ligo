open Cli_expect

let%expect_test "inhabit int singleton" =
  run_ligo_good
    [ "compile"; "expression"; "jsligo"; {|
    do {
      const a : 1 = 1;
    }
    |} ];
  [%expect {|
             Unit
             |}]

let%expect_test "cast int singleton" =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; {|
               do {
                 const a : 1 = 1;
                 return a + 2;
               }
               |}
    ];
  [%expect {|
                        3
                        |}]

let%expect_test "inhabit nat singleton" =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; {|
                          do {
                            const a : 1n = 1n;
                          }
                          |}
    ];
  [%expect
    {|
                                   Unit
                                   |}]

let%expect_test "cast nat singleton" =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; {|
                                     do {
                                       const a : 1n = 1n;
                                       return a + 2n;
                                     }
                                     |}
    ];
  [%expect
    {|
                                              3
                                              |}]

let%expect_test "inhabit string singleton" =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; {|
    do {
      const a : "a" = "a";
    }
    |}
    ];
  [%expect {|
             Unit
             |}]

let%expect_test "cast string singleton" =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; {|
               do {
                 const a : "a" = "a";
                 return a + "b";
               }
               |}
    ];
  [%expect {|
                        "ab"
                        |}]

let%expect_test "wrong singleton type 1" =
  run_ligo_bad
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; {|
    do {
      const a : 1 = "a";
    }
    |}
    ];
  [%expect
    {|
             Invalid type(s).
             Expected "1", but got: ""a"".
             |}]

let%expect_test "wrong singleton type 2" =
  run_ligo_bad
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; {|
             do {
               const a : "a" = 1;
             }
             |}
    ];
  [%expect
    {|
                      Invalid type(s).
                      Expected ""a"", but got: "1".
                    
                      |}]

let%expect_test "wrong singleton type 3" =
  run_ligo_bad
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; {|
                      do {
                        const a : 1 = 2;
                      }
                      |}
    ];
  [%expect {|
    Invalid type(s).
    Expected "1", but got: "2". |}]
