let%expect_test _ =
  Format.printf "%a" Literal_value.pp (Literal_bytes (Bytes.of_string "foo")) ;
  [%expect{| 0x666f6f |}]
