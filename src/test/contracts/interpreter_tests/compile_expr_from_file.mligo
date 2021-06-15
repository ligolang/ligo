let under_test = "./contract_under_test/just_some_types.mligo"
let under_test2 = "./contract_under_test/just_some_types2.mligo"

let test1 =
  let d_one = Test.compile_value (1 + 3 + 5) in
  let x = Test.compile_expression_subst (Some under_test)
    [%cameligo ({| {one = $one ; two = $two ; three = $three ; four = $four ; five = $five} |} : ligo_program)]
    [
      ("one", d_one );
      ("two", Test.compile_value (1n + 2n)) ;
      ("three", Test.compile_value ("a"^"b")) ;
      ("four", Test.compile_value (0xFF00)) ;
      ("five", Test.compile_value ()) ;
    ]
  in
  let ret = Test.compile_expression_subst (Some under_test)
    [%cameligo ({| f $arg |} : ligo_program)]
    [ ("arg", x) ]
  in
  assert (Test.michelson_equal d_one ret)

let test2 =
  let x1 = Test.compile_value (1,2) in
  let x2 = Test.compile_expression (None: string option) [%cameligo ({| (1,2) |} : ligo_program)] in
  let eq = Test.michelson_equal x1 x2 in
  assert eq

let test3 =
  let x1 = Test.compile_expression (Some under_test)
    [%cameligo ({| (Baz 1n : some_v) |} : ligo_program)]
  in
  let x2 = Test.compile_expression (Some under_test2)
    [%cameligo ({| (Baz 1n : some_v) |} : ligo_program)]
  in
  assert (not (Test.michelson_equal x1 x2))

let test4 =
  let x1 = Test.compile_expression (Some under_test)
    [%cameligo ({| {one = 1 ; two = 2n ; three = "a" ; four = 0xFF00 ; five = ()} |} : ligo_program)]
  in
  let x2 = Test.compile_expression (Some under_test2)
    [%cameligo ({| {one = 1 ; two = 2n ; three = "a" ; four = 0xFF00 ; five = ()} |} : ligo_program)]
  in
  assert (not (Test.michelson_equal x1 x2))
