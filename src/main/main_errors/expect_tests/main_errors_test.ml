open Main_errors
module Region  = Simple_utils.Region
module Pos     = Simple_utils.Pos
module Display = Simple_utils.Display
module Michelson = Tezos_utils.Michelson
module Location = Simple_utils.Location
open Region

let byte =
  Lexing.
    {pos_fname= "a dummy file name"; pos_lnum= 20; pos_bol= 25; pos_cnum= 30}

let default_location =
  Region.make
    ~start:(Pos.make ~byte ~point_num:10 ~point_bol:10)
    ~stop:(Pos.make ~byte ~point_num:20 ~point_bol:10)

let default_region1 =
  Region.make
    ~start:(Pos.make ~byte ~point_num:10 ~point_bol:10)
    ~stop:(Pos.make ~byte ~point_num:20 ~point_bol:10)

let error display_format error =
  let buffer = Buffer.create 100 in
  let formatter = Format.formatter_of_buffer buffer in
  Formatter.error_ppformat ~display_format formatter error ;
  Format.pp_print_flush formatter () ;
  print_string (Buffer.contents buffer)

let human_readable_error e =
  let display_format = Display.human_readable in
  let display_format =
    match display_format with
    | Ex_display_format (Human_readable as a) ->
        a
    | _ ->
        assert false
  in
  error display_format e

let%expect_test "main" =
  human_readable_error (`Main_invalid_syntax_name "foo") ;
  [%expect
    {|
      Invalid syntax option: 'foo'.
      Use 'pascaligo', 'cameligo', 'reasonligo', or 'jsligo'.|}] ;
    human_readable_error (`Main_invalid_dialect_name "foo") ;
  [%expect
    {|
      Invalid dialect option: 'foo'.
      Use 'verbose' or 'terse'.|}] ;
  human_readable_error (`Main_invalid_extension "foo") ;
  [%expect
    {|
  Invalid file extension 'foo'.
  Use '.ligo' for PascaLIGO, '.mligo' for CameLIGO, '.religo' for ReasonLIGO, '.jsligo' for JsLIGO, or the --syntax option.|}] ;
  human_readable_error
    (`Main_unparse_tracer
      [`Tezos_alpha_error Tezos_error_monad.Error_monad.Timeout]) ;
  [%expect {|
    Error(s) occurred while translating to Michelson:
    The request has timed out|}] ;
  human_readable_error
    (`Main_typecheck_contract_tracer
      ( let open Tezos_micheline.Micheline in
        root (strip_locations Michelson.t_unit),
        [`Tezos_alpha_error Tezos_error_monad.Error_monad.Canceled] )) ;
  [%expect {|
  Error(s) occurred while type checking the contract:
  The promise was unexpectedly canceled
  |}] ;
  human_readable_error
    (`Main_could_not_serialize [`Tezos_alpha_error Tezos_error_monad.Error_monad.Canceled]) ;
  [%expect {|
    Error(s) occurred while serializing Michelson code:
    The promise was unexpectedly canceled|}] ;
  human_readable_error `Main_unknown ;
  [%expect {|An unknown error occurred.|}];
  human_readable_error `Main_entrypoint_not_a_function ;
  [%expect {|
    Invalid command line argument.
    The provided entrypoint is not a function.|}] ;
  human_readable_error `Main_entrypoint_not_found ;
  [%expect {|
    Invalid command line argument.
    The provided entrypoint is not found in the contract.|}] ;
  human_readable_error (`Main_invalid_amount "foo") ;
  [%expect {|
    Invalid command line option "--amount".
    The provided amount "foo" is invalid. Use an integer instead.|}] ;
  human_readable_error (`Main_invalid_balance "foo") ;
  [%expect {|
    Invalid command line option "--balance".
    The provided balance "foo" is invalid. Use an integer instead.|}] ;
  human_readable_error (`Main_invalid_source "foo") ;
  [%expect {|
    Invalid command line option "--source".
    The provided source address "foo" is invalid. A valid Tezos address is a string prefixed by either tz1, tz2, tz3 or KT1 and followed by a Base58 encoded hash and terminated by a 4-byte checksum.|}];
  human_readable_error (`Main_invalid_sender "foo") ;
  [%expect {|
    Invalid command line option "--sender".
    The provided sender address "foo" is invalid. A valid Tezos address is a string prefixed by either tz1, tz2, tz3 or KT1 and followed by a Base58 encoded hash and terminated by a 4-byte checksum.|}]

let%expect_test _ =
  human_readable_error (unparsing_michelson_tracer [`Tezos_alpha_error Tezos_error_monad.Error_monad.Timeout; `Tezos_alpha_error Tezos_error_monad.Error_monad.Canceled]) ;
  [%expect {|
  Error(s) occurred while unparsing the Michelson result:
  The request has timed out
  The promise was unexpectedly canceled
  |}] ;
  human_readable_error (parsing_input_tracer [`Tezos_alpha_error Tezos_error_monad.Error_monad.Timeout; `Tezos_alpha_error Tezos_error_monad.Error_monad.Canceled]) ;
  [%expect {|
  Error(s) occurred while parsing the Michelson input:
  The request has timed out
  The promise was unexpectedly canceled
  |}] ;
  human_readable_error (parsing_code_tracer [`Tezos_alpha_error Tezos_error_monad.Error_monad.Timeout; `Tezos_alpha_error Tezos_error_monad.Error_monad.Canceled]) ;
  [%expect {|
  Error(s) occurred while checking the contract:
  The request has timed out
  The promise was unexpectedly canceled
  |}] ;
  human_readable_error (error_of_execution_tracer [`Tezos_alpha_error Tezos_error_monad.Error_monad.Timeout; `Tezos_alpha_error Tezos_error_monad.Error_monad.Canceled]) ;
  [%expect {|
  Error(s) occurred while executing the contract:
  The request has timed out
  The promise was unexpectedly canceled
  |}]

let%expect_test "pretty" = () (* not used *)

let%expect_test "self_ast_imperative" =
  let open Ast_imperative in
  let open Location in
  let error e = human_readable_error (self_ast_imperative_tracer e) in
  let location_t = File default_location in
  let type_content = T_variable (TypeVar.of_input_var "foo") in
  let type_expression = {type_content; location= location_t} in
  let expression_content = E_skip in
  let expression = {expression_content; location= location_t} in
  error (`Self_ast_imperative_long_constructor ("foo", type_expression)) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:

  Ill-formed data constructor "foo".
  Data constructors have a maximum length of 32 characters, which is a limitation imposed by annotations in Tezos.
  |}] ;
  error (`Self_ast_imperative_bad_timestamp ("bar", expression)) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:
   Ill-formed timestamp "bar".
  At this point, a string with a RFC3339 notation or the number of seconds since Epoch is expected.
  |}] ;
  error (`Self_ast_imperative_bad_format_literal expression) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:
   Ill-formed literal "skip".
  In the case of an address, a string is expected prefixed by either tz1, tz2, tz3 or KT1 and followed by a Base58 encoded hash and terminated by a 4-byte checksum.
  In the case of a key_hash, signature, or key a Base58 encoded hash is expected.
  |}] ;
  error (`Self_ast_imperative_bad_empty_arity (C_UNIT, expression)) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:

  Ill-formed "UNIT" expression.
  No functions arguments are expected.
  |}] ;
  error (`Self_ast_imperative_bad_single_arity (C_BYTES_UNPACK, expression)) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:

  Ill-formed "BYTES_UNPACK" expression
  One function argument is expected.
  |}] ;
  error (`Self_ast_imperative_bad_map_param_type (C_ADD, expression)) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:
   Ill-formed "ADD" expression.
  A list of pair parameters is expected.
  |}] ;
  error (`Self_ast_imperative_bad_set_param_type (C_ITER, expression)) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:

  Ill-formed "ITER" expression.
  A list of pair parameters is expected.
  |}] ;
  error (`Self_ast_imperative_bad_conversion_bytes expression) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:
   Ill-formed bytes literal.
  Example of a valid bytes literal: "ff7a7aff".
  |}]

let%expect_test _ =
  human_readable_error (purification_tracer [`Purification_corner_case "foo"]) ;
  [%expect {|Corner case: foo|}] ;
  human_readable_error (depurification_tracer (`Purification_corner_case "foo")) ;
  [%expect {| |}]

let%expect_test "desugaring" = ()

let%expect_test "sugaring" = ()

let%expect_test "main_cit_pascaligo" =
  let open Cst.Pascaligo in
  let open Location in
  let error e = human_readable_error (cit_pascaligo_tracer e) in
  let pvar = P_Var (Wrap.wrap "foo" default_region1) in
  let type_expr = T_String (Wrap.wrap "yolo" default_region1) in
  let location_t = File default_location in
  error [`Concrete_pascaligo_unsupported_pattern_type pvar] ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid case pattern.
      Can't match on values.|}] ;
  error [`Concrete_pascaligo_unsupported_string_singleton type_expr] ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid type.
      It's not possible to assign a string to a type.|}] ;
  error [`Concrete_pascaligo_michelson_type_wrong (type_expr, "foo")] ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:

  Invalid "foo" type.
  At this point, an annotation, in the form of a string, is expected for the preceding type.
  |}] ;
  error [`Concrete_pascaligo_michelson_type_wrong_arity (location_t, "zzz")] ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid "zzz" type.
      An even number of 2 or more arguments is expected, where each odd item is a type annotated by the following string.|}] ;
  error [`Concrete_pascaligo_untyped_recursive_fun location_t] ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:

  Invalid function declaration.
  Recursive functions are required to have a type annotation (for now).
  |}]

let%expect_test "main_cit_cameligo" =
  let open Cst.Cameligo in
  let open Location in
  let error e = human_readable_error (cit_cameligo_tracer e) in
  let variable = {value= "dog"; region= default_region1} in
  let pvar = PVar {value = { variable ; attributes = []} ; region = default_region1} in
  let type_expr = TVar {value= "dog"; region= default_region1} in
  let location_t = File default_location in
  error [`Concrete_cameligo_untyped_recursive_fun default_region1] ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid function declaration.
      Recursive functions are required to have a type annotation (for now).|}] ;
  error [`Concrete_cameligo_unsupported_pattern_type [pvar]] ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid pattern.
      Can't match on values.|}] ;
  error [`Concrete_cameligo_unsupported_string_singleton type_expr] ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid type.
      It's not possible to assign a string to a type.|}] ;
  error [`Concrete_cameligo_recursion_on_non_function location_t] ;
  [%expect {|
    File "a dummy file name", line 20, character 5:

    Invalid let declaration.
    Only functions can be recursive.|}] ;
  error [`Concrete_cameligo_michelson_type_wrong (type_expr, "foo")] ;
  [%expect
    {|
File "a dummy file name", line 20, character 5:

Invalid "foo" type.
At this point, an annotation, in the form of a string, is expected for the preceding type.
|}] ;
  error [`Concrete_cameligo_michelson_type_wrong_arity (location_t, "zzz")] ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid "zzz" type.
      An even number of 2 or more arguments is expected, where each odd item is a type annotated by the following string.|}];
  error [`Concrete_cameligo_missing_funarg_annotation variable];
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Missing a type annotation for argument "dog".|}]

let%expect_test "main_cit_reasonligo" =
  let open Cst.Reasonligo in
  let open Location in
  let error e = human_readable_error (cit_reasonligo_tracer e) in
  let variable = {value= "dog"; region= default_region1} in
  let pvar = PVar  {value= {variable; attributes = []}; region= default_region1} in
  let type_expr = TVar {value= "dog"; region= default_region1} in
  let location_t = File default_location in
  error [`Concrete_reasonligo_untyped_recursive_fun location_t] ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid function declaration.
      Recursive functions are required to have a type annotation (for now).|}] ;
  error [`Concrete_reasonligo_unsupported_pattern_type pvar] ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid pattern matching.
      Can't match on values.|}] ;
  error [`Concrete_reasonligo_unsupported_string_singleton type_expr] ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid type.
      It's not possible to assign a string to a type.|}] ;
  error [`Concrete_reasonligo_recursion_on_non_function location_t] ;
  [%expect {|
    File "a dummy file name", line 20, character 5:

    Invalid let declaration.
    Only functions can be recursive.|}] ;
  error [`Concrete_reasonligo_michelson_type_wrong (Location.lift default_region1, "foo")] ;
  [%expect
    {|
        File "a dummy file name", line 20, character 5:

        Invalid "foo" type.
        At this point, an annotation, in the form of a string, is expected for the preceding type.|}] ;
  error [`Concrete_reasonligo_michelson_type_wrong_arity (location_t, "bar")] ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid "bar" type.
      An even number of 2 or more arguments is expected, where each odd item is a type annotated by the following string.|}]


let%expect_test "typer" =
  let open Ast_typed in
  let open Location in
  let error e = human_readable_error (checking_tracer e) in
  let location_t = File default_location in
  let type_variable = TypeVar.of_input_var "foo" in
  let expression_variable = ValueVar.of_input_var "bar" in
  let ast_core_expression_variable : Ast_core.expression_variable = ValueVar.of_input_var "bar"
  in
  let ast_core_expression_content : Ast_core.expression_content =
    E_variable ast_core_expression_variable
  in
  let ast_core_expression : Ast_core.expression =
    {expression_content= ast_core_expression_content; sugar= None; location= location_t}
  in
  let type_expression : Ast_typed.type_expression =
    { type_content= T_variable (TypeVar.of_input_var "foo");
      type_meta= None;
      orig_var = None ;
      location= File default_location }
  in
  let type_expression2 : Ast_typed.type_expression =
    { type_content= T_variable (TypeVar.of_input_var "bar");
      type_meta= None;
      orig_var = None ;
      location= File default_location }
  in
  let expression_content = E_literal Literal_unit in
  let expression =
    {expression_content; location= location_t; type_expression}
  in
  error (`Typer_unbound_type_variable (type_variable, location_t)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Type "foo" not found.|}] ;
  error
    (`Typer_unbound_variable (expression_variable, location_t)) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Variable "bar" not found.|}] ;
  error (`Typer_match_missing_case ([Label "Foo"; Label "Bar"; Label "Cat"], [Label "Bar"], location_t)) ;
  [%expect {|
  File "a dummy file name", line 20, character 5:

  Pattern matching is not exhaustive.
  Cases that are missing: Cat, Foo.|}] ;
  error (`Typer_match_extra_case ([Label "Foo"], [Label "Foo"; Label "Foo"; Label "Bar"], location_t)) ;
  [%expect {|
    File "a dummy file name", line 20, character 5:

    Pattern matching over too many cases.
    These case(s) are duplicate:
    Foo
    These case(s) don't belong to the variant:
    Bar
    Please remove the extra cases.|}] ;
  error
    (`Typer_unbound_constructor (Label "Some-Constructor", location_t)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Constructor "Some-Constructor" not found.|}] ;
  error
    (`Typer_michelson_or_no_annotation (Label "Some-Constructor", location_t)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Incorrect usage of type "michelson_or".
    The contructor "Some-Constructor" must be annotated with a variant type.|}] ;
  error
    (`Typer_should_be_a_function_type (type_expression, ast_core_expression)) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:

  Invalid type.
  Expected a function type, but got "foo".|}] ;
  error
    (`Typer_bad_record_access
      (Label "bar", expression, location_t)) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:

  Invalid record field "bar" in record "unit".|}] ;
  error
    (`Typer_expression_tracer
      ( ast_core_expression,
        `Typer_bad_record_access
          (Label "bar", expression, location_t) )) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:

  Invalid record field "bar" in record "unit".|}] ;
  error (`Typer_assert_equal (location_t, type_expression, type_expression2)) ;
  [%expect {|
    File "a dummy file name", line 20, character 5:

    Invalid type(s).
    Expected: "foo", but got: "bar".|}] ;
  error (`Typer_corner_case "foo") ;
  [%expect {|
    A type system corner case occurred:
    foo|}] ;
  error (`Typer_bad_collect_loop (type_expression, location_t)) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Bounded loop over a value with an incorrect type.
      Expected a value with type: "list", "set" or "map", but got a value of type "foo".|}] ;
  error (`Typer_expected_record (location_t, type_expression)) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid argument.
      Expected a record, but got an argument of type "foo".|}] ;
  error (`Typer_expected_variant (location_t, type_expression)) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid argument.
      Expected a variant, but got an argument of type "foo".|}] ;
  error
    (`Typer_wrong_param_number
      (location_t, "foo", 10, [type_expression; type_expression2])) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Function "foo" called with wrong number of arguments.
      Expected 10 arguments, got 2 arguments.|}] ;
  error (`Typer_expected_option (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Incorrect argument.
    Expected an option, but got an argument of type "foo".|}] ;
  error (`Typer_not_matching (location_t, type_expression, type_expression2)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    These types are not matching:
     - foo
     - bar|}] ;
  error (`Typer_not_annotated location_t);
  [%expect {|
    File "a dummy file name", line 20, character 5:

    Can't infer the type of this value, please add a type annotation.|}] ;
  error (`Typer_typeclass_error (location_t, [[type_expression; type_expression2 ]], [type_expression2])) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid arguments.
    Expected an argument of type (foo, bar), but got an argument of type bar.|}] ;
  error (`Typer_uncomparable_types (location_t, type_expression, type_expression2)) ;
  [%expect {|
    File "a dummy file name", line 20, character 5:

    Invalid arguments.
    These types cannot be compared: "foo" and "bar".|}] ;
  error (`Typer_comparator_composed (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid arguments.
    Only composed types of not more than two element are allowed to be compared.|}]

let%expect_test "interpreter" = ()

let%expect_test "self_ast_typed" =
  let open Ast_typed in
  let open Location in
  let error e = human_readable_error (self_ast_typed_tracer e) in
  let expression_variable = ValueVar.of_input_var "bar" in
  let location_t = File default_location in
  let type_expression : Ast_typed.type_expression =
    { type_content= T_variable (TypeVar.of_input_var "foo");
      type_meta= None ;
      orig_var = None ;
      location= File default_location }
  in
  let type_expression2 : Ast_typed.type_expression =
    { type_content= T_variable (TypeVar.of_input_var "bar");
      type_meta= None ;
      orig_var = None ;
      location= File default_location }
  in
  let expression_content = E_literal Literal_unit in
  let expression =
    {expression_content; location= location_t; type_expression}
  in
  error (`Self_ast_typed_recursive_call_is_only_allowed_as_the_last_operation (expression_variable, location_t)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Recursive call not in tail position.
    The value of a recursive call must be immediately returned by the defined function.|}] ;
  error
    (`Self_ast_typed_bad_self_type
      (type_expression, type_expression2, location_t)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid type annotation.
    "bar" was given, but "foo" was expected.
    Note that "Tezos.self" refers to this contract, so the parameters should be the same. |}] ;
  error (`Self_ast_typed_bad_format_entrypoint_ann ("foo", location_t)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid entrypoint "foo". One of the following patterns is expected:
    * "%bar" is expected for entrypoint "Bar"
    * "%default" when no entrypoint is used. |}] ;
  error (`Self_ast_typed_entrypoint_ann_not_literal location_t) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid entrypoint value.
    The entrypoint value must be a string literal. |}] ;
  error (`Self_ast_typed_unmatched_entrypoint location_t) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid entrypoint value.
    The entrypoint value does not match a constructor of the contract parameter. |}] ;
  error (`Self_ast_typed_nested_bigmap location_t) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid big map nesting.
    A big map cannot be nested inside another big map. |}] ;
  error (`Self_ast_typed_corner_case "foo") ;
  [%expect {|
    Internal error: foo |}] ;
  error (`Self_ast_typed_bad_contract_io (ValueVar.of_input_var "foo", expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid type for entrypoint "foo".
    An entrypoint must of type "parameter * storage -> operation list * storage". |}] ;
  error
    (`Self_ast_typed_expected_list_operation (ValueVar.of_input_var "foo", type_expression, expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid type for entrypoint "foo".
    An entrypoint must of type "parameter * storage -> operation list * storage".
    We expected a list of operations but we got foo |}] ;
  error
    (`Self_ast_typed_expected_same_entry
      (ValueVar.of_input_var "foo", type_expression, type_expression2, expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid type for entrypoint "foo".
    The storage type "foo" of the function parameter must be the same as the storage type "bar" of the return value. |}] ;
  error (`Self_ast_typed_expected_pair_in (location_t, `Contract)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid contract.
    Expected a tuple as argument. |}] ;
  error (`Self_ast_typed_expected_pair_out location_t) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid entrypoint.
    Expected a tuple of operations and storage as return value. |}]

let%expect_test "self_mini_c" =
  let error e = human_readable_error (self_mini_c_tracer e) in
  error (`Self_mini_c_bad_self_address C_SELF_ADDRESS) ;
  [%expect {|"Tezos.self" must be used directly and cannot be used via another function. |}] ;
  error `Self_mini_c_not_a_function ;
  [%expect {|
    Invalid type for entrypoint.
    An entrypoint must of type "parameter * storage -> operation list * storage". |}] ;
  error `Self_mini_c_could_not_aggregate_entry ;
  [%expect {|
    Invalid type for entrypoint.
    An entrypoint must of type "parameter * storage -> operation list * storage". |}]

let%expect_test "spilling" =
  let error (e:Spilling.Errors.spilling_error) = human_readable_error (spilling_tracer e) in
  let open Location in
  let type_variable : Ast_typed.type_variable = Ast_typed.TypeVar.of_input_var "foo" in
  let location_t = File default_location in
  let expression_variable = Ast_aggregated.ValueVar.of_input_var "bar" in
  let type_expression : Ast_aggregated.type_expression =
    { type_content= T_variable (Ast_aggregated.TypeVar.of_input_var "foo");
      orig_var = None ;
      location= File default_location;
      source_type = None }
  in
  let value = Mini_c.D_none in
  error (`Spilling_corner_case ("foo", "bar")) ;
  [%expect
    {|
    foo
     corner case: bar
    Sorry, we don't have a proper error message for this error. Please report this use case so we can improve on this. |}] ;
  error (`Spilling_no_type_variable type_variable) ;
  [%expect{| Type "foo" not found (should not happen and be caught earlier). |}] ;
  error (`Spilling_unsupported_pattern_matching location_t) ;
  [%expect{|
    File "a dummy file name", line 20, character 5:

    Invalid pattern matching.@Tuple patterns are not (yet) supported. |}] ;
  error (`Spilling_unsupported_recursive_function (location_t, expression_variable)) ;
  [%expect{|
    File "a dummy file name", line 20, character 5:

    Invalid recursive function "bar".
    A recursive function can only have one argument. |}] ;
  error (`Spilling_wrong_mini_c_value (type_expression, value)) ;
  [%expect{|
    Invalid type.
    Expected "foo",
    but got "None". |}] ;
  error (`Spilling_bad_decompile value) ;
  [%expect{| Cannot untranspile: None |}]

let%expect_test "scoping" =
  let open Mini_c in
  let error e = human_readable_error (scoping_tracer e) in
  error (`Scoping_corner_case ("foo", "bar")) ;
  [%expect
    {|
      Scoping corner case at foo : bar.
      Sorry, we don't have a proper error message for this error. Please report this use case so we can improve on this. |}] ;
  error (`Scoping_contract_entrypoint "xxx") ;
  [%expect {|contract entrypoint must be given as a literal string: xxx |}] ;
  error (`Scoping_bad_iterator C_UNIT) ;
  [%expect {|bad iterator: iter UNIT |}] ;
  error `Scoping_not_comparable_pair_struct ;
  [%expect
    {|Invalid comparable value. When using a tuple with more than 2 components, structure the tuple like this: "(a, (b, c))". |}]

let%expect_test "stacking" =
  let open Mini_c in
  let error e = human_readable_error (stacking_tracer e) in
  error (`Stacking_corner_case ("foo", "bar")) ;
  [%expect
    {|
      Stacking corner case at foo : bar.
      Sorry, we don't have a proper error message for this error. Please report this use case so we can improve on this. |}] ;
  error (`Stacking_contract_entrypoint "xxx") ;
  [%expect {|contract entrypoint must be given as a literal string: xxx |}] ;
  error (`Stacking_bad_iterator C_UNIT) ;
  [%expect {|bad iterator: iter UNIT |}] ;
  error `Stacking_not_comparable_pair_struct ;
  [%expect
    {|Invalid comparable value. When using a tuple with more than 2 components, structure the tuple like this: "(a, (b, c))". |}]


let%expect_test "decompile_michelson" = () (* same as scoping *)

let%expect_test "decompile_mini_c" = ()  (* same as spilling *)

let%expect_test "decompile_typed" = () (* same as typer *)
