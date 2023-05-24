type format_test =
  { test_name : string
  ; actual : string (** Path to file with unformatted contents. *)
  ; expected : string (** Path to file with formatted contents. *)
  }

let parsing_error_to_string (err : Parsing.Errors.t) : string =
  let ({ content = { message; _ }; _ } : Simple_utils.Error.t) =
    Parsing.Errors.error_json err
  in
  message


type parsing_raise = (Parsing.Errors.t, Main_warnings.all) Simple_utils.Trace.raise

let test { test_name; actual; expected } =
  Alcotest.test_case test_name `Quick
  @@ fun () ->
  let syntax =
    match Caml.Filename.extension actual with
    | ".mligo" -> Syntax_types.CameLIGO
    | ".jsligo" -> Syntax_types.JsLIGO
    | ".ligo" | ".pligo" -> Syntax_types.PascaLIGO
    | other -> failwith (Printf.sprintf "Unknown extension %s" other)
  in
  let contents = In_channel.read_all actual in
  let buffer = Buffer.create (String.length contents) in
  let () = Buffer.add_string buffer contents in
  let raise : parsing_raise =
    { error = (fun err -> failwith (parsing_error_to_string err))
    ; warning = Fun.const ()
    ; log_error = Fun.const ()
    ; fast_fail = false
    }
  in
  let preprocess = false in
  let formatted_contents =
    Parsing.(
      match syntax with
      | CameLIGO ->
        Cameligo.(pretty_print_file ~preprocess ~raise Pretty.default_state buffer actual)
      | JsLIGO ->
        Jsligo.(pretty_print_file ~preprocess ~raise Pretty.default_state buffer actual)
      | PascaLIGO ->
        Pascaligo.(
          pretty_print_file ~preprocess ~raise Pretty.default_state buffer actual))
  in
  Alcotest.(check string)
    "Formatted contents of the files should be equal"
    (In_channel.read_all expected)
    (Buffer.contents formatted_contents)


let test_cases =
  [ { test_name = "namespace with types"
    ; actual = "contracts/formatter/namespace_types.jsligo"
    ; expected = "contracts/formatter/namespace_types_formatted.jsligo"
    }
  ; { test_name = "simple namespace"
    ; actual = "contracts/formatter/simple_namespace.jsligo"
    ; expected = "contracts/formatter/simple_namespace_formatted.jsligo"
    }
  ; { test_name = "function with statements"
    ; actual = "contracts/formatter/statements.jsligo"
    ; expected = "contracts/formatter/statements_formatted.jsligo"
    }
  ; { test_name = "function with one statement"
    ; actual = "contracts/formatter/one_statement.jsligo"
    ; expected = "contracts/formatter/one_statement_formatted.jsligo"
    }
  ; { test_name = "function with one expression"
    ; actual = "contracts/formatter/one_expression.jsligo"
    ; expected = "contracts/formatter/one_expression_formatted.jsligo"
    }
  ; { test_name = "function with two statements"
    ; actual = "contracts/formatter/two_statements.jsligo"
    ; expected = "contracts/formatter/two_statements_formatted.jsligo"
    }
  ; { test_name = "function with long name and arguments"
    ; actual = "contracts/formatter/long_arguments.jsligo"
    ; expected = "contracts/formatter/long_arguments_formatted.jsligo"
    }
  ; { test_name = "function with long name and no arguments"
    ; actual = "contracts/formatter/no_arguments.jsligo"
    ; expected = "contracts/formatter/no_arguments_formatted.jsligo"
    }
  ; { test_name = "flat sum type"
    ; actual = "contracts/formatter/flat_sum.jsligo"
    ; expected = "contracts/formatter/flat_sum_formatted.jsligo"
    }
  ; { test_name = "non-flat sum type"
    ; actual = "contracts/formatter/non_flat_sum.jsligo"
    ; expected = "contracts/formatter/non_flat_sum_formatted.jsligo"
    }
  ; { test_name = "non-flat disc type"
    ; actual = "contracts/formatter/non_flat_disc.jsligo"
    ; expected = "contracts/formatter/non_flat_disc_formatted.jsligo"
    }
  ; { test_name = "long curried function"
    ; actual = "contracts/formatter/curried.jsligo"
    ; expected = "contracts/formatter/curried_formatted.jsligo"
    }
  ; { test_name = "various lists"
    ; actual = "contracts/formatter/lists.jsligo"
    ; expected = "contracts/formatter/lists_formatted.jsligo"
    }
  ; { test_name = "lambda with type variables (regression test)"
    ; actual = "contracts/formatter/lambda_type_vars.jsligo"
    ; expected = "contracts/formatter/lambda_type_vars_formatted.jsligo"
    }
  ]


let _main =
  Printexc.record_backtrace true;
  Alcotest.run
    "pretty_printer_tests"
    [ "formatted matches expected", List.map ~f:test test_cases ]
