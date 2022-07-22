open Simple_utils.Display

let stage = "self_ast_aggregated"

type self_ast_aggregated_error = [
  | `Self_ast_aggregated_expected_obj_ligo of Location.t
  | `Self_ast_aggregated_polymorphism_unresolved of Location.t
  | `Self_ast_aggregated_fvs_in_create_contract_lambda of Ast_aggregated.expression * Ast_aggregated.ValueVar.t
  | `Self_ast_aggregated_create_contract_lambda of Ast_aggregated.constant' * Ast_aggregated.expression
  | `Self_ast_aggregated_bad_self_type of Ast_aggregated.type_expression * Ast_aggregated.type_expression * Location.t
  | `Self_ast_aggregated_bad_format_entrypoint_ann of string * Location.t
  | `Self_ast_aggregated_entrypoint_ann_not_literal of Location.t
  | `Self_ast_aggregated_unmatched_entrypoint of Location.t
  | `Self_ast_aggregated_corner_case of string
] [@@deriving poly_constructor { prefix = "self_ast_aggregated_" }]

let error_ppformat : display_format:string display_format ->
  Format.formatter -> self_ast_aggregated_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Self_ast_aggregated_expected_obj_ligo loc ->
      Format.fprintf f
        "@[<hv>%a@.Invalid call to Test primitive.@]"
        Snippet.pp loc
    | `Self_ast_aggregated_polymorphism_unresolved loc ->
      Format.fprintf f
        "@[<hv>%a@.Can't infer the type of this value, please add a type annotation.@]"
        Snippet.pp loc
    | `Self_ast_aggregated_fvs_in_create_contract_lambda (e,v) ->
      Format.fprintf f
        "@[<hv>%a@.Free variable usage is not allowed in call to Tezos.create_contract:@.%a@]"
        Snippet.pp e.location
        Snippet.pp (Ast_aggregated.ValueVar.get_location v)
    | `Self_ast_aggregated_create_contract_lambda (_cst,e) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid usage of Tezos.create_contract.@.The first argument must be an inline function. @]"
        Snippet.pp e.location
    | `Self_ast_aggregated_bad_self_type (expected,got,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid type annotation.@.\"%a\" was given, but \"%a\" was expected.@.Note that \"Tezos.self\" refers to this contract, so the parameters should be the same. @]"
        Snippet.pp loc
        Ast_aggregated.PP.type_expression got
        Ast_aggregated.PP.type_expression expected
    | `Self_ast_aggregated_bad_format_entrypoint_ann (ep,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid entrypoint \"%s\". One of the following patterns is expected:@.* \"%%bar\" is expected for entrypoint \"Bar\"@.* \"%%default\" when no entrypoint is used."
        Snippet.pp loc
        ep
    | `Self_ast_aggregated_entrypoint_ann_not_literal loc ->
      Format.fprintf f
        "@[<hv>%a@.Invalid entrypoint value.@.The entrypoint value must be a string literal. @]"
        Snippet.pp loc
    | `Self_ast_aggregated_unmatched_entrypoint loc ->
      Format.fprintf f
        "@[<hv>%a@.Invalid entrypoint value.@.The entrypoint value does not match a constructor of the contract parameter. @]"
        Snippet.pp loc
    | `Self_ast_aggregated_corner_case desc ->
      Format.fprintf f
        "@[<hv>Internal error: %s @]"
        desc
  )

let error_jsonformat : self_ast_aggregated_error -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Self_ast_aggregated_expected_obj_ligo loc ->
    let message = `String "unexpected Test primitive" in
    let description = `String "these Test primitive or type cannot be used in code to be compiled or run" in
    let content = `Assoc [
       ("message", message);
       ("location", Location.to_yojson loc);
       ("description", description);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_aggregated_polymorphism_unresolved loc ->
    let message = `String "unexpected polymorphism" in
    let description = `String "Can't infer the type of this value, please add a type annotation" in
    let content = `Assoc [
       ("message", message);
       ("location", Location.to_yojson loc);
       ("description", description);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_aggregated_fvs_in_create_contract_lambda (e, v) ->
    let loc = Ast_aggregated.ValueVar.get_location v in
    let message = `String "Free variables are not allowed in CREATE_CONTRACT lambdas" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let expression = `String (Format.asprintf "%a" Ast_aggregated.PP.expression e) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("expression", expression);
    ] in
    json_error ~stage ~content
  | `Self_ast_aggregated_create_contract_lambda (cst,e) ->
    let message = `String (Format.asprintf "First argument of %a must be inlined using a lambda" Ast_aggregated.PP.constant' cst) in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let expression = `String (Format.asprintf "%a" Ast_aggregated.PP.expression e) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("expression", expression);
    ] in
    json_error ~stage ~content
  | `Self_ast_aggregated_bad_self_type (expected,got,loc) ->
    let message = `String "bad self type" in
    let expected = `String (Format.asprintf "%a" Ast_aggregated.PP.type_expression expected) in
    let actual = `String (Format.asprintf "%a" Ast_aggregated.PP.type_expression got) in
    let content = `Assoc [
       ("message", message);
       ("location", Location.to_yojson loc);
       ("expected", expected);
       ("actual", actual);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_aggregated_bad_format_entrypoint_ann (ep,loc) ->
    let message = `String "bad entrypoint format" in
    let entrypoint = `String ep in
    let hint = `String "we expect '%%bar' for entrypoint Bar and '%%default' when no entrypoint used" in
    let content = `Assoc [
       ("message", message);
       ("location", Location.to_yojson loc);
       ("hint", hint);
       ("entrypoint", entrypoint);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_aggregated_entrypoint_ann_not_literal loc ->
    let message = `String "entrypoint annotation must be a string literal" in
    let content = `Assoc [
       ("message", message);
       ("location", Location.to_yojson loc);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_aggregated_unmatched_entrypoint loc ->
    let message = `String "no constructor matches the entrypoint annotation" in
    let content = `Assoc [
       ("message", message);
       ("location", Location.to_yojson loc);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_aggregated_corner_case desc ->
    let message = `String "internal error" in
    let description = `String desc in
    let content = `Assoc [
       ("message", message);
       ("description", description);
       ]
    in
    json_error ~stage ~content
