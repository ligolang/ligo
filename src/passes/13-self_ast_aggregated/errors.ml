open Simple_utils.Display

let stage = "self_ast_aggregated"

type self_ast_aggregated_error = [
  | `Self_ast_aggregated_expected_obj_ligo of Location.t
  | `Self_ast_aggregated_polymorphism_unresolved of Location.t
  | `Self_ast_aggregated_fvs_in_create_contract_lambda of Ast_aggregated.expression * Ast_aggregated.ValueVar.t
  | `Self_ast_aggregated_create_contract_lambda of Ast_aggregated.constant' * Ast_aggregated.expression
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
        "@[<hv>%a@.Polymorphism not resolved before recursion.@]"
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
    let message = `String "unexpected polymorphism in recursion" in
    let description = `String "polymorphism should be resolved before recursion" in
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
