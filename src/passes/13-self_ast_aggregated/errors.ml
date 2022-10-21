open Simple_utils.Display
open Ligo_prim

let stage = "self_ast_aggregated"

type self_ast_aggregated_error = [
  | `Self_ast_aggregated_expected_obj_ligo of Location.t
  | `Self_ast_aggregated_polymorphism_unresolved of Location.t
  | `Self_ast_aggregated_fvs_in_create_contract_lambda of Ast_aggregated.expression * Value_var.t
  | `Self_ast_aggregated_create_contract_lambda of Constant.constant' * Ast_aggregated.expression
  | `Self_ast_aggregated_bad_format_entrypoint_ann of string * Location.t
  | `Self_ast_aggregated_entrypoint_ann_not_literal of Location.t
  | `Self_ast_aggregated_emit_tag_not_literal of Location.t
  | `Self_ast_aggregated_unmatched_entrypoint of Location.t
  | `Self_ast_aggregated_corner_case of string
  | `Self_ast_aggregated_bad_single_arity of (Constant.constant' * Ast_aggregated.expression)
  | `Self_ast_aggregated_bad_map_param_type of (Constant.constant' * Ast_aggregated.expression)
  | `Self_ast_aggregated_bad_set_param_type of (Constant.constant' * Ast_aggregated.expression)
] [@@deriving poly_constructor { prefix = "self_ast_aggregated_" }]

let error_ppformat : display_format:string display_format ->
  Format.formatter -> self_ast_aggregated_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Self_ast_aggregated_expected_obj_ligo loc ->
      Format.fprintf f
        "@[<hv>%a@.Invalid usage of a Test primitive or type in object ligo.@]"
        Snippet.pp loc
    | `Self_ast_aggregated_polymorphism_unresolved loc ->
      Format.fprintf f
        "@[<hv>%a@.Can't infer the type of this value, please add a type annotation.@]"
        Snippet.pp loc
    | `Self_ast_aggregated_fvs_in_create_contract_lambda (e,v) ->
      Format.fprintf f
        "@[<hv>%a@.Free variable usage is not allowed in call to Tezos.create_contract:@.%a@]"
        Snippet.pp e.location
        Snippet.pp (Value_var.get_location v)
    | `Self_ast_aggregated_create_contract_lambda (_cst,e) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid usage of Tezos.create_contract.@.The first argument must be an inline function. @]"
        Snippet.pp e.location
    | `Self_ast_aggregated_bad_format_entrypoint_ann (ep,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid entrypoint \"%s\". One of the following patterns is expected:@.* \"%%bar\" is expected for entrypoint \"Bar\"@.* \"%%default\" when no entrypoint is used."
        Snippet.pp loc
        ep
    | `Self_ast_aggregated_entrypoint_ann_not_literal loc ->
      Format.fprintf f
        "@[<hv>%a@.Invalid entrypoint value.@.The entrypoint value must be a string literal. @]"
        Snippet.pp loc
    | `Self_ast_aggregated_emit_tag_not_literal loc ->
      Format.fprintf f
        "@[<hv>%a@.Invalid event tag.@.The tag must be a string literal. @]"
        Snippet.pp loc
    | `Self_ast_aggregated_unmatched_entrypoint loc ->
      Format.fprintf f
        "@[<hv>%a@.Invalid entrypoint value.@.The entrypoint value does not match a constructor of the contract parameter. @]"
        Snippet.pp loc
    | `Self_ast_aggregated_corner_case desc ->
      Format.fprintf f
        "@[<hv>Internal error: %s @]"
        desc
    | `Self_ast_aggregated_bad_single_arity (c, e) ->
      Format.fprintf f
        "@[<hv>%a@ Ill-formed \"%a\" expression@.One function argument is expected. @]"
        Snippet.pp e.location Constant.pp_constant' c
    | `Self_ast_aggregated_bad_map_param_type (c,e) ->
      Format.fprintf f
        "@[<hv>%a@ Ill-formed \"%a\" expression.@.A list of pair parameters is expected.@]"
        Snippet.pp e.location Constant.pp_constant' c
    | `Self_ast_aggregated_bad_set_param_type (c,e) ->
      Format.fprintf f
        "@[<hv>%a@ Ill-formed \"%a\" expression.@.A list of pair parameters is expected.@]"
        Snippet.pp e.location Constant.pp_constant' c
  )

let error_json : self_ast_aggregated_error -> Simple_utils.Error.t =
  fun e ->
  let open Simple_utils.Error in
  match e with
  | `Self_ast_aggregated_expected_obj_ligo location ->
    let message = "Invalid usage of a Test primitive or type in object ligo." in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_polymorphism_unresolved location ->
    let message = "Can't infer the type of this value, please add a type annotation." in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_fvs_in_create_contract_lambda (_,v) ->
    let location = Value_var.get_location v in
    let message  = "Free variable usage is not allowed in call to Tezos.create_contract." in
    let content  = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_create_contract_lambda (_cst,e) ->
    let location = e.location in
    let message = Format.sprintf "Invalid usage of Tezos.create_contract.@.The first argument must be an inline function." in 
    let content  = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_bad_format_entrypoint_ann (ep,location) ->
    let message = Format.sprintf "Invalid entrypoint \"%s\". One of the following patterns is expected:@.* \"%%bar\" is expected for entrypoint \"Bar\"@.* \"%%default\" when no entrypoint is used." ep in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_entrypoint_ann_not_literal location ->
    let message = Format.sprintf "Invalid entrypoint value.@.The entrypoint value must be a string literal." in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_emit_tag_not_literal location ->
    let message = Format.sprintf "Invalid event tag.@.The tag must be a string literal." in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_unmatched_entrypoint location ->
    let message = Format.sprintf "Invalid entrypoint value.@.The entrypoint value does not match a constructor of the contract parameter." in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_corner_case desc ->
    let message = Format.sprintf "Internal error: %s" desc in
    let content = make_content ~message () in
    make ~stage ~content
  | `Self_ast_aggregated_bad_single_arity (c, e) ->
    let location = e.location in 
    let message = Format.asprintf "Ill-formed \"%a\" expression@.One function argument is expected." Constant.pp_constant' c in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_bad_map_param_type (c,e) ->
    let location = e.location in
    let message = Format.asprintf "Ill-formed \"%a\" expression.@.A list of pair parameters is expected." Constant.pp_constant' c in 
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_aggregated_bad_set_param_type (c,e) ->
    let location = e.location in
    let message = Format.asprintf "Ill-formed \"%a\" expression.@.A list of pair parameters is expected." Constant.pp_constant' c in
    let content = make_content ~message ~location () in
    make ~stage ~content

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
    let loc = Value_var.get_location v in
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
    let message = `String (Format.asprintf "First argument of %a must be inlined using a lambda" Constant.pp_constant' cst) in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let expression = `String (Format.asprintf "%a" Ast_aggregated.PP.expression e) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("expression", expression);
    ] in
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
  | `Self_ast_aggregated_emit_tag_not_literal loc ->
    let message = `String "The tag must be a string literal" in
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
  | `Self_ast_aggregated_bad_single_arity (c, e) ->
    let message = `String "constant expects one parameters" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let value = `String (Format.asprintf "%a" Constant.pp_constant' c) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Self_ast_aggregated_bad_map_param_type (c,e) ->
    let message = `String "constant expects a list of pair as parameter" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let value = `String (Format.asprintf "%a" Constant.pp_constant' c) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Self_ast_aggregated_bad_set_param_type (c,e) ->
    let message = `String "constant expects a list as parameter" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let value = `String (Format.asprintf "%a" Constant.pp_constant' c) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
