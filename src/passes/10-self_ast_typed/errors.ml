open Simple_utils.Display

let stage = "self_ast_typed"

type self_ast_typed_error = [
  | `Self_ast_typed_rec_call of Ast_typed.expression_variable * Location.t
  | `Self_ast_typed_bad_self_type of Ast_typed.type_expression * Ast_typed.type_expression * Location.t
  | `Self_ast_typed_format_entrypoint_ann of string * Location.t
  | `Self_ast_typed_entrypoint_ann_not_literal of Location.t
  | `Self_ast_typed_unmatched_entrypoint of Location.t
  | `Self_ast_typed_nested_big_map of Location.t
  | `Self_ast_typed_corner_case of string
  | `Self_ast_typed_contract_io of string * Ast_typed.expression
  | `Self_ast_typed_contract_list_ops of string * Ast_typed.type_expression * Ast_typed.expression
  | `Self_ast_typed_expected_same_entry of
    string * Ast_typed.type_expression * Ast_typed.type_expression * Ast_typed.expression
  | `Self_ast_typed_pair_in of Location.t
  | `Self_ast_typed_pair_out of Location.t
]
let recursive_call_is_only_allowed_as_the_last_operation name loc =
  `Self_ast_typed_rec_call (name,loc)
let bad_self_type expected got loc =
  `Self_ast_typed_bad_self_type (expected,got,loc)
let bad_format_entrypoint_ann ep loc =
  `Self_ast_typed_format_entrypoint_ann (ep,loc)
let entrypoint_annotation_not_literal loc =
  `Self_ast_typed_entrypoint_ann_not_literal loc
let unmatched_entrypoint loc =
  `Self_ast_typed_unmatched_entrypoint loc
let nested_bigmap loc = `Self_ast_typed_nested_big_map loc
let corner_case s = `Self_ast_typed_corner_case s
let bad_contract_io entrypoint e = `Self_ast_typed_contract_io (entrypoint, e)
let expected_list_operation entrypoint got e =
  `Self_ast_typed_contract_list_ops (entrypoint, got, e)
let expected_same entrypoint t1 t2 e =
  `Self_ast_typed_expected_same_entry (entrypoint,t1,t2,e)
let expected_pair_in loc = `Self_ast_typed_pair_in loc
let expected_pair_out loc = `Self_ast_typed_pair_out loc

let error_ppformat : display_format:string display_format ->
  Format.formatter -> self_ast_typed_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Self_ast_typed_rec_call (_name,loc) ->
      Format.fprintf f
        "@[<hv>%a@ Recursion must be achieved through tail-calls only@]"
        Location.pp loc
    | `Self_ast_typed_bad_self_type (expected,got,loc) ->
      Format.fprintf f
        "@[<hv>%a@ Bad self type@ expected %a@ got %a@]"
        Location.pp loc
        Ast_typed.PP.type_expression expected
        Ast_typed.PP.type_expression got 
    | `Self_ast_typed_format_entrypoint_ann (ep,loc) ->
      Format.fprintf f
        "@[<hv>%a@ Bad entrypoint format '%s'@ We expect '%%bar' for entrypoint Bar and '%%default' when no entrypoint used"
        Location.pp loc
        ep
    | `Self_ast_typed_entrypoint_ann_not_literal loc ->
      Format.fprintf f
        "@[<hv>%a@ Entrypoint annotation must be a string literal@]"
        Location.pp loc
    | `Self_ast_typed_unmatched_entrypoint loc ->
      Format.fprintf f
        "@[<hv>%a@ No constructor matches the entrypoint annotation@]"
        Location.pp loc
    | `Self_ast_typed_nested_big_map loc ->
      Format.fprintf f
        "@[<hv>%a@ It looks like you have nested a big map inside another big map, this is not supported@]"
        Location.pp loc
    | `Self_ast_typed_corner_case desc ->
      Format.fprintf f
        "@[<hv>Internal error: %s @]"
        desc
    | `Self_ast_typed_contract_io (_entrypoint, e) ->
      Format.fprintf f
        "@[<hv>%a@ Badly typed contract:@ unexpected entrypoint type %a@]"
        Location.pp e.location
        Ast_typed.PP.type_expression e.type_expression
    | `Self_ast_typed_contract_list_ops (_entrypoint, got, e) ->
      Format.fprintf f
        "@[<hv>%a@ Badly typed contract:@ expected %a but got %a@]"
        Location.pp e.location
        Ast_typed.PP.type_expression {got with type_content= T_constant {type_constant=TC_list;arguments=[{got with type_content=T_constant {type_constant=TC_operation;arguments=[]}}]}}
        Ast_typed.PP.type_expression got
    | `Self_ast_typed_expected_same_entry (entrypoint,t1,t2,e) ->
      Format.fprintf f
        "@[<hv>%a@ Badly typed contract %s:@ expected storage type as right member of a pair in the input and output, \
        but got:@ - %a in the input@ - %a in the output @]"
        Location.pp e.location
        entrypoint
        Ast_typed.PP.type_expression t1
        Ast_typed.PP.type_expression t2
    | `Self_ast_typed_pair_in loc ->
      Format.fprintf f
        "@[<hv>%a@ Badly typed contract:@ expected a pair as parameter@]"
        Location.pp loc
    | `Self_ast_typed_pair_out loc ->
      Format.fprintf f
        "@[<hv>%a@ Badly typed contract:@ expected a pair as return type@]"
        Location.pp loc
  )

let error_jsonformat : self_ast_typed_error -> Yojson.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Self_ast_typed_rec_call (name,loc) ->
    let message = `String "recursion must be achieved through tail-calls only" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let fn = `String (Format.asprintf "%a" Ast_typed.PP.expression_variable name) in
    let content = `Assoc [
       ("message", message);
       ("location", loc);
       ("function", fn);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_bad_self_type (expected,got,loc) ->
    let message = `String "bad self type" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let expected = `String (Format.asprintf "%a" Ast_typed.PP.type_expression expected) in
    let actual = `String (Format.asprintf "%a" Ast_typed.PP.type_expression got) in
    let content = `Assoc [
       ("message", message);
       ("location", loc);
       ("expected", expected);
       ("actual", actual);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_format_entrypoint_ann (ep,loc) ->
    let message = `String "bad entrypoint format" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let entrypoint = `String ep in
    let hint = `String "we expect '%%bar' for entrypoint Bar and '%%default' when no entrypoint used" in
    let content = `Assoc [
       ("message", message);
       ("location", loc);
       ("hint", hint);
       ("entrypoint", entrypoint);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_entrypoint_ann_not_literal loc ->
    let message = `String "entrypoint annotation must be a string literal" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
       ("message", message);
       ("location", loc);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_unmatched_entrypoint loc ->
    let message = `String "no constructor matches the entrypoint annotation" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
       ("message", message);
       ("location", loc);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_nested_big_map loc ->
    let message = `String "it looks like you have nested a big map inside another big map, this is not supported" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
       ("message", message);
       ("location", loc);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_corner_case desc ->
    let message = `String "internal error" in
    let description = `String desc in
    let content = `Assoc [
       ("message", message);
       ("description", description);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_contract_io (entrypoint, e) ->
    let message = `String "badly typed contract" in
    let description = `String "unexpected entrypoint type" in
    let entrypoint = `String entrypoint in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let eptype = `String (Format.asprintf "%a" Ast_typed.PP.type_expression e.type_expression) in
    let content = `Assoc [
       ("message", message);
       ("description", description);
       ("entrypoint", entrypoint);
       ("location", loc);
       ("type", eptype);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_contract_list_ops (entrypoint, got, e) ->
    let entrypoint = `String entrypoint in
    let message = `String "badly typed contract" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let actual = `String (Format.asprintf "%a"
      Ast_typed.PP.type_expression {got with type_content= T_constant {type_constant=TC_list;arguments=[{got with type_content=T_constant {type_constant=TC_operation;arguments=[]}}]}}) in
    let expected = `String (Format.asprintf "%a" Ast_typed.PP.type_expression got) in
    let content = `Assoc [
       ("message", message);
       ("entrypoint", entrypoint);
       ("location", loc);
       ("expected", expected);
       ("actual", actual);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_expected_same_entry (entrypoint,t1,t2,e) ->
    let entrypoint = `String entrypoint in
    let message = `String "badly typed contract" in
    let description = `String "expected storages" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let t1 = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t1) in
    let t2 = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t2) in
    let content = `Assoc [
       ("entrypoint", entrypoint);
       ("message", message);
       ("location", loc);
       ("description", description);
       ("type1", t1);
       ("type2", t2);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_pair_in loc ->
    let message = `String "badly typed contract" in
    let description = `String "expected a pair as parameter" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
       ("message", message);
       ("location", loc);
       ("description", description);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_pair_out loc ->
    let message = `String "badly typed contract" in
    let description = `String "expected a pair as return type" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
       ("message", message);
       ("location", loc);
       ("description", description);
       ]
    in
    json_error ~stage ~content
