open Simple_utils.Display

module Raw = Cst.Cameligo

let stage = "abstracter"

type abs_error = [
  | `Concrete_cameligo_wrong_pattern of string * Raw.pattern
  | `Concrete_cameligo_unsupported_let_in of Raw.pattern list
  | `Concrete_cameligo_unknown_predefined_type of Raw.type_constr
  | `Concrete_cameligo_untyped_fun_param of Raw.variable
  | `Concrete_cameligo_recursive_fun of Region.t
  | `Concrete_cameligo_unsupported_tuple_pattern of Raw.pattern
  | `Concrete_cameligo_unsupported_constant_constr of Raw.pattern
  | `Concrete_cameligo_unsupported_non_var_pattern of Raw.pattern
  | `Concrete_cameligo_unsupported_pattern_type of Raw.pattern list
  | `Concrete_cameligo_unsupported_string_singleton of Raw.type_expr
  | `Concrete_cameligo_unsupported_deep_list_pattern of Raw.pattern
  | `Concrete_cameligo_unsupported_deep_tuple_pattern of (Raw.pattern, Raw.wild) Simple_utils.Utils.nsepseq Raw.par Raw.reg
  | `Concrete_cameligo_abstraction_tracer of Raw.expr * abs_error
  | `Concrete_cameligo_abstraction_type_tracer of Raw.type_expr * abs_error
  | `Concrete_cameligo_bad_deconstruction of Raw.expr
  | `Concrete_cameligo_only_constructors of Raw.pattern
  | `Concrete_cameligo_unsupported_sugared_lists of Raw.wild
  | `Concrete_cameligo_corner_case of string
  | `Concrete_cameligo_unknown_built_in of string
  | `Concrete_cameligo_michelson_type_wrong of Raw.type_expr * string
  | `Concrete_cameligo_michelson_type_wrong_arity of Location.t * string
  | `Concrete_cameligo_program_tracer of Raw.declaration list * abs_error
  ]

let wrong_pattern expected actual = `Concrete_cameligo_wrong_pattern (expected,actual)
let unsupported_let_in_function patterns = `Concrete_cameligo_unsupported_let_in patterns
let unknown_predefined_type name = `Concrete_cameligo_unknown_predefined_type name
let untyped_fun_param var = `Concrete_cameligo_untyped_fun_param var
let untyped_recursive_fun reg = `Concrete_cameligo_recursive_fun reg
let unsupported_tuple_pattern p = `Concrete_cameligo_unsupported_tuple_pattern p
let unsupported_cst_constr p = `Concrete_cameligo_unsupported_constant_constr p
let unsupported_non_var_pattern p = `Concrete_cameligo_unsupported_non_var_pattern p
let unsupported_pattern_type pl = `Concrete_cameligo_unsupported_pattern_type pl
let unsupported_deep_list_patterns cons = `Concrete_cameligo_unsupported_deep_list_pattern cons
let unsupported_deep_tuple_patterns t = `Concrete_cameligo_unsupported_deep_tuple_pattern t
let unsupported_string_singleton te = `Concrete_cameligo_unsupported_string_singleton te
let abstracting_expr_tracer t err = `Concrete_cameligo_abstraction_tracer (t,err)
let abstracting_type_expr_tracer t err = `Concrete_cameligo_abstraction_type_tracer (t,err)
let bad_deconstruction t = `Concrete_cameligo_bad_deconstruction t
let only_constructors p = `Concrete_cameligo_only_constructors p
let unsupported_sugared_lists region = `Concrete_cameligo_unsupported_sugared_lists region
let corner_case desc = `Concrete_cameligo_corner_case desc
let unknown_built_in name = `Concrete_cameligo_unknown_built_in name
let michelson_type_wrong texpr name = `Concrete_cameligo_michelson_type_wrong (texpr,name)
let michelson_type_wrong_arity loc name = `Concrete_cameligo_michelson_type_wrong_arity (loc,name)
let program_tracer decl err = `Concrete_cameligo_program_tracer (decl,err)

let rec error_ppformat : display_format:string display_format ->
  Format.formatter -> abs_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Concrete_cameligo_wrong_pattern (expected_name,actual) ->
      Format.fprintf f
        "@[<hv>%a@ Wrong pattern: expected %s got %s@]"
        Location.pp_lift (Raw.pattern_to_region actual)
        (Cst_cameligo.ParserLog.pattern_to_string ~offsets:true ~mode:`Point actual)
        expected_name
    | `Concrete_cameligo_unsupported_let_in expr ->
      Format.fprintf f
        "@[<hv>%a@ Defining functions with \"let ... in\" is not supported yet@]"
        Location.pp_lift (List.fold_left (fun a p -> Region.cover a (Raw.pattern_to_region p)) Region.ghost expr)
    | `Concrete_cameligo_unknown_predefined_type type_name ->
      Format.fprintf f
        "@[<hv>%a@ Unknown predefined type \"%s\"@]"
        Location.pp_lift type_name.Region.region
        type_name.Region.value
    | `Concrete_cameligo_untyped_fun_param variable ->
      Format.fprintf f
        "@[<hv>%a@ Untyped function parameters are not supported yet@]"
        Location.pp_lift variable.Region.region
    | `Concrete_cameligo_recursive_fun reg ->
      Format.fprintf f
        "@[<hv>%a@ Untyped recursive functions are not supported yet@]"
        Location.pp_lift reg
    | `Concrete_cameligo_unsupported_tuple_pattern p ->
      Format.fprintf f
        "@[<hv>%a@ The following tuple pattern is not supported yet:@\"%s\"@]"
        Location.pp_lift (Raw.pattern_to_region p)
        (Cst_cameligo.ParserLog.pattern_to_string ~offsets:true ~mode:`Point p)
    | `Concrete_cameligo_unsupported_constant_constr p ->
      Format.fprintf f
        "@[<hv>%a@ Constant constructors are not supported yet@]"
        Location.pp_lift (Raw.pattern_to_region p)
    | `Concrete_cameligo_unsupported_non_var_pattern p ->
      Format.fprintf f
        "@[<hv>%a@ Non-variable patterns in constructors are not supported yet@]"
        Location.pp_lift (Raw.pattern_to_region p)
    | `Concrete_cameligo_unsupported_pattern_type pl ->
      Format.fprintf f
        "@[<hv>%a@ Currently, only booleans, lists, options, and constructors are supported in patterns@]"
        Location.pp_lift (List.fold_left (fun a p -> Region.cover a (Raw.pattern_to_region p)) Region.ghost pl)
    | `Concrete_cameligo_unsupported_string_singleton te ->
      Format.fprintf f
        "@[<hv>%a@ Unsupported singleton string type@]"
        Location.pp_lift (Raw.type_expr_to_region te)
    | `Concrete_cameligo_unsupported_deep_list_pattern cons ->
      Format.fprintf f
        "@[<hv>%a@Currently, only empty lists and x::y are supported in list patterns@]"
        Location.pp_lift @@ Raw.pattern_to_region cons
    | `Concrete_cameligo_unsupported_deep_tuple_pattern tuple ->
      Format.fprintf f
        "@[<hv>%a@Currently, nested tuple pattern is not suppoerted@]"
        Location.pp_lift @@ tuple.Region.region
    | `Concrete_cameligo_abstraction_tracer (expr,err) ->
      Format.fprintf f
        "@[<hv>%a@ Abstracting expression:@\"%s\"@%a@]"
        Location.pp_lift (Raw.expr_to_region expr)
        (Cst_cameligo.ParserLog.expr_to_string ~offsets:true ~mode:`Point expr)
        (error_ppformat ~display_format) err
    | `Concrete_cameligo_abstraction_type_tracer (te,err) ->
      Format.fprintf f
        "@[<hv>%a@ Abstracting type expression:@\"%s\"@%a@]"
        Location.pp_lift (Raw.type_expr_to_region te)
        (Cst_cameligo.ParserLog.type_expr_to_string ~offsets:true ~mode:`Point te)
        (error_ppformat ~display_format) err
    | `Concrete_cameligo_bad_deconstruction expr ->
      Format.fprintf f
        "@[<hv>%a@ Bad tuple deconstruction \"%s\"@]"
        Location.pp_lift (Raw.expr_to_region expr)
        (Cst_cameligo.ParserLog.expr_to_string ~offsets:true ~mode:`Point expr)
    | `Concrete_cameligo_only_constructors p ->
      Format.fprintf f
        "@[<hv>%a@ Currently, only constructors are supported in patterns@]"
        Location.pp_lift (Raw.pattern_to_region p)
    | `Concrete_cameligo_unsupported_sugared_lists wild ->
      Format.fprintf f
        "@[<hv>%a@ Currently, only empty lists and constructors (::) are supported in patterns@]"
        Location.pp_lift wild
    | `Concrete_cameligo_corner_case desc ->
      Format.fprintf f "Corner case: %s" desc
    | `Concrete_cameligo_unknown_built_in bi ->
      Format.fprintf f "Unknown built-in function %s" bi
    | `Concrete_cameligo_michelson_type_wrong (texpr,name) ->
      Format.fprintf f
        "@[<hv>%a@ Argument %s of %s must be a string singleton@]"
          Location.pp_lift (Raw.type_expr_to_region texpr)
          (Cst_cameligo.ParserLog.type_expr_to_string ~offsets:true ~mode:`Point texpr)
          name
    | `Concrete_cameligo_michelson_type_wrong_arity (loc,name) ->
      Format.fprintf f
        "@[<hv>%a@ %s does not have the right number of argument@]"
        Location.pp loc
        name
    | `Concrete_cameligo_program_tracer (decl,err) ->
      Format.fprintf f
        "@[<hv>%a@ Abstracting program@%a@]"
        Location.pp_lift (List.fold_left (fun a d -> Region.cover a (Raw.declaration_to_region d)) Region.ghost decl)
        (error_ppformat ~display_format) err
  )


let rec error_jsonformat : abs_error -> Yojson.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Concrete_cameligo_wrong_pattern (expected_name,actual) ->
    let message = `String "wrong pattern" in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.pattern_to_region actual) in
    let actual = (Cst_cameligo.ParserLog.pattern_to_string ~offsets:true ~mode:`Point actual) in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);
      ("expected", `String expected_name);
      ("actual", `String actual) ] in
    json_error ~stage ~content
  | `Concrete_cameligo_unsupported_let_in expr ->
    let message = `String "Defining functions with \"let ... in\" is not supported yet" in
    let loc = Format.asprintf "%a"
      Location.pp_lift (List.fold_left (fun a p -> Region.cover a (Raw.pattern_to_region p)) Region.ghost expr) in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc)] in
    json_error ~stage ~content
  | `Concrete_cameligo_unknown_predefined_type type_name ->
    let message = `String "Unknown predefined type" in
    let t = `String type_name.Region.value in
    let loc = Format.asprintf "%a" Location.pp_lift type_name.Region.region in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);
      ("type", t ) ] in
    json_error ~stage ~content
  | `Concrete_cameligo_untyped_fun_param variable ->
    let message = `String "Untyped function parameters are not supported yet" in
    let loc = Format.asprintf "%a" Location.pp_lift variable.Region.region in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_cameligo_recursive_fun reg ->
    let message = `String "Untyped recursive functions are not supported yet" in
    let loc = Format.asprintf "%a" Location.pp_lift reg in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_cameligo_unsupported_tuple_pattern p ->
    let message = `String "The following tuple pattern is not supported yet" in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.pattern_to_region p) in
    let pattern = Cst_cameligo.ParserLog.pattern_to_string ~offsets:true ~mode:`Point p in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);
      ("pattern", `String pattern); ] in
    json_error ~stage ~content
  | `Concrete_cameligo_unsupported_constant_constr p ->
    let message = `String "Constant constructors are not supported yet" in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.pattern_to_region p) in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_cameligo_unsupported_non_var_pattern p ->
    let message = `String "Non-variable patterns in constructors are not supported yet" in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.pattern_to_region p) in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_cameligo_unsupported_pattern_type pl ->
    let loc = Format.asprintf "%a"
      Location.pp_lift (List.fold_left (fun a p -> Region.cover a (Raw.pattern_to_region p)) Region.ghost pl) in
    let message = `String "Currently, only booleans, lists, options, and constructors are supported in patterns" in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_cameligo_unsupported_string_singleton te ->
    let message = `String "Unsupported singleton string type" in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.type_expr_to_region te) in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_cameligo_unsupported_deep_list_pattern cons ->
    let message = `String "Currently, only empty lists and x::y are supported in list patterns" in
    let loc = Format.asprintf "%a" Location.pp_lift @@ Raw.pattern_to_region cons in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_cameligo_unsupported_deep_tuple_pattern tuple ->
    let message = `String "Currently, nested tuple pattern is not supported" in
    let loc = Format.asprintf "%a" Location.pp_lift @@ tuple.Region.region in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_cameligo_abstraction_tracer (expr,err) ->
    let message = `String "Abstracting expression" in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.expr_to_region expr) in
    let expr = Cst_cameligo.ParserLog.expr_to_string ~offsets:true ~mode:`Point expr in
    let children = error_jsonformat err in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);
      ("expression", `String expr);
      ("children", children) ] in
    json_error ~stage ~content
  | `Concrete_cameligo_abstraction_type_tracer (te,err) ->
    let message = `String "Abstracting type expression" in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.type_expr_to_region te) in
    let expr = Cst_cameligo.ParserLog.type_expr_to_string ~offsets:true ~mode:`Point te in
    let children = error_jsonformat err in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);
      ("type expression", `String expr);
      ("children", children) ] in
    json_error ~stage ~content
  | `Concrete_cameligo_bad_deconstruction expr ->
    let message = `String "Bad tuple deconstruction" in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.expr_to_region expr) in
    let expr = Cst_cameligo.ParserLog.expr_to_string ~offsets:true ~mode:`Point expr in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);
      ("expression", `String expr) ] in
    json_error ~stage ~content
  | `Concrete_cameligo_only_constructors p ->
    let message = `String "Currently, only constructors are supported in patterns" in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.pattern_to_region p) in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_cameligo_unsupported_sugared_lists wild ->
    let message = `String "Currently, only empty lists and constructors (::) are supported in patterns" in
    let loc = Format.asprintf "%a" Location.pp_lift wild in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc); ] in
    json_error ~stage ~content
  | `Concrete_cameligo_corner_case desc ->
    let message = Format.asprintf "Corner case: %s" desc in
    let content = `Assoc [
      ("message", `String message ); ] in
    json_error ~stage ~content
  | `Concrete_cameligo_unknown_built_in bi ->
    let message = Format.asprintf "Unknown built-in function %s" bi in
    let content = `Assoc [
      ("message", `String message ); ] in
    json_error ~stage ~content
  | `Concrete_cameligo_michelson_type_wrong (texpr,name) ->
    let message = Format.asprintf "Argument %s of %s must be a string singleton"
        (Cst_cameligo.ParserLog.type_expr_to_string ~offsets:true ~mode:`Point texpr) name in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.type_expr_to_region texpr) in
    let content = `Assoc [
      ("message", `String message );
      ("location", `String loc); ] in
    json_error ~stage ~content
  | `Concrete_cameligo_michelson_type_wrong_arity (loc,name) ->
    let message = Format.asprintf "%s does not have the right number of argument" name in
    let loc = Format.asprintf "%a" Location.pp loc in
    let content = `Assoc [
      ("message", `String message );
      ("location", `String loc); ] in
    json_error ~stage ~content
  | `Concrete_cameligo_program_tracer (decl,err) ->
    let message = `String "Abstracting program" in
    let loc = Format.asprintf "%a"
      Location.pp_lift (List.fold_left (fun a d -> Region.cover a (Raw.declaration_to_region d)) Region.ghost decl) in
    let children = error_jsonformat err in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);
      ("children", children) ] in
    json_error ~stage ~content
