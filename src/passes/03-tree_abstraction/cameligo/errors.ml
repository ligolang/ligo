open Simple_utils.Display

module Raw = Cst.Cameligo

let stage = "abstracter"

type abs_error = [
  | `Concrete_cameligo_unknown_predefined_type of Raw.type_constr
  | `Concrete_cameligo_recursive_fun of Region.t
  | `Concrete_cameligo_unsupported_pattern_type of Raw.pattern list
  | `Concrete_cameligo_unsupported_string_singleton of Raw.type_expr
  | `Concrete_cameligo_unsupported_deep_list_pattern of Raw.pattern
  | `Concrete_cameligo_michelson_type_wrong of Raw.type_expr * string
  | `Concrete_cameligo_michelson_type_wrong_arity of Location.t * string
  | `Concrete_cameligo_recursion_on_non_function of Location.t
  | `Concrete_cameligo_missing_funarg_annotation of Raw.variable
  ]

let unknown_predefined_type name = `Concrete_cameligo_unknown_predefined_type name
let untyped_recursive_fun reg = `Concrete_cameligo_recursive_fun reg
let unsupported_pattern_type pl = `Concrete_cameligo_unsupported_pattern_type pl
let unsupported_deep_list_patterns cons = `Concrete_cameligo_unsupported_deep_list_pattern cons
let unsupported_string_singleton te = `Concrete_cameligo_unsupported_string_singleton te
let recursion_on_non_function reg = `Concrete_cameligo_recursion_on_non_function reg
let michelson_type_wrong texpr name = `Concrete_cameligo_michelson_type_wrong (texpr,name)
let michelson_type_wrong_arity loc name = `Concrete_cameligo_michelson_type_wrong_arity (loc,name)
let missing_funarg_annotation v = `Concrete_cameligo_missing_funarg_annotation v

let error_ppformat : display_format:string display_format ->
  Format.formatter -> abs_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Concrete_cameligo_unknown_predefined_type type_name ->
      Format.fprintf f
        "@[<hv>%a@.Unknown type \"%s\". @]"
        Location.pp_lift type_name.Region.region
        type_name.Region.value
    | `Concrete_cameligo_recursive_fun reg ->
      Format.fprintf f
      "@[<hv>%a@.Invalid function declaration.@.Recursive functions are required to have a type annotation (for now). @]"
        Location.pp_lift reg
    | `Concrete_cameligo_unsupported_pattern_type pl ->
      Format.fprintf f
      "@[<hv>%a@.Invalid pattern matching.
If this is pattern matching over Booleans, then \"true\" or \"false\" is expected.
If this is pattern matching on a list, then one of the following is expected:
  * an empty list pattern \"[]\";
  * a cons list pattern \"head#tail\".
If this is pattern matching over variants, then a constructor of a variant is expected.
      
Other forms of pattern matching are not (yet) supported. @]"
        Location.pp_lift (List.fold_left (fun a p -> Region.cover a (Raw.pattern_to_region p)) Region.ghost pl)
    | `Concrete_cameligo_unsupported_string_singleton te ->
      Format.fprintf f
        "@[<hv>%a@.Invalid type. @.It's not possible to assign a string to a type. @]"
        Location.pp_lift (Raw.type_expr_to_region te)
    | `Concrete_cameligo_unsupported_deep_list_pattern cons ->
      Format.fprintf f
        "@[<hv>%a@.Invalid pattern matching. @.At this point, one of the following is expected: 
  * an empty list pattern \"[]\";
  * a cons list pattern \"head :: tail\".@]"
        Location.pp_lift @@ Raw.pattern_to_region cons
    | `Concrete_cameligo_recursion_on_non_function reg ->
      Format.fprintf f "@[<hv>%a@.Invalid let declaration.@.Only functions can be recursive. @]"
      Location.pp reg
    | `Concrete_cameligo_michelson_type_wrong (texpr,name) ->
      Format.fprintf f
       "@[<hv>%a@.Invalid \"%s\" type.@.At this point, an annotation, in the form of a string, is expected for the preceding type. @]"
          Location.pp_lift (Raw.type_expr_to_region texpr)
          name
    | `Concrete_cameligo_michelson_type_wrong_arity (loc,name) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid \"%s\" type.@.An even number of 2 or more arguments is expected, where each odd item is a type annotated by the following string. @]"
        Location.pp loc
        name
    | `Concrete_cameligo_missing_funarg_annotation v ->
      Format.fprintf f
        "@[<hv>%a@.Missing a type annotation for argument \"%s\". @]"
          Location.pp_lift v.region
          v.value
  )


let error_jsonformat : abs_error -> Yojson.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Concrete_cameligo_unknown_predefined_type type_name ->
    let message = `String "Unknown predefined type" in
    let t = `String type_name.Region.value in
    let loc = Format.asprintf "%a" Location.pp_lift type_name.Region.region in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);
      ("type", t ) ] in
    json_error ~stage ~content
  | `Concrete_cameligo_recursive_fun reg ->
    let message = `String "Untyped recursive functions are not supported yet" in
    let loc = Format.asprintf "%a" Location.pp_lift reg in
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
  | `Concrete_cameligo_recursion_on_non_function reg ->
    let message = Format.asprintf "Only functions can be recursive." in
    let loc = Format.asprintf "%a" Location.pp reg in
    let content = `Assoc [
      ("message", `String message );
      ("location", `String loc) ] in
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
  | `Concrete_cameligo_missing_funarg_annotation v ->
    let message = Format.asprintf "Missing type annotation for argument \"%s\"" v.value in
    let loc = Format.asprintf "%a" Location.pp_lift v.region in
    let content = `Assoc [
      ("message", `String message );
      ("location", `String loc); ] in
    json_error ~stage ~content