open Simple_utils.Display

module Raw = Cst.Pascaligo

let stage = "abstracter"

type abs_error = [
  | `Concrete_pascaligo_unknown_predefined_type of Raw.constr
  | `Concrete_pascaligo_unsupported_pattern_type of Raw.pattern
  | `Concrete_pascaligo_unsupported_string_singleton of Raw.type_expr
  | `Concrete_pascaligo_unsupported_deep_list_pattern of Raw.pattern
  | `Concrete_pascaligo_unsupported_deep_tuple_pattern of (Raw.pattern, Raw.wild) Simple_utils.Utils.nsepseq Raw.par Raw.reg
  | `Concrete_pascaligo_michelson_type_wrong of Raw.type_expr * string
  | `Concrete_pascaligo_michelson_type_wrong_arity of Location.t * string
  | `Concrete_pascaligo_recursive_fun of Location.t
  | `Concrete_pascaligo_block_attribute of Raw.block Region.reg
  ]

let unknown_predefined_type name = `Concrete_pascaligo_unknown_predefined_type name
let untyped_recursive_fun loc = `Concrete_pascaligo_recursive_fun loc
let unsupported_pattern_type pl = `Concrete_pascaligo_unsupported_pattern_type pl
let unsupported_string_singleton te = `Concrete_pascaligo_unsupported_string_singleton te
let unsupported_deep_list_patterns cons = `Concrete_pascaligo_unsupported_deep_list_pattern cons
let unsupported_deep_tuple_patterns t = `Concrete_pascaligo_unsupported_deep_tuple_pattern t
let michelson_type_wrong texpr name = `Concrete_pascaligo_michelson_type_wrong (texpr,name)
let michelson_type_wrong_arity loc name = `Concrete_pascaligo_michelson_type_wrong_arity (loc,name)
let block_start_with_attribute block = `Concrete_pascaligo_block_attribute block

let error_ppformat : display_format:string display_format ->
  abs_error -> Location.t * string =
  fun ~display_format a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Concrete_pascaligo_unknown_predefined_type type_name ->
      (Location.lift type_name.Region.region, Format.asprintf
        "@[<hv>Unknown type \"%s\". @]"
        type_name.Region.value)
    | `Concrete_pascaligo_unsupported_pattern_type pl ->
      (Location.lift @@ Raw.pattern_to_region pl, Format.asprintf
        "@[<hv>Invalid case pattern.
If this is a case over Booleans, then \"true\" or \"false\" is expected.
If this is a case on a list, then one of the following is expected:
  * an empty list pattern \"[]\";
  * a cons list pattern \"head#tail\".
If this is a case over variants, then a constructor of a variant is expected.

Other patterns in case clauses are not (yet) supported. @]")
    | `Concrete_pascaligo_unsupported_string_singleton te ->
      (Location.lift (Raw.type_expr_to_region te), Format.asprintf
        "@[<hv>Invalid type. @.It's not possible to assign a string to a type. @]")
    | `Concrete_pascaligo_unsupported_deep_list_pattern cons ->
      (Location.lift @@ Raw.pattern_to_region cons, Format.asprintf
        "@[<hv>Invalid list pattern in a case clause. @.At this point, one of the following is expected: 
  * an empty list pattern \"nil\";
  * a cons list pattern \"head#tail\".@]")
    | `Concrete_pascaligo_unsupported_deep_tuple_pattern tuple ->
      (Location.lift tuple.Region.region, Format.asprintf
        "@[<hv>Invalid constructor in a case clause.@.Currently, nested constructor arguments (tuples) are not supported in case clauses.@]")
    | `Concrete_pascaligo_michelson_type_wrong (texpr,name) ->
      (Location.lift (Raw.type_expr_to_region texpr), Format.asprintf
        "@[<hv>Invalid \"%s\" type.@.At this point, an annotation, in the form of a string, is expected for the preceding type. @]"
          name)
    | `Concrete_pascaligo_michelson_type_wrong_arity (loc,name) ->
      (loc, Format.asprintf
        "@[<hv>Invalid \"%s\" type.@.An even number of 2 or more arguments is expected, where each odd item is a type annotated by the following string. @]"
        name)
    | `Concrete_pascaligo_recursive_fun loc ->
      (loc, Format.asprintf
        "@[<hv>Invalid function declaration.@.Recursive functions are required to have a type annotation (for now). @]")
    | `Concrete_pascaligo_block_attribute block ->
      (Location.lift  @@ block.region, Format.asprintf
        "@[<hv>Invalid attribute declaration.@.Attributes have to follow the declaration it is attached to. @]")
  )


let error_jsonformat : abs_error -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Concrete_pascaligo_unknown_predefined_type type_name ->
    let message = `String "Unknown predefined type" in
    let t = `String type_name.Region.value in
    let loc = Format.asprintf "%a" Location.pp_lift type_name.Region.region in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);
      ("type", t ) ] in
    json_error ~stage ~content
  | `Concrete_pascaligo_recursive_fun loc ->
    let message = `String "Untyped recursive functions are not supported yet" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_pascaligo_unsupported_pattern_type pl ->
    let loc = Format.asprintf "%a"
      Location.pp_lift @@ Raw.pattern_to_region pl in
    let message = `String "Currently, only booleans, lists, options, and constructors are supported in patterns" in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_pascaligo_unsupported_string_singleton te ->
    let message = `String "Unsupported singleton string type" in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.type_expr_to_region te) in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_pascaligo_unsupported_deep_list_pattern cons ->
    let message = `String "Currently, only empty lists and x::y are supported in list patterns" in
    let loc = Format.asprintf "%a" Location.pp_lift @@ Raw.pattern_to_region cons in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_pascaligo_unsupported_deep_tuple_pattern tuple ->
    let message = `String "Currently, nested tuple pattern is not supported" in
    let loc = Format.asprintf "%a" Location.pp_lift @@ tuple.Region.region in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_pascaligo_michelson_type_wrong (texpr,name) ->
    let message = Format.asprintf "Argument %s of %s must be a string singleton"
        (Cst_pascaligo.ParserLog.type_expr_to_string ~offsets:true ~mode:`Point texpr) name in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.type_expr_to_region texpr) in
    let content = `Assoc [
      ("message", `String message );
      ("location", `String loc); ] in
    json_error ~stage ~content
  | `Concrete_pascaligo_michelson_type_wrong_arity (loc,name) ->
    let message = Format.asprintf "%s does not have the right number of argument" name in
    let loc = Format.asprintf "%a" Location.pp loc in
    let content = `Assoc [
      ("message", `String message );
      ("location", `String loc); ] in
    json_error ~stage ~content
  | `Concrete_pascaligo_block_attribute block ->
    let message = Format.asprintf "Attributes have to follow the declaration it is attached" in
    let loc = Format.asprintf "%a" Location.pp_lift block.region in
    let content = `Assoc [
      ("message", `String message );
      ("location", `String loc); ] in
    json_error ~stage ~content
