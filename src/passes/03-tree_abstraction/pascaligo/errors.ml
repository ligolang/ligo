open Simple_utils.Display

module Raw = Cst.Pascaligo

let stage = "abstracter"

type abs_error = [
  | `Concrete_pascaligo_unsupported_constant_constr of Raw.pattern
  | `Concrete_pascaligo_unknown_predefined_type of Raw.constr
  | `Concrete_pascaligo_unsupported_non_var_pattern of Raw.pattern
  | `Concrete_pascaligo_only_constructors of Raw.pattern
  | `Concrete_pascaligo_unsupported_pattern_type of Raw.pattern
  | `Concrete_pascaligo_unsupported_tuple_pattern of Raw.pattern
  | `Concrete_pascaligo_unsupported_string_singleton of Raw.type_expr
  | `Concrete_pascaligo_unsupported_deep_some_pattern of Raw.pattern
  | `Concrete_pascaligo_unsupported_deep_list_pattern of Raw.pattern
  | `Concrete_pascaligo_unsupported_deep_tuple_pattern of (Raw.pattern, Raw.wild) Simple_utils.Utils.nsepseq Raw.par Raw.reg
  | `Concrete_pascaligo_unknown_built_in of string
  | `Concrete_pascaligo_michelson_type_wrong of Raw.type_expr * string
  | `Concrete_pascaligo_michelson_type_wrong_arity of Location.t * string
  | `Concrete_pascaligo_instruction_tracer of Raw.instruction * abs_error
  | `Concrete_pascaligo_program_tracer of Raw.declaration list * abs_error
  | `Concrete_pascaligo_recursive_fun of Location.t
  | `Concrete_pascaligo_block_attribute of Raw.block Region.reg
  ]

let unsupported_cst_constr p = `Concrete_pascaligo_unsupported_constant_constr p
let unknown_predefined_type name = `Concrete_pascaligo_unknown_predefined_type name
let unsupported_non_var_pattern p = `Concrete_pascaligo_unsupported_non_var_pattern p
let untyped_recursive_fun loc = `Concrete_pascaligo_recursive_fun loc
let only_constructors p = `Concrete_pascaligo_only_constructors p
let unsupported_pattern_type pl = `Concrete_pascaligo_unsupported_pattern_type pl
let unsupported_tuple_pattern p = `Concrete_pascaligo_unsupported_tuple_pattern p
let unsupported_string_singleton te = `Concrete_pascaligo_unsupported_string_singleton te
let unsupported_deep_some_patterns p = `Concrete_pascaligo_unsupported_deep_some_pattern p
let unsupported_deep_list_patterns cons = `Concrete_pascaligo_unsupported_deep_list_pattern cons
let unsupported_deep_tuple_patterns t = `Concrete_pascaligo_unsupported_deep_tuple_pattern t
let unknown_built_in name = `Concrete_pascaligo_unknown_built_in name
let michelson_type_wrong texpr name = `Concrete_pascaligo_michelson_type_wrong (texpr,name)
let michelson_type_wrong_arity loc name = `Concrete_pascaligo_michelson_type_wrong_arity (loc,name)
let abstracting_instruction_tracer i err = `Concrete_pascaligo_instruction_tracer (i,err)
let program_tracer decl err = `Concrete_pascaligo_program_tracer (decl,err)
let block_start_with_attribute block = `Concrete_pascaligo_block_attribute block

let rec error_ppformat : display_format:string display_format ->
  Format.formatter -> abs_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Concrete_pascaligo_unknown_predefined_type type_name ->
      Format.fprintf f
        "@[<hv>%a@Unknown predefined type \"%s\"@]"
        Location.pp_lift type_name.Region.region
        type_name.Region.value
    | `Concrete_pascaligo_unsupported_pattern_type pl ->
      Format.fprintf f
        "@[<hv>%a@Currently, only booleans, lists, options, and constructors are supported in patterns@]"
        Location.pp_lift @@ Raw.pattern_to_region pl
    | `Concrete_pascaligo_unsupported_tuple_pattern p ->
      Format.fprintf f
        "@[<hv>%a@The following tuple pattern is not supported yet:@\"%s\"@]"
        Location.pp_lift (Raw.pattern_to_region p)
        (Cst_pascaligo.ParserLog.pattern_to_string ~offsets:true ~mode:`Point p)
    | `Concrete_pascaligo_unsupported_constant_constr p ->
      Format.fprintf f
        "@[<hv>%a@Constant constructors are not supported yet@]"
        Location.pp_lift (Raw.pattern_to_region p)
    | `Concrete_pascaligo_unsupported_non_var_pattern p ->
      Format.fprintf f
        "@[<hv>%a@Non-variable patterns in constructors are not supported yet@]"
        Location.pp_lift (Raw.pattern_to_region p)
    | `Concrete_pascaligo_unsupported_string_singleton te ->
      Format.fprintf f
        "@[<hv>%a@Unsupported singleton string type@]"
        Location.pp_lift (Raw.type_expr_to_region te)
    | `Concrete_pascaligo_unsupported_deep_some_pattern p ->
      Format.fprintf f
        "@[<hv>%a@Currently, only variables in Some constructors are supported@]"
        Location.pp_lift (Raw.pattern_to_region p)
    | `Concrete_pascaligo_unsupported_deep_list_pattern cons ->
      Format.fprintf f
        "@[<hv>%a@Currently, only empty lists and x::y are supported in list patterns@]"
        Location.pp_lift @@ Raw.pattern_to_region cons
    | `Concrete_pascaligo_unsupported_deep_tuple_pattern tuple ->
      Format.fprintf f
        "@[<hv>%a@Currently, nested tuple pattern is not suppoerted@]"
        Location.pp_lift @@ tuple.Region.region
    | `Concrete_pascaligo_only_constructors p ->
      Format.fprintf f
        "@[<hv>%a@Currently, only constructors are supported in patterns@]"
        Location.pp_lift (Raw.pattern_to_region p)
    | `Concrete_pascaligo_unknown_built_in bi ->
      Format.fprintf f "Unknown built-in function %s" bi
    | `Concrete_pascaligo_michelson_type_wrong (texpr,name) ->
      Format.fprintf f
        "@[<hv>%a@Argument %s of %s must be a string singleton@]"
          Location.pp_lift (Raw.type_expr_to_region texpr)
          (Cst_pascaligo.ParserLog.type_expr_to_string ~offsets:true ~mode:`Point texpr)
          name
    | `Concrete_pascaligo_michelson_type_wrong_arity (loc,name) ->
      Format.fprintf f
        "@[<hv>%a@%s does not have the right number of argument@]"
        Location.pp loc
        name
    | `Concrete_pascaligo_instruction_tracer (inst,err) ->
      Format.fprintf f
        "@[<hv>%a@Abstracting instruction:@\"%s\"@%a@]"
        Location.pp_lift (Raw.instr_to_region inst)
        (Cst_pascaligo.ParserLog.instruction_to_string ~offsets:true ~mode:`Point inst)
        (error_ppformat ~display_format) err
    | `Concrete_pascaligo_program_tracer (decl,err) ->
      Format.fprintf f
        "@[<hv>%a@Abstracting program@%a@]"
        Location.pp_lift (List.fold_left (fun a d -> Region.cover a (Raw.declaration_to_region d)) Region.ghost decl)
        (error_ppformat ~display_format) err
    | `Concrete_pascaligo_recursive_fun loc ->
      Format.fprintf f
        "@[<hv>%a@Untyped recursive functions are not supported yet@]"
        Location.pp loc
    | `Concrete_pascaligo_block_attribute block ->
      Format.fprintf f
        "@[<hv>%a@Attributes have to follow the declaration it is attached@]"
        Location.pp_lift @@ block.region
  )


let rec error_jsonformat : abs_error -> Yojson.t = fun a ->
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
  | `Concrete_pascaligo_unsupported_tuple_pattern p ->
    let message = `String "The following tuple pattern is not supported yet" in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.pattern_to_region p) in
    let pattern = Cst_pascaligo.ParserLog.pattern_to_string ~offsets:true ~mode:`Point p in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);
      ("pattern", `String pattern); ] in
    json_error ~stage ~content
  | `Concrete_pascaligo_unsupported_constant_constr p ->
    let message = `String "Constant constructors are not supported yet" in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.pattern_to_region p) in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_pascaligo_unsupported_non_var_pattern p ->
    let message = `String "Non-variable patterns in constructors are not supported yet" in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.pattern_to_region p) in
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
  | `Concrete_pascaligo_unsupported_deep_some_pattern p ->
    let message = `String "Currently, only variables in Some constructors are supported" in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.pattern_to_region p) in
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
  | `Concrete_pascaligo_only_constructors p ->
    let message = `String "Currently, only constructors are supported in patterns" in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.pattern_to_region p) in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_pascaligo_unknown_built_in bi ->
    let message = Format.asprintf "Unknown built-in function %s" bi in
    let content = `Assoc [
      ("message", `String message ); ] in
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
  | `Concrete_pascaligo_instruction_tracer (inst,err) ->
    let message = `String "Abstracting instruction" in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.instr_to_region inst) in
    let expr = Cst_pascaligo.ParserLog.instruction_to_string ~offsets:true ~mode:`Point inst in
    let children = error_jsonformat err in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);
      ("instruction", `String expr);
      ("children", children) ] in
    json_error ~stage ~content
  | `Concrete_pascaligo_program_tracer (decl,err) ->
    let message = `String "Abstracting program" in
    let loc = Format.asprintf "%a"
      Location.pp_lift (List.fold_left (fun a d -> Region.cover a (Raw.declaration_to_region d)) Region.ghost decl) in
    let children = error_jsonformat err in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);
      ("children", children) ] in
    json_error ~stage ~content
  | `Concrete_pascaligo_block_attribute block ->
    let message = Format.asprintf "Attributes have to follow the declaration it is attached" in
    let loc = Format.asprintf "%a" Location.pp_lift block.region in
    let content = `Assoc [
      ("message", `String message );
      ("location", `String loc); ] in
    json_error ~stage ~content
