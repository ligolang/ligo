open Simple_utils.Display

type spilling_error = [
  | `Spilling_corner_case of string * string
  | `Spilling_no_type_variable of Stage_common.Types.type_variable
  | `Spilling_unsupported_pattern_matching of Location.t
  | `Spilling_unsupported_iterator of Location.t
  | `Spilling_unsupported_recursive_function of Ast_typed.expression_variable
  | `Spilling_tracer of Location.t * spilling_error
  | `Spilling_wrong_mini_c_value of Ast_typed.type_expression * Mini_c.value
  | `Spilling_bad_decompile of Mini_c.value
  ]

let stage = "spilling"

let translation_tracer loc err = `Spilling_tracer (loc , err)

let corner_case ~loc desc = `Spilling_corner_case (loc, desc)
let corner_case_message () =
  "we don't have a good error message for this case. we are
   striving find ways to better report them and find the use-cases that generate
   them. please report this to the developers."

let no_type_variable name = `Spilling_no_type_variable name

let unsupported_tuple_pattern_matching location =
  `Spilling_unsupported_pattern_matching location

let unsupported_iterator location =
  `Spilling_unsupported_iterator location

let unsupported_recursive_function expression_variable =
  `Spilling_unsupported_recursive_function expression_variable

let wrong_mini_c_value expected actual =
  `Spilling_wrong_mini_c_value (expected , actual)

let bad_decompile bad_type =
  `Spilling_bad_decompile bad_type

let rec error_ppformat : display_format:string display_format ->
  Format.formatter -> spilling_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Spilling_tracer (loc,err) ->
      Format.fprintf f
        "@[<hv>%a@Translating expression@%a@]"
        Location.pp loc
        (error_ppformat ~display_format) err
    | `Spilling_corner_case (loc,desc) ->
      let s = Format.asprintf "%s\n corner case: %s\n%s" loc desc (corner_case_message ()) in
      Format.pp_print_string f s
    | `Spilling_no_type_variable tv ->
      let s = Format.asprintf "unbound type variables can't be transpiled : %a" Var.pp tv in
      Format.pp_print_string f s
    | `Spilling_unsupported_pattern_matching loc ->
      let s = Format.asprintf "%a\n unsupported pattern-matching: tuple patterns aren't supported yet" Location.pp loc in
      Format.pp_print_string f s
    | `Spilling_unsupported_iterator loc ->
      let s = Format.asprintf "%a\n unsupported iterator: only lambda are supported as iterators" Location.pp loc in
      Format.pp_print_string f s
    | `Spilling_unsupported_recursive_function var ->
      let s = Format.asprintf "Recursive functions with only one variable are supported : %a"
        Ast_typed.PP.expression_variable var in
      Format.pp_print_string f s
    | `Spilling_wrong_mini_c_value (expected , actual) ->
      let s = Format.asprintf "illed typed intermediary value: expected %a got %a"
        Ast_typed.PP.type_expression expected 
        Mini_c.PP.value actual in
      Format.pp_print_string f s
    | `Spilling_bad_decompile bad ->
      let s = Format.asprintf "can not untranspile %a"
        Mini_c.PP.value bad in
      Format.pp_print_string f s
  )

let rec error_jsonformat : spilling_error -> Yojson.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Spilling_tracer (loc, err) ->
    let loc' = Format.asprintf "%a" Location.pp loc in
    let children = error_jsonformat err in
    let content = `Assoc [
      ("location", `String loc');
      ("children", children) ]
    in
    json_error ~stage ~content
  | `Spilling_corner_case (loc, desc) ->
    let content = `Assoc [
      ("location", `String loc);
      ("description", `String desc);
      ("message", `String (corner_case_message ()) ); ]
    in
    json_error ~stage ~content
  | `Spilling_no_type_variable tv ->
    let tv' = Format.asprintf "%a" Var.pp tv in
    let content = `Assoc [
      ("description", `String "type variables can't be transpiled");
      ("type_variable", `String tv'); ]
    in
    json_error ~stage ~content
  | `Spilling_unsupported_pattern_matching loc ->
    let loc' = Format.asprintf "%a" Location.pp loc in
    let content = `Assoc [
      ("location", `String loc');
      ("message", `String "unsupported tuple in pattern-matching"); ]
    in
    json_error ~stage ~content
  | `Spilling_unsupported_iterator loc ->
    let loc' = Format.asprintf "%a" Location.pp loc in
    let content = `Assoc [
      ("location", `String loc');
      ("message", `String "unsupported iterator"); ]
    in
    json_error ~stage ~content
  | `Spilling_unsupported_recursive_function var ->
    let var' = Format.asprintf "%a" Ast_typed.PP.expression_variable var in
    let content = `Assoc [
      ("message", `String "Recursive functions with only one variable are supported");
      ("value", `String var'); ]
    in
    json_error ~stage ~content
  | `Spilling_wrong_mini_c_value (expected , actual) ->
    let expected' = Format.asprintf "%a" Ast_typed.PP.type_expression expected in
    let actual' = Format.asprintf "%a" Mini_c.PP.value actual in
    let content = `Assoc [
      ("message", `String "illed type of intermediary value does not match what was expected");
      ("expected", `String expected');
      ("actual", `String actual'); ]
    in
    json_error ~stage ~content
  | `Spilling_bad_decompile bad ->
    let var' = Format.asprintf "%a" Mini_c.PP.value bad in
    let content = `Assoc [
      ("message", `String "untranspiling bad value");
      ("value", `String var'); ]
    in
    json_error ~stage ~content
