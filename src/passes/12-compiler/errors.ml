open Trace
open Simple_utils.Display
open Stage_common.Types

type compiler_error = [
  | `Compiler_get_environment of expression_variable * Mini_c.environment
  | `Compiler_corner_case of string * string
  | `Compiler_contract_entrypoint of string
  | `Compiler_expression_tracer of Mini_c.expression * Mini_c.type_expression * compiler_error
  | `Compiler_bad_iterator of Mini_c.constant'
  | `Compiler_not_comparable_base of Mini_c.type_base
  | `Compiler_not_comparable of Mini_c.type_expression
  | `Compiler_not_comparable_pair_struct
  | `Compiler_void_type_not_compilable
  | `Compiler_unparsing_unrecognized_data of
    (Proto_alpha_utils.Trace.tezos_alpha_error list)
  | `Compiler_untranspilable of Michelson.michelson * Michelson.michelson
  | `Compiler_bad_constant_arity of Mini_c.constant'
]

let stage = "compiler"
let uncompiler_stage = "uncompiler_stage"
let corner_case_msg () = 
  "we don't have a good error message for this case. we are
  striving find ways to better report them and find the use-cases that generate
  them. please report this to the developers."

let get_env var env = `Compiler_get_environment (var , env)
let corner_case ~loc  message = `Compiler_corner_case (loc,message)
let contract_entrypoint_must_be_literal ~loc = `Compiler_contract_entrypoint loc
let compile_expression_tracer e ty err = `Compiler_expression_tracer (e,ty,err)
let bad_iterator cst = `Compiler_bad_iterator cst
let not_comparable_base tb = `Compiler_not_comparable_base tb
let not_comparable t = `Compiler_not_comparable t
let not_comparable_pair_struct = `Compiler_not_comparable_pair_struct
let void_type_not_compilable = `Compiler_void_type_not_compilable
let unrecognized_data errs = `Compiler_unparsing_unrecognized_data errs
let untranspilable m_data m_type = `Compiler_untranspilable (m_data, m_type)
let bad_constant_arity c = `Compiler_bad_constant_arity c

let rec error_ppformat : display_format:string display_format ->
  Format.formatter -> compiler_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Compiler_get_environment (var,env) ->
      let s = Format.asprintf "failed to get var %a in environment %a"
        Var.pp var
        Mini_c.PP.environment env in
      Format.pp_print_string f s ;
    | `Compiler_corner_case (loc,msg) ->
      let s = Format.asprintf "compiler corner case at %s : %s\n %s"
        loc msg (corner_case_msg ()) in
      Format.pp_print_string f s ;
    | `Compiler_contract_entrypoint loc ->
      let s = Format.asprintf "contract entrypoint must be given as a literal string: %s"
        loc in
      Format.pp_print_string f s ;
    | `Compiler_expression_tracer (e,ty,err) ->
      Format.fprintf f
        "@[<hv>compiling expression@%a of type %a@%a]"
        Mini_c.PP.expression e 
        Mini_c.PP.type_variable ty
        (error_ppformat ~display_format) err
    | `Compiler_bad_iterator cst ->
       let s = Format.asprintf "bad iterator: iter %a" Mini_c.PP.constant cst in
      Format.pp_print_string f s ;
    | `Compiler_not_comparable_base tb ->
      let s = Format.asprintf "not a comparable type: %a" Mini_c.PP.type_constant tb in
      Format.pp_print_string f s ;
    | `Compiler_not_comparable t ->
      let s = Format.asprintf "not a comparable type: %a" Mini_c.PP.type_variable t in
      Format.pp_print_string f s ;
    | `Compiler_not_comparable_pair_struct ->
      let s = "pair does not have a comparable structure. (hint: use (a,(b,c)) instead of (a,b,c))" in
      Format.pp_print_string f s;
    | `Compiler_void_type_not_compilable ->
      let s = "void is not a compilable type" in
      Format.pp_print_string f s;
    | `Compiler_unparsing_unrecognized_data _errlist ->
      let s = "unparsing unrecognized data" in
      Format.pp_print_string f s;
    | `Compiler_untranspilable (mdata,mty) ->
      let s = Format.asprintf "this value can't be transpiled back yet. data : %a type : %a"
        Michelson.pp mdata
        Michelson.pp mty in
      Format.pp_print_string f s;
    | `Compiler_bad_constant_arity c ->
      Format.fprintf f
        "Bad arity for %a"
        Mini_c.PP.constant c
  )

let rec error_jsonformat : compiler_error -> J.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Compiler_get_environment (var,env) ->
    let var' = Format.asprintf "%a" Var.pp var in
    let env' = Format.asprintf "%a" Mini_c.PP.environment env in
    let content = `Assoc [
      ("message", `String "failed to get var from environment");
      ("var", `String var'); 
      ("environment", `String env'); ] in
    json_error ~stage ~content
  | `Compiler_corner_case (loc,msg) ->
    let content = `Assoc [
      ("location", `String loc); 
      ("message", `String msg); ] in
    json_error ~stage ~content
  | `Compiler_contract_entrypoint loc ->
    let content = `Assoc [
      ("location", `String loc); 
      ("message", `String "contract entrypoint must be given as literal string"); ] in
    json_error ~stage ~content
  | `Compiler_expression_tracer (e,ty,err) ->
    let e' = Format.asprintf "%a" Mini_c.PP.expression e in
    let ty' = Format.asprintf "%a" Mini_c.PP.type_variable ty in
    let children = error_jsonformat err in
    let content = `Assoc [
      ("message", `String "compiling expression");
      ("expression", `String e');
      ("type", `String ty');
      ("children", children) ]
    in
    json_error ~stage ~content
  | `Compiler_bad_iterator cst ->
    let s = Format.asprintf "%a" Mini_c.PP.constant cst in
    let content = `Assoc [
       ("message", `String "bad iterator");
       ("iterator", `String s); ]
    in
    json_error ~stage ~content
  | `Compiler_not_comparable_base tb ->
    let s = Format.asprintf "%a" Mini_c.PP.type_constant tb in
    let content = `Assoc [
       ("message", `String "not a comparable type");
       ("type", `String s); ]
    in
    json_error ~stage ~content
  | `Compiler_not_comparable t ->
    let s = Format.asprintf "%a" Mini_c.PP.type_variable t in
    let content = `Assoc [
       ("message", `String "not a comparable type");
       ("type", `String s); ]
    in
    json_error ~stage ~content
  | `Compiler_not_comparable_pair_struct ->
    let content = `Assoc [
       ("message", `String "pair does not have a comparable structure");
       ("hint", `String "use (a,(b,c)) instead of (a,b,c)"); ]
    in
    json_error ~stage ~content
  | `Compiler_void_type_not_compilable ->
    let content = `Assoc [
       ("message", `String "void is not a compilable type"); ]
    in
    json_error ~stage ~content
  | `Compiler_unparsing_unrecognized_data _errlist ->
    let content = `Assoc [
       ("message", `String "unparsing unrecognized data"); ]
    in
    json_error ~stage:uncompiler_stage ~content
  | `Compiler_untranspilable (mdata,mty) ->
    let mdata' = Format.asprintf "%a" Michelson.pp mdata in
    let mty' = Format.asprintf "%a" Michelson.pp mty in
    let content = `Assoc [
       ("message", `String "this value can't be transpiled back yet");
       ("michelson data", `String mdata');
       ("michelson type", `String mty');
       ]
    in
    json_error ~stage:uncompiler_stage ~content
  | `Compiler_bad_constant_arity c ->
    let constant = Format.asprintf "%a" Mini_c.PP.constant c in
    let content = `Assoc [
       ("message", `String "Bad constant arity");
       ("constant", `String constant)]
    in
    json_error ~stage ~content