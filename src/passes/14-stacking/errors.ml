open Simple_utils.Display

type stacking_error = [
  | `Stacking_corner_case of string * string
  | `Stacking_contract_entrypoint of string
  | `Stacking_bad_iterator of Mini_c.constant'
  | `Stacking_not_comparable_base of Mini_c.type_base
  | `Stacking_not_comparable of Mini_c.type_expression
  | `Stacking_not_comparable_pair_struct
]

let stage = "stacking"
let unstacking_stage = "unstacking_stage"
let corner_case_msg () = 
  "Sorry, we don't have a proper error message for this error. Please report \
   this use case so we can improve on this."

let corner_case ~loc  message = `Stacking_corner_case (loc,message)
let contract_entrypoint_must_be_literal ~loc = `Stacking_contract_entrypoint loc
let bad_iterator cst = `Stacking_bad_iterator cst
let not_comparable_base tb = `Stacking_not_comparable_base tb
let not_comparable t = `Stacking_not_comparable t
let not_comparable_pair_struct = `Stacking_not_comparable_pair_struct
let unrecognized_data errs = `Stacking_unparsing_unrecognized_data errs
let untranspilable m_data m_type = `Stacking_untranspilable (m_data, m_type)
let bad_constant_arity c = `Stacking_bad_constant_arity c

let error_ppformat : display_format:string display_format ->
  Format.formatter -> stacking_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Stacking_corner_case (loc,msg) ->
      let s = Format.asprintf "Stacking corner case at %s : %s.\n%s"
        loc msg (corner_case_msg ()) in
      Format.pp_print_string f s ;
    | `Stacking_contract_entrypoint loc ->
      let s = Format.asprintf "contract entrypoint must be given as a literal string: %s"
        loc in
      Format.pp_print_string f s ;
    | `Stacking_bad_iterator cst ->
       let s = Format.asprintf "bad iterator: iter %a" Mini_c.PP.constant cst in
      Format.pp_print_string f s ;
    | `Stacking_not_comparable_base tb ->
      let s = Format.asprintf "Invalid comparable value \"%a\".@.Only the following types can be compared here: address, bool, bytes, int, key_hash, mutez, nat, string, and timestamp." Mini_c.PP.type_constant tb in
      Format.pp_print_string f s ;
    | `Stacking_not_comparable t ->
      let s = Format.asprintf "Invalid comparable value \"%a\".@.The following types can be compared here: address, bool, bytes, int, key_hash, mutez, nat, string, and timestamp.@.These types can be composed in a record or tuple." Mini_c.PP.type_variable t in
      Format.pp_print_string f s ;
    | `Stacking_not_comparable_pair_struct ->
      let s = "Invalid comparable value. When using a tuple of with more than 2 components, structure the tuple like this: \"(a, (b, c))\". " in
      Format.pp_print_string f s;
  )

let error_jsonformat : stacking_error -> Yojson.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Stacking_corner_case (loc,msg) ->
    let content = `Assoc [
      ("location", `String loc); 
      ("message", `String msg); ] in
    json_error ~stage ~content
  | `Stacking_contract_entrypoint loc ->
    let content = `Assoc [
      ("location", `String loc); 
      ("message", `String "contract entrypoint must be given as literal string"); ] in
    json_error ~stage ~content
  | `Stacking_bad_iterator cst ->
    let s = Format.asprintf "%a" Mini_c.PP.constant cst in
    let content = `Assoc [
       ("message", `String "bad iterator");
       ("iterator", `String s); ]
    in
    json_error ~stage ~content
  | `Stacking_not_comparable_base tb ->
    let s = Format.asprintf "%a" Mini_c.PP.type_constant tb in
    let content = `Assoc [
       ("message", `String "not a comparable type");
       ("type", `String s); ]
    in
    json_error ~stage ~content
  | `Stacking_not_comparable t ->
    let s = Format.asprintf "%a" Mini_c.PP.type_variable t in
    let content = `Assoc [
       ("message", `String "not a comparable type");
       ("type", `String s); ]
    in
    json_error ~stage ~content
  | `Stacking_not_comparable_pair_struct ->
    let content = `Assoc [
       ("message", `String "pair does not have a comparable structure");
       ("hint", `String "use (a,(b,c)) instead of (a,b,c)"); ]
    in
    json_error ~stage ~content
