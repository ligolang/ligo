module Michelson = Tezos_utils.Michelson
open Ligo_prim
open Simple_utils.Display

type scoping_error = [
  | `Scoping_corner_case of string * string
  | `Scoping_contract_entrypoint of string
  | `Scoping_bad_iterator of Constant.constant'
  | `Scoping_not_comparable_pair_struct
  | `Scoping_could_not_tokenize_michelson of string
  | `Scoping_could_not_parse_michelson of string
  | `Scoping_untranspilable of int Michelson.t * int Michelson.t
  | `Scoping_unsupported_primitive of Constant.constant' * Environment.Protocols.t
] [@@deriving poly_constructor { prefix = "scoping_" }]

let stage = "scoping"
let unscoping_stage = "unscoping_stage"
let corner_case_msg () =
  "Sorry, we don't have a proper error message for this error. Please report \
   this use case so we can improve on this."

let untranspilable m_type m_data =
  let open Tezos_micheline.Micheline in
  let m_type = root (strip_locations m_type) in
  let m_data = root (strip_locations m_data) in
  `Scoping_untranspilable (m_type, m_data)

let error_ppformat : display_format:string display_format ->
  Format.formatter -> scoping_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Scoping_unsupported_primitive (c,p) ->
      Format.fprintf f "@[<hv>unsupported primitive %a in protocol %s@]" Constant.pp_constant' c (Environment.Protocols.variant_to_string p)
    | `Scoping_corner_case (loc,msg) ->
      let s = Format.asprintf "Scoping corner case at %s : %s.\n%s"
        loc msg (corner_case_msg ()) in
      Format.pp_print_string f s ;
    | `Scoping_contract_entrypoint loc ->
      let s = Format.asprintf "contract entrypoint must be given as a literal string: %s"
        loc in
      Format.pp_print_string f s ;
    | `Scoping_bad_iterator cst ->
       let s = Format.asprintf "bad iterator: iter %a" Mini_c.PP.constant cst in
       Format.pp_print_string f s ;
    | `Scoping_not_comparable_pair_struct ->
      let s = "Invalid comparable value. When using a tuple with more than 2 components, structure the tuple like this: \"(a, (b, c))\". " in
      Format.pp_print_string f s;
    | `Scoping_could_not_tokenize_michelson code ->
      Format.fprintf f "Could not tokenize raw Michelson: %s" code
    | `Scoping_could_not_parse_michelson code ->
      Format.fprintf f "Could not parse raw Michelson: %s" code
    | `Scoping_untranspilable (ty, value) ->
      Format.fprintf f "Could not untranspile Michelson value: %a %a"
        Michelson.pp ty
        Michelson.pp value
  )

let error_jsonformat : scoping_error -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Scoping_unsupported_primitive (_,_) ->
    let content = `Assoc [
      ("message", `String "unsupported primitive" );
      ]
    in
    json_error ~stage ~content
  | `Scoping_corner_case (loc,msg) ->
    let content = `Assoc [
      ("location", `String loc);
      ("message", `String msg); ] in
    json_error ~stage ~content
  | `Scoping_contract_entrypoint loc ->
    let content = `Assoc [
      ("location", `String loc);
      ("message", `String "contract entrypoint must be given as literal string"); ] in
    json_error ~stage ~content
  | `Scoping_bad_iterator cst ->
    let s = Format.asprintf "%a" Mini_c.PP.constant cst in
    let content = `Assoc [
       ("message", `String "bad iterator");
       ("iterator", `String s); ]
    in
    json_error ~stage ~content
  | `Scoping_not_comparable_pair_struct ->
    let content = `Assoc [
       ("message", `String "pair does not have a comparable structure");
       ("hint", `String "use (a,(b,c)) instead of (a,b,c)"); ]
    in
    json_error ~stage ~content
  | `Scoping_could_not_tokenize_michelson code ->
    let content =
      `Assoc [("message", `String "Could not tokenize raw Michelson");
              ("code", `String code)] in
    json_error ~stage ~content
  | `Scoping_could_not_parse_michelson code ->
    let content =
      `Assoc [("message", `String "Could not parse raw Michelson");
              ("code", `String code)] in
    json_error ~stage ~content
  | `Scoping_untranspilable (ty, value) ->
    let content =
      `Assoc [("message", `String "Could not untranspile Michelson value");
              ("type", `String (Format.asprintf "%a" Michelson.pp ty));
              ("value", `String (Format.asprintf "%a" Michelson.pp value))] in
    json_error ~stage ~content
