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

let error_json : scoping_error -> Simple_utils.Error.t =
  fun e ->
    let open Simple_utils.Error in
    match e with
    | `Scoping_unsupported_primitive (c,p) ->
      let message = Format.asprintf "@[<hv>unsupported primitive %a in protocol %s@]" Constant.pp_constant' c (Environment.Protocols.variant_to_string p) in
      let content = make_content ~message () in
      make ~stage ~content
    | `Scoping_corner_case (location,msg) ->
      let message = Format.asprintf "Scoping corner case at %s : %s.\n%s"
location msg (corner_case_msg ()) in
      let content = make_content ~message () in
      make ~stage ~content
    | `Scoping_contract_entrypoint loc ->
      let message = Format.asprintf "contract entrypoint must be given as a literal string: %s"
        loc in
      let content = make_content ~message () in
      make ~stage ~content
    | `Scoping_bad_iterator cst ->
      let message = Format.asprintf "bad iterator: iter %a" Mini_c.PP.constant cst in
      let content = make_content ~message () in
      make ~stage ~content
    | `Scoping_not_comparable_pair_struct ->
      let message = "Invalid comparable value. When using a tuple with more than 2 components, structure the tuple like this: \"(a, (b, c))\". " in
      let content = make_content ~message () in
      make ~stage ~content
    | `Scoping_could_not_tokenize_michelson code ->
      let message = Format.asprintf "Could not tokenize raw Michelson: %s" code in
      let content = make_content ~message () in
      make ~stage ~content
    | `Scoping_could_not_parse_michelson code ->
      let message = Format.asprintf "Could not parse raw Michelson: %s" code in
      let content = make_content ~message () in
      make ~stage ~content
    | `Scoping_untranspilable (ty, value) ->
      let message = Format.asprintf "Could not untranspile Michelson value: %a %a"
        Michelson.pp ty
        Michelson.pp value in
      let content = make_content ~message () in
      make ~stage ~content
