module Michelson = Tezos_utils.Michelson
open Simple_utils.Display

type stacking_error =
  [ `Stacking_corner_case of string * string
  | `Stacking_contract_entrypoint of string
  | `Stacking_bad_iterator of Ligo_prim.Constant.constant'
  | `Stacking_not_comparable_pair_struct
  | `Stacking_could_not_tokenize_michelson of string
  | `Stacking_could_not_parse_michelson of string
  | `Stacking_untranspilable of int Michelson.t * int Michelson.t
  | `Stacking_unsupported_primitive of Ligo_prim.Constant.constant'
  ]
[@@deriving poly_constructor { prefix = "stacking_" }]

let stage = "stacking"

let corner_case_msg () =
  "Sorry, we don't have a proper error message for this error. Please report this use \
   case so we can improve on this."


let untranspilable m_type m_data =
  let open Tezos_micheline.Micheline in
  let m_type = root (strip_locations m_type) in
  let m_data = root (strip_locations m_data) in
  `Stacking_untranspilable (m_type, m_data)


let error_ppformat
    : display_format:string display_format -> Format.formatter -> stacking_error -> unit
  =
 fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev ->
    (match a with
    | `Stacking_unsupported_primitive c ->
      Format.fprintf
        f
        "@[<hv>unsupported primitive %a@]"
        Ligo_prim.Constant.pp_constant'
        c
    | `Stacking_corner_case (loc, msg) ->
      let s =
        Format.asprintf
          "Stacking corner case at %s : %s.\n%s"
          loc
          msg
          (corner_case_msg ())
      in
      Format.pp_print_string f s
    | `Stacking_contract_entrypoint loc ->
      let s =
        Format.asprintf "contract entrypoint must be given as a literal string: %s" loc
      in
      Format.pp_print_string f s
    | `Stacking_bad_iterator cst ->
      let s = Format.asprintf "bad iterator: iter %a" Mini_c.PP.constant cst in
      Format.pp_print_string f s
    | `Stacking_not_comparable_pair_struct ->
      let s =
        "Invalid comparable value. When using a tuple with more than 2 components, \
         structure the tuple like this: \"(a, (b, c))\". "
      in
      Format.pp_print_string f s
    | `Stacking_could_not_tokenize_michelson code ->
      Format.fprintf f "Could not tokenize raw Michelson: %s" code
    | `Stacking_could_not_parse_michelson code ->
      Format.fprintf f "Could not parse raw Michelson: %s" code
    | `Stacking_untranspilable (ty, value) ->
      Format.fprintf
        f
        "Could not untranspile Michelson value: %a %a"
        Michelson.pp
        ty
        Michelson.pp
        value)


let error_json : stacking_error -> Simple_utils.Error.t =
 fun e ->
  let open Simple_utils.Error in
  match e with
  | `Stacking_unsupported_primitive c ->
    let message =
      Format.asprintf "@[<hv>unsupported primitive %a@]" Ligo_prim.Constant.pp_constant' c
    in
    let content = make_content ~message () in
    make ~stage ~content
  | `Stacking_corner_case (loc, msg) ->
    let message =
      Format.asprintf "Stacking corner case at %s : %s.\n%s" loc msg (corner_case_msg ())
    in
    let content = make_content ~message () in
    make ~stage ~content
  | `Stacking_contract_entrypoint loc ->
    let message =
      Format.asprintf "contract entrypoint must be given as a literal string: %s" loc
    in
    let content = make_content ~message () in
    make ~stage ~content
  | `Stacking_bad_iterator cst ->
    let message = Format.asprintf "bad iterator: iter %a" Mini_c.PP.constant cst in
    let content = make_content ~message () in
    make ~stage ~content
  | `Stacking_not_comparable_pair_struct ->
    let message =
      "Invalid comparable value. When using a tuple with more than 2 components, \
       structure the tuple like this: \"(a, (b, c))\". "
    in
    let content = make_content ~message () in
    make ~stage ~content
  | `Stacking_could_not_tokenize_michelson code ->
    let message = Format.asprintf "Could not tokenize raw Michelson: %s" code in
    let content = make_content ~message () in
    make ~stage ~content
  | `Stacking_could_not_parse_michelson code ->
    let message = Format.asprintf "Could not parse raw Michelson: %s" code in
    let content = make_content ~message () in
    make ~stage ~content
  | `Stacking_untranspilable (ty, value) ->
    let message =
      Format.asprintf
        "Could not untranspile Michelson value: %a %a"
        Michelson.pp
        ty
        Michelson.pp
        value
    in
    let content = make_content ~message () in
    make ~stage ~content
