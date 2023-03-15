module Region = Simple_utils.Region
module Attr   = Lexing_shared.Attr
module Wrap   = Lexing_shared.Wrap

let decompile_attributes lst =
  let f : string -> Attr.t Wrap.t =
   fun str ->
    match String.split ~on:':' str with
    | [ k; v ] -> Wrap.ghost (k, Some (Attr.String v))
    | [ v ] -> Wrap.ghost (v, None)
    | _ ->
      failwith
        "wrong attribute, expect 'key:value' or 'value'" (* TODO: Use the string as key *)
  in
  List.map ~f lst
