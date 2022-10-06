module Region = Simple_utils.Region
module Attr = Lexing_shared.Attr

let decompile_attributes lst =
  let f : string -> Attr.t Region.reg = fun str ->
    match String.split ~on:':' str with
    | [k;v] -> Region.wrap_ghost (k, Some (Attr.String v))
    | [v] -> Region.wrap_ghost (v, None)
    | _ -> failwith "wrong attribute, expect 'key:value' or 'value'" (* TODO: Use the string as key *)
  in
  List.map ~f lst

