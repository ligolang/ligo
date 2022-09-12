open Ligo_prim
module Region = Simple_utils.Region
module Attr = Lexing_shared.Attr

let binder_attributes_of_strings (ss : string list) : bool =
  if List.mem ~equal:String.equal ss "var" then
      true
  else if List.mem ~equal:String.equal ss "const" then
    false
  else
    false

let strings_of_binder_attributes
      (lang : [`CameLIGO | `ReasonLIGO | `PascaLIGO | `JsLIGO ])
      (binder : _ Binder.t) : string list =
  let pureligo =
    if Binder.is_mutable binder then
      ["var"] else []
  in
  let impureligo =
    if Binder.is_mutable binder then
      ["var"] else ["const"]
  in
  match lang with
  | `CameLIGO | `ReasonLIGO -> pureligo
  | `PascaLIGO | `JsLIGO -> impureligo

let decompile_attributes lst =
  let f : string -> Attr.t Region.reg = fun str ->
    match String.split ~on:':' str with
    | [k;v] -> Region.wrap_ghost (k, Some (Attr.String v))
    | [v] -> Region.wrap_ghost (v, None)
    | _ -> failwith "wrong attribute, expect 'key:value' or 'value'" (* TODO: Use the string as key *)
  in
  List.map ~f lst
