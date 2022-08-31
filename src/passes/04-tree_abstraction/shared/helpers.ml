open Ligo_prim
module Region = Simple_utils.Region
module Attr = Lexing_shared.Attr

let binder_attributes_of_strings (ss : string list) : Binder.binder_attributes =
  let open Binder in
  if List.mem ~equal:String.equal ss "var" then
      var_attribute
  else if List.mem ~equal:String.equal ss "const" then
    const_attribute
  else
    empty_attribute

let strings_of_binder_attributes
      (lang : [`CameLIGO | `ReasonLIGO | `PascaLIGO | `JsLIGO ])
      (attributes : Binder.binder_attributes) : string list =
  let pureligo {Binder.const_or_var} =
    match const_or_var with
                | Some `Var -> ["var"]
                | _ -> [] in
  let impureligo {Binder.const_or_var} =
    match const_or_var with
                | Some `Var -> ["var"]
                | Some `Const -> ["const"]
                | _ -> [] in
  match lang with
  | `CameLIGO | `ReasonLIGO -> pureligo attributes
  | `PascaLIGO | `JsLIGO -> impureligo attributes

let decompile_attributes lst =
  let f : string -> Attr.t Region.reg = fun str ->
    match String.split ~on:':' str with
    | [k;v] -> Region.wrap_ghost (k, Some (Attr.String v))
    | [v] -> Region.wrap_ghost (v, None)
    | _ -> failwith "wrong attribute, expect 'key:value' or 'value'" (* TODO: Use the string as key *)
  in
  List.map ~f lst
