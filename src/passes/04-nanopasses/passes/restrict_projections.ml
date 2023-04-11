open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location
open Simple_utils.Ligo_string

(* projection in Jsligo have different behaviors: x["a"] is a record access , and no map access is supported *)

let compile ~raise ~syntax =
  let pass_expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_proj { struct_; path = Selection.Component_expr rhs } ->
      (match get_e_literal rhs with
      | Some (Literal_int z) ->
        e_proj ~loc { struct_; path = Selection.Component_num (Z.to_string z, z) }
      | Some (Literal_string z) ->
        e_proj ~loc { struct_; path = Selection.FieldName (Label.of_string (extract z)) }
      | _ -> raise.error (unsupported_projection ({ fp = e } : expr)))
    | _ -> make_e ~loc e.wrap_content
  in
  if Syntax_types.equal syntax JsLIGO
  then `Cata { idle_cata_pass with expr = pass_expr }
  else `Cata idle_cata_pass


let reduction ~raise ~syntax =
  if Syntax_types.equal syntax JsLIGO
  then
    { Iter.defaults with
      expr =
        (function
        | { wrap_content = E_proj { path = Selection.Component_expr _; _ }; _ } ->
          raise.error (wrong_reduction __MODULE__)
        | _ -> ())
    }
  else Iter.defaults


let pass ~raise ~syntax =
  morph
    ~name:__MODULE__
    ~compile:(compile ~raise ~syntax)
    ~decompile:`None
    ~reduction_check:(reduction ~raise ~syntax)
