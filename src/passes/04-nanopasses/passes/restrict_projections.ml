open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location
open Simple_utils.Ligo_string

(* projection in Jsligo have different behaviors: x["a"] is a record access , and no map access is supported *)

include Flag.No_arg ()

let compile ~raise =
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
  Fold { idle_fold with expr = pass_expr }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_proj { path = Selection.Component_expr _; _ }; _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__
let decompile ~raise:_ = Nothing
