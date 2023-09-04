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
    let f (s : expr Selection.t) : expr Selection.t =
      match s with
      | Component_expr rhs ->
        (match get_e_literal rhs with
        | Some (Literal_int z) -> Selection.Component_num (Z.to_string z, z)
        | Some (Literal_string z) -> Selection.FieldName (Label.of_string (extract z))
        | _ -> raise.error (unsupported_projection ({ fp = e } : expr)))
      | _ -> s
    in
    match Location.unwrap e with
    | E_proj (struct_, path) ->
      let path = List.map ~f path in
      e_proj ~loc struct_ path
    | _ -> make_e ~loc e.wrap_content
  in
  Fold { idle_fold with expr = pass_expr }


let reduction ~raise =
  let is_component_expr (t : _ Selection.t) =
    match t with
    | Component_expr _ -> true
    | _ -> false
  in
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_proj (struct_, path); _ }
        when List.exists ~f:is_component_expr path ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__
let decompile ~raise:_ = Nothing
