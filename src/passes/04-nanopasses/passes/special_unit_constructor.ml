open Ast_unified
open Pass_type
module Location = Simple_utils.Location
include Flag.No_arg ()

let name = __MODULE__

let is_unit e =
  match get_e_literal e with
  | Some Literal_unit -> true
  | _ -> false


let compile ~raise:_ =
  let expr : (expr, ty_expr, pattern, _, _) expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_applied_constructor { constructor; element }
      when Label.(equal constructor (of_string "Unit")) && is_unit element ->
      e_literal ~loc Literal_unit
    | e -> make_e ~loc e
  in
  Fold { idle_fold with expr }


let decompile ~raise:_ = Nothing
let reduction ~raise:_ = Iter.defaults
