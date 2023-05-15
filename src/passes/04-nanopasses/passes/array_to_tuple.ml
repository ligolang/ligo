open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location
include Flag.No_arg ()

let compile ~raise =
  let expr : (expr, ty_expr, pattern, _, _) expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_array elements ->
      (match elements with
      | [] -> e_unit ~loc
      | hd :: tl ->
        let f = function
          | Array_repr.Expr_entry e -> e
          | Rest_entry e -> raise.error @@ unsupported_rest_property e
        in
        e_tuple ~loc (List.Ne.map f (hd, tl)))
    | e -> make_e ~loc e
  in
  Fold { idle_fold with expr }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_array _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__
let decompile ~raise:_ = Nothing (* TODO *)
