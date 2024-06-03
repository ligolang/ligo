open Ast_unified
open Pass_type
open Errors
module Trace = Simple_utils.Trace
module Location = Simple_utils.Location
include Flag.No_arg ()

let compile ~raise =
  let expr : (expr, ty_expr, pattern, _, _) expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_block_poly_fun { type_params; parameters = []; ret_type; body } ->
      let parameters : pattern Param.t list =
        [ { param_kind = `Const; pattern = make_p ~loc:Location.generated P_unit } ]
      in
      e_block_poly_fun ~loc { type_params; parameters; ret_type; body }
    | E_poly_fun { type_params; parameters = []; ret_type; body } ->
      let parameters : pattern Param.t list =
        [ { param_kind = `Const; pattern = make_p ~loc:Location.generated P_unit } ]
      in
      e_poly_fun ~loc { type_params; parameters; ret_type; body }
    | e -> make_e ~loc e
  in
  Fold { idle_fold with expr }


let reduction ~(raise : _ Trace.raise) =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content =
            E_poly_fun { parameters = []; _ } | E_block_poly_fun { parameters = []; _ }
        ; _
        } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__
let decompile ~raise:_ = Nothing (* TODO *)
