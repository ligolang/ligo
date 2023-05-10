open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location
include Flag.No_arg ()

let name = __MODULE__

let compile ~raise:_ =
  let expr : (expr, ty_expr, pattern, _, _) expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_tuple prod ->
      let rows =
        List.mapi
          ~f:(fun i expr -> Field.Complete (Label.of_int i, expr))
          (List.Ne.to_list prod)
      in
      e_record_pun ~loc rows
    | e -> make_e ~loc e
  in
  let ty_expr : ty_expr ty_expr_ -> ty_expr =
   fun ty ->
    let loc = Location.get_location ty in
    match Location.unwrap ty with
    | T_prod prod ->
      let rows =
        List.mapi
          ~f:(fun i ty ->
            ( Label.of_int i
            , Non_linear_rows.{ associated_type = Some ty; attributes = []; decl_pos = i }
            ))
          (List.Ne.to_list prod)
      in
      t_record_raw ~loc rows
    | ty -> make_t ~loc ty
  in
  Fold { idle_fold with expr; ty_expr }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_tuple _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  ; ty_expr =
      (function
      | { wrap_content = T_prod _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let decompile ~raise:_ = Nothing
