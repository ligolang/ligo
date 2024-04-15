open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location
include Flag.No_arg ()

let name = __MODULE__

let compile ~raise:_ =
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
  Fold { idle_fold with ty_expr }


let reduction ~raise =
  { Iter.defaults with
    ty_expr =
      (function
      | { wrap_content = T_prod _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let decompile ~raise:_ = Nothing
