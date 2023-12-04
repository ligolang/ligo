open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils
open Errors
module Location = Simple_utils.Location
include Flag.No_arg ()

let compile ~raise =
  let instruction
      : (instruction, expr, pattern, statement, block) instruction_ -> instruction
    =
   fun i ->
    let loc = Location.get_location i in
    match Location.unwrap i with
    | I_for_of { index_kind; index; expr; for_stmt } ->
      let pattern =
        match get_p index with
        | P_var _ | P_var_esc _ -> index
        | P_tuple [ p1; p2 ] when is_p_var p1 && is_p_var p2 -> index
        | P_tuple_with_ellipsis
            [ { ellipsis = false; pattern = p1 }; { ellipsis = false; pattern = p2 } ]
          when is_p_var p1 && is_p_var p2 -> index
        | _ -> raise.error (unsupported_pattern_loop loc)
      in
      let block =
        make_b
          ~loc
          (match index_kind with
          | `Let ->
            let let_rhs =
              trace_option ~raise (unsupported_pattern_loop loc)
              @@ Combinators.expr_of_pattern_opt index
            in
            let extra =
              s_decl
                ~loc
                (d_var ~loc { type_params = None; pattern; rhs_type = None; let_rhs })
            in
            List.Ne.(cons extra (singleton for_stmt))
          | `Const -> List.Ne.singleton for_stmt)
      in
      i_for_in ~loc (ForAny { pattern; collection = expr; block })
    | x -> make_i ~loc x
  in
  Fold { idle_fold with instruction }


let reduction ~raise =
  { Iter.defaults with
    instruction =
      (function
      | { wrap_content = I_for_of _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__
let decompile ~raise:_ = Nothing (* for now ? *)
