open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils
open Errors
module Location = Simple_utils.Location

let compile =
  let instruction : _ instruction_ -> instruction =
   fun i ->
    let loc = Location.get_location i in
    match Location.unwrap i with
    | I_for_of { index_kind; index; expr; for_stmt } ->
      let index_loc = Variable.get_location index in
      let pattern = p_var ~loc:index_loc index in
      let block =
        make_b
          ~loc
          (match index_kind with
          | `Let ->
            let extra =
              s_decl
                ~loc
                (d_var
                   ~loc
                   { type_params = None
                   ; pattern
                   ; rhs_type = None
                   ; let_rhs = e_variable ~loc:index_loc index
                   })
            in
            List.Ne.(cons extra (singleton for_stmt))
          | `Const -> List.Ne.singleton for_stmt)
      in
      i_for_in ~loc (ForAny { pattern; collection = expr; block })
    | x -> make_i ~loc x
  in
  `Cata { idle_cata_pass with instruction }


let reduction ~raise =
  { Iter.defaults with
    instruction =
      (function
      | { wrap_content = I_for_of _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise =
  morph
    ~name:__MODULE__
    ~compile
    ~decompile:`None (* for now ? *)
    ~reduction_check:(reduction ~raise)
