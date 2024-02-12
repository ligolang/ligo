open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location
include Flag.No_arg ()

let compile ~raise:_ =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_cond { test; ifso; ifnot } ->
      let cases =
        List.Ne.of_list
          [ Case.
              { pattern =
                  Some (p_variant ~loc:(get_e_loc ifso) (Label.of_string "True") None)
              ; rhs = ifso
              }
          ; Case.
              { pattern =
                  Some (p_variant ~loc:(get_e_loc ifso) (Label.of_string "False") None)
              ; rhs = Option.value_map ifnot ~default:(e_unit ~loc) ~f:Fun.id
              }
          ]
      in
      let test =
        e_annot (test, tv_bool ~loc:Location.generated ()) ~loc:(get_e_loc test)
      in
      e_match ~loc { expr = test; disc_label = None; cases }
    | e -> make_e ~loc e
  in
  Fold { idle_fold with expr }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_cond _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__
let decompile ~raise:_ = Nothing
