open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

let compile =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_cond { test; ifso; ifnot } ->
      let cases =
        List.Ne.of_list
          [ Case.
              { pattern = p_variant ~loc:(get_e_loc ifso) (Label.of_string "True") None
              ; rhs = ifso
              }
          ; Case.
              { pattern = p_variant ~loc:(get_e_loc ifso) (Label.of_string "False") None
              ; rhs = Option.value_map ifnot ~default:(e_unit ~loc) ~f:Fun.id
              }
          ]
      in
      e_match ~loc { expr = test; cases }
    | e -> make_e ~loc e
  in
  `Cata { idle_cata_pass with expr }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_cond _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise =
  morph ~name:__MODULE__ ~compile ~decompile:`None ~reduction_check:(reduction ~raise)
