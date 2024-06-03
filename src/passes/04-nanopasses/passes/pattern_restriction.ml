open Ast_unified
open Pass_type
open Errors
module Trace = Simple_utils.Trace
module Location = Simple_utils.Location
include Flag.No_arg ()

(* Throw errors on currently unsupported patterns (literals, rests) *)

let compile ~(raise : _ Trace.raise) =
  let pattern : _ pattern_ -> pattern = function
    | { wrap_content = P_unit; location = loc } ->
      (* For some reason we need to annotate unit pattern with unit type.
        see https://gitlab.com/ligolang/ligo/-/issues/1700
      *)
      p_typed ~loc (tv_unit ~loc ()) (p_unit ~loc)
    | { wrap_content = P_literal _; _ } as p -> raise.error (unsupported_pattern_type p)
    | { wrap_content = P_rest _; _ } as p -> raise.error (unsupported_pattern_type p)
    | e -> make_p ~loc:(Location.get_location e) (Location.unwrap e)
  in
  Fold { idle_fold with pattern }


let name = __MODULE__
let decompile ~raise:_ = Nothing
let reduction ~raise:_ = Iter.defaults
