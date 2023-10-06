open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

(* Handling variables that can be escaped: x or @x *)

(* TODO: If a variable is escaped at the binding location, we should
   keep that information to be able to write the decompilation
   function for this pass *)

include Flag.No_arg ()

let esc = function
  | Value_escaped_var.Esc v | Raw v -> v


let esc_t = function
  | Ty_escaped_var.Esc v | Raw v -> v


let compile ~raise =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_variable_esc v -> e_variable ~loc (esc v)
    | _ -> make_e ~loc e.wrap_content
  in
  let ty_expr : _ ty_expr_ -> ty_expr =
   fun t ->
    let loc = Location.get_location t in
    match Location.unwrap t with
    | T_var_esc t -> t_var ~loc (esc_t t)
    | _ -> make_t ~loc t.wrap_content
  in
  let pattern : _ pattern_ -> pattern =
   fun p ->
    let loc = Location.get_location p in
    match Location.unwrap p with
    | P_var_esc p -> p_var ~loc (esc p)
    | _ -> make_p ~loc p.wrap_content
  in
  Fold { idle_fold with expr; ty_expr; pattern }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_variable_esc _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  ; ty_expr =
      (function
      | { wrap_content = T_var_esc _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  ; pattern =
      (function
      | { wrap_content = P_var_esc _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__
let decompile ~raise:_ = Nothing
