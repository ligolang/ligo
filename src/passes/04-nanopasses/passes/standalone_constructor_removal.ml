(*
  After the [Constructor_application] pass,
  there are sill remaining [E_constr]/[P_ctor],
  those are the constructor which were not wrapped in a [E_ctor_app].

  This pass transforms them to [E_applied_constructor] so that
  there is only one node for constructors,
  [E_constr] should be completely removed after this pass.
*)

open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

let compile =
  let expr : (expr, ty_expr, pattern, _, _) expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_constr constructor ->
      let element = e_unit ~loc in
      e_applied_constructor ~loc { constructor; element }
    | e -> make_e ~loc e
  in
  let pattern : _ pattern_ -> pattern =
   fun p ->
    let loc = Location.get_location p in
    match Location.unwrap p with
    | P_ctor constructor -> p_variant ~loc constructor None
    | p -> make_p ~loc p
  in
  `Cata { idle_cata_pass with expr; pattern }


let decompile =
  let expr : (expr, ty_expr, pattern, _, _) expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_applied_constructor { constructor; element } ->
      (match get_e element with
      | E_literal Literal_unit -> e_constr ~loc constructor
      | e -> make_e ~loc e)
    | e -> make_e ~loc e
  in
  let pattern : _ pattern_ -> pattern =
   fun p ->
    let loc = Location.get_location p in
    match Location.unwrap p with
    | P_variant (x, None) -> p_ctor ~loc x
    | p -> make_p ~loc p
  in
  `Cata { idle_cata_pass with expr; pattern }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_constr _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  ; pattern =
      (function
      | { wrap_content = P_ctor _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise =
  morph ~name:__MODULE__ ~compile ~decompile ~reduction_check:(reduction ~raise)
