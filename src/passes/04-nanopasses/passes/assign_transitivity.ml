open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location
include Flag.No_arg ()

let computation ~loc v r op =
  let open Assign_chainable in
  let assign r =
    e_assign_unitary ~loc { binder = Ligo_prim.Binder.make v None; expression = r }
  in
  let res =
    match op with
    | Eq -> r
    | Assignment_operator x ->
      let computation cons_name =
        e_constant ~loc { cons_name; arguments = [ e_variable ~loc v; r ] }
      in
      let op : Ligo_prim.Constant.constant' =
        match x with
        | Times_eq -> C_MUL
        | Div_eq -> C_DIV
        | Min_eq -> C_POLYMORPHIC_SUB
        | Plus_eq -> C_POLYMORPHIC_ADD
        | Mod_eq -> C_MOD
      in
      computation op
  in
  assign res


let compile ~raise:_ =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_assign_chainable { var; op; rhs; returned } ->
      let assignment = computation ~loc var rhs op in
      let_unit_in assignment returned
    | e -> make_e ~loc e
  in
  Fold { idle_fold with expr }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_assign_chainable _; _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__
let decompile ~raise:_ = Nothing (* for now ? *)
