open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

(* Conversion of JsLIGO pattern matching (mimicking TC-39) to
   caml-styled pattern matching. The only sensible difference
   is the explicit default case *)
include Flag.No_arg ()

let name = __MODULE__
let wild_case rhs = Case.{ pattern = None; rhs }

let compile ~raise =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    let same = make_e ~loc e.wrap_content in
    match Location.unwrap e with
    | E_match_tc39 { subject; match_clauses = DefaultClause expr } ->
      e_match ~loc { expr = subject; cases = List.Ne.singleton (wild_case expr) }
    | E_match_tc39 { subject; match_clauses = AllClauses (clauses, default_opt) } ->
      let clauses =
        List.Ne.map
          (fun Match_tc39.{ filter; clause_expr } ->
            Case.{ pattern = Some filter; rhs = clause_expr })
          clauses
      in
      let cases =
        Option.value_map default_opt ~default:clauses ~f:(fun expr ->
            List.Ne.append clauses (List.Ne.singleton @@ wild_case expr))
      in
      e_match ~loc { expr = subject; cases }
    | _ -> same
  in
  Fold { idle_fold with expr }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_match_tc39 _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let decompile ~raise:_ = Nothing
