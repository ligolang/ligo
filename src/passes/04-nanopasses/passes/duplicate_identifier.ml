open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location
open Unit_test_helpers

(* This pass prevent shadowing in scopes (i.e. two bindings to the same variable in the same scope) *)
let name = __MODULE__

include Flag.No_arg ()

let check_for_duplicated ~raise b =
  List.iter b ~f:(fun bound ->
      let dups = List.find_a_dup ~compare:Variable.compare (List.rev bound) in
      match dups with
      | Some v when not (Variable.is_ignored v) -> raise.error (duplicate_identifier v)
      | _ -> ())


let compile ~raise =
  let program : _ program_ -> unit =
   fun prg -> check_for_duplicated ~raise @@ Bound_vars.bound_program { fp = prg }
  in
  let expr : _ expr_ -> unit =
   fun e -> check_for_duplicated ~raise @@ Bound_vars.bound_expr { fp = e }
  in
  let block : _ block_ -> unit =
   fun b -> check_for_duplicated ~raise @@ Bound_vars.bound_block { fp = b }
  in
  Check { Iter.defaults with program; expr; block }


let reduction ~raise:_ = Iter.defaults
let decompile ~raise:_ = Nothing

let%expect_test _ =
  Block.(
    {|
    ((S_decl
      (D_var ((pattern (P_var x)) (let_rhs (EXPR1)))))
     (S_decl
      (D_var ((pattern (P_var y)) (let_rhs (EXPR2)))))
     (S_decl
      (D_var ((pattern (P_var x)) (let_rhs (EXPR3)))))
     (S_instr (I_return ((EXPR4)))))
    |}
    |->! compile;
    [%expect {|
    Err : (Small_passes_duplicate_identifier x)
    |}])
