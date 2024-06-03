open Ast_unified
open Pass_type
open Errors
open Unit_test_helpers
module Trace = Simple_utils.Trace
module Location = Simple_utils.Location
include Flag.No_arg ()

(* handles unpunning of record pattern, associating the field to a
   variable of the same name *)

let name = __MODULE__

let compile ~raise:_ =
  let pattern : _ pattern_ -> pattern =
   fun p ->
    let loc = Location.get_location p in
    match Location.unwrap p with
    | P_pun_record fields ->
      let open Field in
      let fields =
        List.map fields ~f:(function
            | Complete x -> Complete x
            | Punned { wrap_content = label; location = loc } ->
              let pvar =
                p_var ~loc (Variable.of_input_var ~loc (Label.to_string label))
              in
              Complete (label, pvar))
      in
      p_pun_record ~loc fields
    | e -> make_p ~loc e
  in
  Fold { idle_fold with pattern }


let reduction ~(raise : _ Trace.raise) =
  { Iter.defaults with
    pattern =
      (function
      | { wrap_content = P_pun_record lst; _ } when List.exists lst ~f:Field.is_pun ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let decompile ~raise:_ = Nothing

let%expect_test _ =
  Pattern.(
    {|
      (P_pun_record
        ((Punned (Label a (Virtual generated))) (Punned (Label b (Virtual generated))))) |}
    |-> compile;
    [%expect
      {|
      (P_pun_record
       ((Complete ((Label a (Virtual generated)) (P_var a)))
        (Complete ((Label b (Virtual generated)) (P_var b)))))|}])
