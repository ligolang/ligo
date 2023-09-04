open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

(* This pass should forbid break/continue outside switch statements
*)
include Flag.No_arg ()

let compile ~raise =
  let switch_to_unit =
    let instruction
        : (instruction, expr, pattern, statement, block) Types.instruction_ -> instruction
      =
     fun i ->
      let loc = Location.get_location i in
      let i = Location.unwrap i in
      match i with
      | I_switch { subject; cases } ->
        let cases =
          let update_block b =
            let b_loc = get_b_loc b in
            let b = get_b b in
            let f (s : statement) =
              let s_loc = get_s_loc s in
              let s = get_s s in
              match s with
              | S_instr i ->
                let i_loc = get_i_loc i in
                let i = get_i i in
                let i =
                  match i with
                  | I_break -> I_skip
                  | _ -> i
                in
                let i = make_i ~loc:i_loc i in
                make_s ~loc:s_loc (S_instr i)
              | _ -> make_s ~loc:s_loc s
            in
            let b = List.Ne.map f b in
            let b = make_b ~loc:b_loc b in
            b
          in
          match cases with
          | AllCases (c, d) ->
            let c =
              List.Ne.map
                (fun x ->
                  Switch.{ x with case_body = Option.map ~f:update_block x.case_body })
                c
            in
            Switch.AllCases (c, Option.map ~f:(Option.map ~f:update_block) d)
          | Default b -> Switch.Default (Option.map ~f:update_block b)
        in
        make_i ~loc (I_switch { subject; cases })
      | _ -> make_i ~loc i
    in
    Fold { Morphing.idle_fold with instruction }
  in
  let filter_continue_break =
    let instruction
        : (instruction, expr, pattern, statement, block) Types.instruction_ -> unit
      =
     fun i ->
      let loc = Location.get_location i in
      let i = Location.unwrap i in
      match i with
      | I_break -> raise.error (unsupported_break loc)
      | _ -> ()
    in
    Check { Iter.defaults with instruction }
  in
  (* turn all the good break to unit; throw on any remaining break and ignore the remaining tree *)
  Ignore (Seq (switch_to_unit, filter_continue_break))


let name = __MODULE__
let decompile ~raise:_ = Nothing
let reduction ~raise:_ = Iter.defaults
