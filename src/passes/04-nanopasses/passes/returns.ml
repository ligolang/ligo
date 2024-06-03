open Ast_unified
open Pass_type
open Errors
module Trace = Simple_utils.Trace
module Location = Simple_utils.Location
module Ligo_fun = Simple_utils.Ligo_fun
include Flag.No_arg ()

let ( <@ ) = Ligo_fun.( <@ )

(* warns about unreachable code and restrict returns in unsupported instructions (loops) *)

let name = __MODULE__

let is_s_return_or_break : statement -> bool =
 fun s ->
  match get_s_instr s with
  | Some i -> is_i_return i || is_i_break i
  | None -> false


let unreachable_code ~(raise : _ Trace.raise) : _ block_ -> unit =
 fun block ->
  let stmts = Location.unwrap block in
  if Nonempty_list.length stmts > 1
  then (
    let _, aft =
      List.split_while (Nonempty_list.to_list stmts) ~f:(not <@ is_s_return_or_break)
    in
    match aft with
    | [] | [ _ ] -> ()
    | _ :: unreachable ->
      raise.warning
        (`Jsligo_unreachable_code
          (unreachable
          |> List.map ~f:get_s_loc
          |> List.fold ~init:Location.generated ~f:Location.cover)))


let compile ~(raise : _ Trace.raise) =
  let block : _ block_ -> block =
   fun b ->
    unreachable_code ~raise b;
    make_b ~loc:b.location b.wrap_content
  in
  let instruction : (_, _, _, statement, block) instruction_ -> instruction = function
    | { wrap_content =
          ( I_while { block; _ }
          | I_for { block; _ }
          | I_for_of
              { for_stmt =
                  { fp =
                      { wrap_content =
                          S_instr { fp = { wrap_content = I_block block; _ } }
                      ; _
                      }
                  }
              ; _
              }
          | I_for_in (ForMap { block; _ })
          | I_for_in (ForSetOrList { block; _ }) ) as i
      ; location = loc
      } ->
      let block = get_b block in
      if List.exists ~f:is_s_return_or_break (Nonempty_list.to_list block)
      then raise.error (unsupported_return (Nonempty_list.to_list block))
      else make_i ~loc i
    | { wrap_content; location = loc } -> make_i ~loc wrap_content
  in
  let expr : _ expr_ -> expr = function
    | { wrap_content = E_block_with { block; expr = _ } as e; location = loc } ->
      let block = get_b block in
      if List.exists ~f:is_s_return_or_break (Nonempty_list.to_list block)
      then raise.error (unsupported_return (Nonempty_list.to_list block))
      else make_e ~loc e
    | { wrap_content; location = loc } -> make_e ~loc wrap_content
  in
  Fold { idle_fold with instruction; block; expr }


let decompile ~raise:_ = Nothing
let reduction ~raise:_ = Iter.defaults
