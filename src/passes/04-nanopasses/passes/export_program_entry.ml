open Ast_unified
open Pass_type
open Errors
module Trace = Simple_utils.Trace
include Flag.No_arg ()

(* Upon exported declaration 'export', attribute "public" must be added *)

let compile ~raise:_ =
  let program_entry
      : (program_entry, declaration, instruction) program_entry_ -> program_entry
    =
   fun e ->
    match e with
    | PE_export pe -> pe_attr Attribute.{ key = "public"; value = None } pe
    | _ -> make_pe e
  in
  Fold { idle_fold with program_entry }


let decompile ~raise:_ =
  let program_entry : _ program_entry_ -> program_entry =
   fun e ->
    match e with
    | PE_attr (Attribute.{ key = "public"; value = None }, d) -> pe_export d
    | _ -> make_pe e
  in
  Fold { idle_fold with program_entry }


let reduction ~(raise : _ Trace.raise) =
  { Iter.defaults with
    program_entry =
      (function
      | PE_export _ -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__
