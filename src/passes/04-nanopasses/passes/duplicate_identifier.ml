open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

let check_for_duplicated ~raise b =
  List.iter b ~f:(fun bound ->
      let dups = List.find_a_dup ~compare:Variable.compare (List.rev bound) in
      match dups with
      | Some v -> raise.error (duplicate_identifier v)
      | None -> ())


let compile ~raise ~enable =
  let program : _ program_ -> unit =
   fun prg -> check_for_duplicated ~raise @@ Bound_vars.bound_program { fp = prg }
  in
  let expr : _ expr_ -> unit =
   fun e -> check_for_duplicated ~raise @@ Bound_vars.bound_expr { fp = e }
  in
  let block : _ block_ -> unit =
   fun b -> check_for_duplicated ~raise @@ Bound_vars.bound_block { fp = b }
  in
  if enable
  then `Check { Iter.defaults with program; expr; block }
  else `Check Iter.defaults


let pass ~raise ~enable =
  morph
    ~name:__MODULE__
    ~compile:(compile ~raise ~enable)
    ~decompile:`None
    ~reduction_check:Iter.defaults
