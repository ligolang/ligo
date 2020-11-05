open Trace
open Ast_typed.Types

type 'typeVariable t = unit
let create_state ~cmp:_ = ()
let add_constraint _repr () _constraint = ()
let remove_constraint _repr () _constraint = ok ()
let merge_aliases : 'old 'new_ . ('old, 'new_) merge_keys -> 'old t -> 'new_ t =
  fun _merge_keys state -> state

