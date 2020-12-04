open Ast_typed.Types

type 'typeVariable t = (constraint_identifier, constraint_identifier) PolyMap.t
let create_state ~cmp:_ =
  PolyMap.create ~cmp:Ast_typed.Compare.constraint_identifier
let add_constraint _ = failwith "todo"
let remove_constraint _ = failwith "todo"
let merge_aliases : 'old 'new_ . ('old, 'new_) merge_keys -> 'old t -> 'new_ t =
  fun _merge_keys state -> state

