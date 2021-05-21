open Types
open Trace
open Function

(* Types level *)

let type_app : ('acc -> 'a -> ('acc, _) result) -> 'acc -> 'a type_app -> ('acc, _) result
= fun g acc {type_operator=_;arguments} ->
  let* acc = bind_fold_list g acc arguments in
  ok @@ acc

let rows : ('acc -> 'a -> ('acc,_) result) -> 'acc -> 'a rows -> ('acc,_) result
= fun g acc {fields;attributes=_} ->
  Helpers.bind_fold_lmap
  (fun acc _ {associated_type;attributes=_;decl_pos=_} ->
    g acc associated_type
  ) acc fields

let arrow : ('acc -> 'a -> ('acc,_) result) -> 'acc -> 'a arrow -> ('acc,_) result
= fun g acc {type1;type2} ->
  let* acc = g acc type1 in
  let* acc = g acc type2 in
  ok @@ acc

(* Expression level *)

let constant : ('acc -> 'a ->  ('acc,_) result) -> 'acc -> 'a constant -> ('acc,_) result
= fun f acc {cons_name=_;arguments} ->
  let* acc = bind_fold_list f acc arguments in
  ok @@ acc

let constructor : ('acc -> 'a -> ('acc,_) result) -> 'acc -> 'a constructor -> ('acc,_) result
= fun f acc {constructor=_;element} ->
  let* acc = f acc element in
  ok @@ acc

let application : ('acc -> 'a -> ('acc,_) result) -> 'acc -> 'a application -> ('acc,_) result
= fun f acc {lamb;args} ->
  let* acc = f acc lamb in
  let* acc = f acc args in
  ok @@ acc

let option f acc = map (Option.unopt ~default:acc) <@ bind_map_option (f acc)

let binder : ('acc -> 'a -> ('acc, _) result) -> 'acc -> 'a binder -> ('acc, _) result
= fun f acc {var=_; ascr; attributes=_} ->
  let* acc = option f acc ascr in
  ok @@ acc

let let_in : ('acc -> 'a -> ('acc, _) result) -> ('acc -> 'c -> ('acc, _) result) -> 'acc -> ('a,'c) let_in -> ('acc , _) result
= fun f g acc { let_binder; rhs ; let_result; attributes=_} ->
  let* acc = binder g acc let_binder in
  let* acc = f acc rhs in
  let* acc = f acc let_result in
  ok @@ acc

let type_in : ('acc -> 'a -> ('acc, _) result) -> ('acc -> 'c -> ('acc, _) result) -> 'acc -> ('a,'c) type_in -> ('acc , _) result
= fun f g acc { type_binder=_; rhs ; let_result} ->
  let* acc = g acc rhs in
  let* acc = f acc let_result in
  ok @@ acc

let lambda : ('acc -> 'a -> ('acc, _) result) -> ('acc -> 'c -> ('acc, _) result) -> 'acc -> ('a,'c) lambda -> ('acc , _) result
= fun f g acc {binder=b;output_type;result}->
  let* acc = binder g acc b in
  let* acc = option g acc output_type in
  let* acc = f acc result in
  ok @@ acc

let path : ('acc -> 'a -> ('acc,_) result) -> 'acc -> 'a access list -> ('acc, _) result
= fun f acc path ->
  let aux acc a = match a with
    | Access_record _ -> ok @@ acc
    | Access_tuple  _ -> ok @@ acc
    | Access_map e ->
      let* acc = f acc e in
      ok @@ acc
  in
  bind_fold_list aux acc path

let record : ('acc -> 'a -> ('acc,_) result) -> 'acc -> 'a label_map -> ('acc,_) result
= fun f acc record ->
  Helpers.bind_fold_lmap (
    fun acc _ a -> f acc a
  ) acc record

let tuple : ('acc -> 'a -> ('acc,_) result) -> 'acc -> 'a list -> ('acc,_) result
= fun f acc record ->
  bind_fold_list f acc record

let recursive : ('acc -> 'a -> ('acc,_) result) -> ('acc -> 'c -> ('acc,_) result) -> 'acc -> ('a,'c) recursive -> ('acc, _) result
= fun f g acc {fun_name=_;fun_type;lambda=l} ->
  let* acc = g acc fun_type in
  let* acc = lambda f g acc l in
  ok @@ acc

let accessor : ('acc -> 'a -> ('b,_) result) -> 'acc -> 'a accessor -> ('acc, _) result
= fun f acc {record;path=p} ->
  let* acc = f acc record in
  let* acc = path f acc p in
  ok @@ acc

let record_accessor : ('acc -> 'a -> ('b,_) result) -> 'acc -> 'a record_accessor -> ('acc, _) result
= fun f acc {record;path=_} ->
  let* acc = f acc record in
  ok @@ acc

let update : ('acc -> 'a -> ('acc,_) result) -> 'acc -> 'a update -> ('acc, _) result
= fun f acc {record;path=p;update} ->
  let* acc = f acc record in
  let* acc = path f acc p in
  let* acc = f acc update in
  ok @@ acc

let record_update : ('acc -> 'a -> ('acc,_) result) -> 'acc -> 'a record_update -> ('acc, _) result
= fun f acc {record;path=_;update} ->
  let* acc = f acc record in
  let* acc = f acc update in
  ok @@ acc

let sequence : ('acc -> 'a -> ('acc,_) result) -> 'acc -> 'a sequence -> ('acc, _) result
= fun f acc {expr1;expr2} ->
  let* acc = f acc expr1 in
  let* acc = f acc expr2 in
  ok @@ acc

let ascription : ('acc -> 'a -> ('acc,_) result) -> ('acc -> 'c -> ('acc,_) result) -> 'acc -> ('a,'c) ascription -> ('acc, _) result
= fun f g acc {anno_expr; type_annotation} ->
  let* acc = f acc anno_expr in
  let* acc = g acc type_annotation in
  ok @@ acc

let raw_code : ('acc -> 'a -> ('acc,_) result) -> 'acc -> 'a raw_code -> ('acc, _) result
= fun f acc {language=_;code} ->
  let* acc = f acc code in
  ok @@ acc

let conditional : ('acc -> 'a -> ('acc,_) result) -> 'acc -> 'a conditional -> ('acc, _) result
= fun f acc {condition;then_clause;else_clause} ->
  let* acc = f acc condition in
  let* acc = f acc then_clause in
  let* acc = f acc else_clause in
  ok @@ acc

let assign : ('acc -> 'a -> ('b,_) result) -> 'acc -> 'a assign -> ('acc, _) result
= fun f acc {variable=_; access_path; expression} ->
  let* acc = path f acc access_path in
  let* acc = f acc expression in
  ok @@ acc

let for_ : ('acc -> 'a -> ('b,_) result) -> 'acc -> 'a for_ -> ('acc, _) result
= fun f acc {binder=_;start;final;incr;f_body} ->
  let* acc = f acc start in
  let* acc = f acc final in
  let* acc = f acc incr in
  let* acc = f acc f_body in
  ok @@ acc

let for_each : ('acc -> 'a -> ('b,_) result) -> 'acc -> 'a for_each -> ('acc, _) result
= fun f acc {fe_binder=_;collection;collection_type=_;fe_body} ->
  let* acc = f acc collection in
  let* acc = f acc fe_body in
  ok @@ acc

let while_loop : ('acc -> 'a -> ('b,_) result) -> 'acc -> 'a while_loop -> ('acc, _) result
= fun f acc {cond; body} ->
  let* acc = f acc cond in
  let* acc = f acc body in
  ok @@ acc

(* Declaration *)
let declaration_type : ('acc -> 'a -> ('acc, _) result) -> 'acc -> 'a declaration_type -> ('acc, _) result
= fun g acc {type_binder=_; type_expr} ->
  let* acc = g acc type_expr in
  ok @@ acc

let declaration_constant : ('acc -> 'a -> ('acc,_) result) -> ('acc -> 'b -> ('acc,_) result) -> 'acc -> ('a,'b) declaration_constant -> ('acc, _) result
= fun f g acc {name = _; binder=b; attr=_; expr} ->
  let* acc = binder g acc b in
  let* acc = f acc expr     in
  ok @@ acc

let rec declaration_module : ('acc -> 'a -> ('acc,_) result) -> ('acc -> 'b -> ('acc,_) result) -> 'acc -> ('a,'b) declaration_module -> ('acc, _) result
= fun f g acc {module_binder=_;module_} ->
  let* acc = module' f g acc module_ in
  ok @@ acc

and module_alias
= fun acc _ ->
  ok @@ acc

and declaration : ('acc -> 'a -> ('acc,_) result) -> ('acc -> 'b -> ('acc,_) result) -> 'acc -> ('a,'b) declaration' -> ('acc,_) result
= fun f g acc -> function
  Declaration_type    ty -> declaration_type       g acc ty
| Declaration_constant c -> declaration_constant f g acc c
| Declaration_module   m -> declaration_module   f g acc m
| Module_alias        ma -> module_alias             acc ma

and module' : ('acc -> 'a -> ('acc,_) result) -> ('acc -> 'b -> ('acc,_) result) -> 'acc -> ('a,'b) module' -> ('acc, _) result
= fun f g acc prg ->
  bind_fold_list (bind_fold_location (declaration f g)) acc prg

let mod_in : ('acc -> 'a -> ('acc, _) result) -> ('acc -> 'c -> ('acc, _) result) -> 'acc -> ('a,'c) mod_in -> ('acc , _) result
= fun f g acc { module_binder=_; rhs ; let_result} ->
  let* acc = (module' f g) acc rhs in
  let* acc = f acc let_result in
  ok @@ acc

let mod_alias : ('acc -> 'a -> ('acc, _) result) -> 'acc -> 'a mod_alias -> ('acc , _) result
= fun f acc { alias=_; binders=_; result} ->
  let* acc = f acc result in
  ok @@ acc
