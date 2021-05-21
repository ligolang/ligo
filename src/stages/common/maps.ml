open Types
open Trace

let module_access : ('a -> ('b, _) result) -> 'a module_access -> ('b module_access, _) result
= fun f {module_name; element} ->
  let* element = f element in
  ok {module_name; element}

(* Types level *)

let type_app : ('a -> ('b, _) result) -> 'a type_app -> ('b type_app, _) result
= fun g {type_operator;arguments} ->
  let* arguments = bind_map_list g arguments in
  ok @@ {type_operator; arguments}

let rows : ('a -> ('b,_) result) -> 'a rows -> ('b rows,_) result
= fun g {fields; attributes} ->
  let* fields = Helpers.bind_map_lmap
  (fun {associated_type ; attributes ; decl_pos} ->
    let* associated_type = g associated_type in
    ok @@ ({associated_type ; attributes ; decl_pos}: 'b row_element)
  ) fields in
  ok @@ {fields; attributes}

let arrow : ('a -> ('b,_) result) -> 'a arrow -> ('b arrow,_) result
= fun g {type1;type2} ->
  let* type1 = g type1 in
  let* type2 = g type2 in
  ok @@ {type1;type2}

(* Expression level *)

let constant : ('a ->  ('b,_) result) -> 'a constant -> ('b constant,_) result
= fun f {cons_name;arguments} ->
  let* arguments = bind_map_list f arguments in
  ok @@ {cons_name;arguments}

let constructor : ('a -> ('b,_) result) -> 'a constructor -> ('b constructor,_) result
= fun f {constructor;element} ->
  let* element = f element in
  ok @@ {constructor; element}

let application : ('a -> ('b,_) result) -> 'a application -> ('b application,_) result
= fun f {lamb;args} ->
  let* lamb = f lamb in
  let* args = f args in
  ok @@ {lamb; args}

and binder : ('a -> ('b, _) result) -> 'a binder -> ('b binder, _) result
= fun f {var; ascr; attributes} ->
  let* ascr = bind_map_option f ascr in
  ok @@ {var; ascr; attributes}

let let_in :  ('a -> ('b, _) result) -> ('c -> ('d, _) result) -> ('a,'c) let_in -> (('b,'d) let_in, _) result
= fun f g {let_binder; rhs; let_result; attributes} ->
  let* let_binder = binder g let_binder in
  let* rhs        = f rhs in
  let* let_result = f let_result in
  ok @@ {let_binder; rhs; let_result; attributes}

let type_in :  ('a -> ('b, _) result) -> ('c -> ('d, _) result) -> ('a,'c) type_in -> (('b,'d) type_in, _) result
= fun f g {type_binder; rhs; let_result} ->
  let* rhs        = g rhs in
  let* let_result = f let_result in
  ok @@ {type_binder; rhs; let_result}

let lambda : ('a -> ('b, _) result) -> ('c -> ('d, _) result) -> ('a,'c) lambda -> (('b,'d) lambda , _) result
= fun f g {binder=b;output_type;result}->
  let* binder = binder g b in
  let* output_type = bind_map_option g output_type in
  let* result = f result in
  ok @@ {binder;output_type;result}

let path : ('a -> ('b,_) result) -> 'a access list -> ('b access list, _) result
= fun f path ->
  let aux a = match a with
    | Access_record s -> ok @@ Access_record s
    | Access_tuple  i -> ok @@ Access_tuple  i
    | Access_map e ->
      let* e = f e in
      ok @@ Access_map e
  in
  bind_map_list aux path

let record : ('a -> ('b,_) result) -> 'a label_map -> ('b label_map,_) result
= fun f record ->
  Helpers.bind_map_lmap f record

let recursive : ('a -> ('b,_) result) -> ('c -> ('d,_) result) -> ('a,'c) recursive -> (('b,'d) recursive, _) result
= fun f g {fun_name;fun_type;lambda=l} ->
  let* fun_type = g fun_type in
  let* lambda = lambda f g l in
  ok @@ {fun_name;fun_type;lambda}

let accessor : ('a -> ('b,_) result) -> 'a accessor -> ('b accessor, _) result
= fun f {record;path=p} ->
  let* record = f record in
  let* path   = path f p in
  ok @@ ({record;path} : 'b accessor)

let update : ('a -> ('b,_) result) -> 'a update -> ('b update, _) result
= fun f {record;path=p;update} ->
  let* record = f record in
  let* path   = path f p in
  let* update = f update in
  ok @@ ({record;path;update} : 'b update)

let record_accessor : ('a -> ('b,_) result) -> 'a record_accessor -> ('b record_accessor, _) result
= fun f {record;path} ->
  let* record = f record in
  ok @@ ({record;path} : 'b record_accessor)

let record_update : ('a -> ('b,_) result) -> 'a record_update -> ('b record_update, _) result
= fun f {record;path;update} ->
  let* record = f record in
  let* update = f update in
  ok @@ ({record;path;update} : 'b record_update)

let sequence : ('a -> ('b,_) result) -> 'a sequence -> ('b sequence, _) result
= fun f {expr1;expr2} ->
  let* expr1 = f expr1 in
  let* expr2 = f expr2 in
  ok @@ {expr1;expr2}

let ascription : ('a -> ('b,_) result) -> ('c -> ('d,_) result) -> ('a,'c) ascription -> (('b,'d) ascription, _) result
= fun f g {anno_expr; type_annotation} ->
  let* anno_expr = f anno_expr in
  let* type_annotation = g type_annotation in
  ok @@ {anno_expr; type_annotation}

let raw_code : ('a -> ('b,_) result) -> 'a raw_code -> ('b raw_code, _) result
= fun f {language;code} ->
  let* code = f code in
  ok @@ {language;code}

let conditional : ('a -> ('b,_) result) -> 'a conditional -> ('b conditional, _) result
= fun f {condition;then_clause;else_clause} ->
  let* condition   = f condition in
  let* then_clause = f then_clause in
  let* else_clause = f else_clause in
  ok @@ {condition;then_clause;else_clause}

let assign : ('a -> ('b,_) result) -> 'a assign -> ('b assign, _) result
= fun f {variable; access_path; expression} ->
  let* access_path = path f access_path in
  let* expression  = f expression in
  ok @@ {variable; access_path; expression}

let for_
= fun f {binder; start; final; incr; f_body} ->
  let* f_body = f f_body in
  ok @@ {binder; start; final; incr; f_body}

let for_each
= fun f {fe_binder; collection; collection_type; fe_body} ->
  let* collection = f collection in
  let* fe_body    = f fe_body in
  ok @@ {fe_binder; collection; collection_type; fe_body}

let while_loop
= fun f {cond; body} ->
  let* cond = f cond in
  let* body = f body in
  ok @@ {cond; body}

(* Declaration *)
let declaration_type : ('a -> ('b, _) result) -> 'a declaration_type -> ('b declaration_type, _) result
= fun g {type_binder; type_expr} ->
  let* type_expr = g type_expr in
  ok @@ {type_binder; type_expr}

let declaration_constant : ('a -> ('b,_) result) -> ('c -> ('d,_) result) -> ('a,'c) declaration_constant -> (('b,'d) declaration_constant, _) result
= fun f g {name; binder=b; attr; expr} ->
  let* binder = binder g b in
  let* expr   = f expr     in
  ok @@ {name;binder;attr;expr}

let rec declaration_module : ('a -> ('b,_) result) -> ('c -> ('d,_) result) -> ('a,'c) declaration_module -> (('b,'d) declaration_module, _) result
= fun f g {module_binder; module_} ->
  let* module_ = module' f g module_ in
  ok @@ {module_binder;module_}

and module_alias
= fun ma ->
  ok @@ ma

and declaration
= fun f g -> function
  Declaration_type    ty -> let* ty = declaration_type      g ty in ok @@ Declaration_type ty
| Declaration_constant c -> let* c  = declaration_constant f g c in ok @@ Declaration_constant c
| Declaration_module   m -> let* m  = declaration_module   f g m in ok @@ Declaration_module   m
| Module_alias        ma -> let* ma = module_alias            ma in ok @@ Module_alias        ma

and module' : ('a -> ('b,_) result) -> ('c -> ('d,_) result) -> ('a,'c) module' -> (('b,'d) module', _) result
= fun f g prg ->
  bind_map_list (bind_map_location (declaration f g)) prg

and mod_in :  ('a -> ('b, _) result) -> ('c -> ('d, _) result) -> ('a,'c) mod_in -> (('b,'d) mod_in, _) result
= fun f g {module_binder; rhs; let_result} ->
  let* rhs        = (module' f g) rhs in
  let* let_result = f let_result in
  ok @@ {module_binder; rhs; let_result}

and mod_alias :  ('a -> ('b, _) result) -> 'a mod_alias -> ('b mod_alias, _) result
= fun f {alias; binders; result} ->
  let* result = f result in
  ok @@ {alias; binders; result}
