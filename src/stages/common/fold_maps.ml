open Types
open Trace

(* Types level *)

let type_app : ('acc -> 'a -> ('acc * 'b, _) result) -> 'acc -> 'a type_app -> ('acc *  'a type_app, _) result
= fun g acc {type_operator;arguments} ->
  let%bind acc,arguments = bind_fold_map_list g acc arguments in
  ok @@ (acc,{type_operator; arguments})

let rows : ('acc -> 'a -> ('acc * 'b,_) result) -> 'acc -> 'a rows -> ('acc * 'b rows,_) result
= fun g acc {fields;attributes} ->
  let%bind acc,fields = Helpers.bind_fold_map_lmap
  (fun acc _ {associated_type;attributes;decl_pos} ->
    let%bind acc,associated_type = g acc associated_type in
    ok @@ (acc,({associated_type;attributes;decl_pos}:'b row_element))
  ) acc fields in
  ok @@ (acc,{fields;attributes})

let arrow : ('acc -> 'a -> ('acc * 'b,_) result) -> 'acc -> 'a arrow -> ('acc * 'b arrow,_) result
= fun g acc {type1;type2} ->
  let%bind acc,type1 = g acc type1 in
  let%bind acc,type2 = g acc type2 in
  ok @@ (acc,{type1;type2})

(* Expression level *)

let constant : ('acc -> 'a ->  ('acc * 'b,_) result) -> 'acc -> 'a constant -> ('acc * 'b constant,_) result
= fun f acc {cons_name;arguments} ->
  let%bind acc,arguments = bind_fold_map_list f acc arguments in
  ok @@ (acc,{cons_name;arguments})

let constructor : ('acc -> 'a -> ('acc * 'b,_) result) -> 'acc -> 'a constructor -> ('acc * 'b constructor,_) result
= fun f acc {constructor;element} ->
  let%bind acc,element = f acc element in
  ok @@ (acc,{constructor; element})

let application : ('acc -> 'a -> ('acc * 'b,_) result) -> 'acc -> 'a application -> ('acc * 'b application,_) result
= fun f acc {lamb;args} ->
  let%bind acc,lamb = f acc lamb in
  let%bind acc,args = f acc args in
  ok @@ (acc,{lamb; args})

let option f acc = function
  Some ty ->
    let%bind acc,ty = f acc ty in
    ok @@ (acc,Some ty)
| None -> ok @@ (acc, None)

let binder : ('acc -> 'a -> ('acc * 'b, _) result) -> 'acc -> 'a binder -> ('acc * 'b binder, _) result
= fun f acc {var; ascr} ->
  let%bind acc,ascr = option f acc ascr in
  ok @@ (acc,{var; ascr})

let let_in :  ('acc -> 'a -> ('acc * 'b, _) result) -> ('acc -> 'c -> ('acc * 'd, _) result) -> 'acc -> ('a,'c) let_in -> ('acc * ('b,'d) let_in, _) result
= fun f g acc {let_binder; rhs; let_result; attributes} ->
  let%bind acc,let_binder = binder g acc let_binder in
  let%bind acc,rhs        = f acc rhs in
  let%bind acc,let_result = f acc let_result in
  ok @@ (acc,{let_binder; rhs; let_result; attributes})

let lambda : ('acc -> 'a -> ('acc * 'b, _) result) -> ('acc -> 'c -> ('acc * 'd, _) result) -> 'acc -> ('a,'c) lambda -> ('acc * ('b,'d) lambda , _) result
= fun f g acc {binder=b;output_type;result}->
  let%bind acc,binder = binder g acc b in
  let%bind acc,output_type = option g acc output_type in
  let%bind acc,result = f acc result in
  ok @@ (acc,{binder;output_type;result})

let path : ('acc -> 'a -> ('acc * 'b,_) result) -> 'acc -> 'a access list -> ('acc * 'b access list, _) result
= fun f acc path ->
  let aux acc a = match a with
    | Access_record s -> ok @@ (acc,Access_record s)
    | Access_tuple  i -> ok @@ (acc,Access_tuple  i)
    | Access_map e ->
      let%bind acc,e = f acc e in
      ok @@ (acc,Access_map e)
  in
  bind_fold_map_list aux acc path

let record : ('acc -> 'a -> ('acc * 'b,_) result) -> 'acc -> 'a label_map -> ('acc * 'b label_map,_) result
= fun f acc record ->
  Helpers.bind_fold_map_lmap (
    fun acc _ a -> f acc a
  ) acc record

let recursive : ('acc -> 'a -> ('acc * 'b,_) result) -> ('acc -> 'c -> ('acc * 'd,_) result) -> 'acc -> ('a,'c) recursive -> ('acc * ('b,'d) recursive, _) result
= fun f g acc {fun_name;fun_type;lambda=l} ->
  let%bind acc,fun_type = g acc fun_type in
  let%bind acc,lambda = lambda f g acc l in
  ok @@ (acc,{fun_name;fun_type;lambda})

let accessor : ('acc -> 'a -> ('acc * 'b,_) result) -> 'acc -> 'a accessor -> ('acc * 'b accessor, _) result
= fun f acc {record;path=p} ->
  let%bind acc,record = f acc record in
  let%bind acc,path   = path f acc p in
  ok @@ (acc,({record;path} : 'b accessor))

let update : ('acc -> 'a -> ('acc * 'b,_) result) -> 'acc -> 'a update -> ('acc * 'b update, _) result
= fun f acc {record;path=p;update} ->
  let%bind acc,record = f acc record in
  let%bind acc,path   = path f acc p in
  let%bind acc,update = f acc update in
  ok @@ (acc,({record;path;update} : 'b update))

let record_accessor : ('acc -> 'a -> ('acc * 'b,_) result) -> 'acc -> 'a record_accessor -> ('acc * 'b record_accessor, _) result
= fun f acc {record;path} ->
  let%bind acc,record = f acc record in
  ok @@ (acc,({record;path} : 'b record_accessor))

let record_update : ('acc -> 'a -> ('acc * 'b,_) result) -> 'acc -> 'a record_update -> ('acc * 'b record_update, _) result
= fun f acc {record;path;update} ->
  let%bind acc,record = f acc record in
  let%bind acc,update = f acc update in
  ok @@ (acc,({record;path;update} : 'b record_update))

let sequence : ('acc -> 'a -> ('acc * 'b,_) result) -> 'acc -> 'a sequence -> ('acc * 'b sequence, _) result
= fun f acc {expr1;expr2} ->
  let%bind acc,expr1 = f acc expr1 in
  let%bind acc,expr2 = f acc expr2 in
  ok @@ (acc,{expr1;expr2})

let ascription : ('acc -> 'a -> ('acc * 'b,_) result) -> ('acc -> 'c -> ('acc * 'd,_) result) -> 'acc -> ('a,'c) ascription -> ('acc * ('b,'d) ascription, _) result
= fun f g acc {anno_expr; type_annotation} ->
  let%bind acc,anno_expr = f acc anno_expr in
  let%bind acc,type_annotation = g acc type_annotation in
  ok @@ (acc,{anno_expr; type_annotation})

let raw_code : ('acc -> 'a -> ('acc * 'b,_) result) -> 'acc -> 'a raw_code -> ('acc * 'b raw_code, _) result
= fun f acc {language;code} ->
  let%bind acc,code = f acc code in
  ok @@ (acc,{language;code})

let conditional : ('acc -> 'a -> ('acc * 'b,_) result) -> 'acc -> 'a conditional -> ('acc * 'b conditional, _) result
= fun f acc {condition;then_clause;else_clause} ->
  let%bind acc,condition   = f acc condition in
  let%bind acc,then_clause = f acc then_clause in
  let%bind acc,else_clause = f acc else_clause in
  ok @@ (acc,{condition;then_clause;else_clause})

let assign : ('acc -> 'a -> ('acc * 'b,_) result) -> 'acc -> 'a assign -> ('acc * 'b assign, _) result
= fun f acc {variable; access_path; expression} ->
  let%bind acc,access_path = path f acc access_path in
  let%bind acc,expression  = f acc expression in
  ok @@ (acc, {variable; access_path; expression})

let for_
= fun f acc {binder; start; final; incr; f_body} ->
  let%bind acc,f_body = f acc f_body in
  ok @@ (acc, {binder; start; final; incr; f_body})

let for_each
= fun f acc {fe_binder; collection; collection_type; fe_body} ->
  let%bind acc,collection = f acc collection in
  let%bind acc,fe_body    = f acc fe_body in
  ok @@ (acc, {fe_binder; collection; collection_type; fe_body})

let while_loop
= fun f acc {cond; body} ->
  let%bind acc,cond = f acc cond in
  let%bind acc,body = f acc body in
  ok @@ (acc, {cond; body})

(* Declaration *)
let declaration_type : ('acc -> 'a -> ('acc * 'b, _) result) -> 'acc -> 'a declaration_type -> ('acc * 'b declaration_type, _) result
= fun g acc {type_binder; type_expr} ->
  let%bind acc,type_expr = g acc type_expr in
  ok @@ (acc,{type_binder; type_expr})

let declaration_constant : ('acc -> 'a -> ('acc * 'b,_) result) -> ('acc -> 'c -> ('acc * 'd,_) result) -> 'acc -> ('a,'c) declaration_constant -> ('acc * ('b,'d) declaration_constant, _) result
= fun f g acc {binder=b; attr; expr} ->
  let%bind acc,binder = binder g acc b in
  let%bind acc,expr   = f acc expr     in
  ok @@ (acc,{binder;attr;expr})

let program : ('acc -> 'a -> ('acc * 'b,_) result) -> 'acc -> 'a list -> ('acc * 'b list, _) result
= fun d acc prg ->
  bind_fold_map_list d acc prg
