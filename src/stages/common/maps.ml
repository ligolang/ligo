open Types
open Trace

(* Types level *)

let type_app : ('a -> ('b, _) result) -> 'a type_app -> ('b type_app, _) result
= fun g {type_operator;arguments} ->
  let%bind arguments = bind_map_list g arguments in
  ok @@ {type_operator; arguments}

let rows : ('a -> ('b,_) result) -> 'a rows -> ('b rows,_) result
= fun g {fields; attributes} ->
  let%bind fields = Helpers.bind_map_lmap
  (fun {associated_type ; attributes ; decl_pos} ->
    let%bind associated_type = g associated_type in
    ok @@ ({associated_type ; attributes ; decl_pos}: 'b row_element)
  ) fields in
  ok @@ {fields; attributes}

let arrow : ('a -> ('b,_) result) -> 'a arrow -> ('b arrow,_) result
= fun g {type1;type2} ->
  let%bind type1 = g type1 in
  let%bind type2 = g type2 in
  ok @@ {type1;type2}

(* Expression level *)

let constant : ('a ->  ('b,_) result) -> 'a constant -> ('b constant,_) result
= fun f {cons_name;arguments} ->
  let%bind arguments = bind_map_list f arguments in
  ok @@ {cons_name;arguments}

let constructor : ('a -> ('b,_) result) -> 'a constructor -> ('b constructor,_) result
= fun f {constructor;element} ->
  let%bind element = f element in
  ok @@ {constructor; element}

let application : ('a -> ('b,_) result) -> 'a application -> ('b application,_) result
= fun f {lamb;args} ->
  let%bind lamb = f lamb in
  let%bind args = f args in
  ok @@ {lamb; args}

and binder : ('a -> ('b, _) result) -> 'a binder -> ('b binder, _) result
= fun f {var; ascr} ->
  let%bind ascr = bind_map_option f ascr in
  ok @@ {var; ascr}

let let_in :  ('a -> ('b, _) result) -> ('c -> ('d, _) result) -> ('a,'c) let_in -> (('b,'d) let_in, _) result
= fun f g {let_binder; rhs; let_result; attributes} ->
  let%bind let_binder = binder g let_binder in
  let%bind rhs        = f rhs in
  let%bind let_result = f let_result in
  ok @@ {let_binder; rhs; let_result; attributes}

let lambda : ('a -> ('b, _) result) -> ('c -> ('d, _) result) -> ('a,'c) lambda -> (('b,'d) lambda , _) result
= fun f g {binder=b;output_type;result}->
  let%bind binder = binder g b in
  let%bind output_type = bind_map_option g output_type in
  let%bind result = f result in
  ok @@ {binder;output_type;result}

let path : ('a -> ('b,_) result) -> 'a access list -> ('b access list, _) result
= fun f path ->
  let aux a = match a with
    | Access_record s -> ok @@ Access_record s
    | Access_tuple  i -> ok @@ Access_tuple  i
    | Access_map e ->
      let%bind e = f e in
      ok @@ Access_map e
  in
  bind_map_list aux path

let record : ('a -> ('b,_) result) -> 'a label_map -> ('b label_map,_) result
= fun f record ->
  Helpers.bind_map_lmap f record

let recursive : ('a -> ('b,_) result) -> ('c -> ('d,_) result) -> ('a,'c) recursive -> (('b,'d) recursive, _) result
= fun f g {fun_name;fun_type;lambda=l} ->
  let%bind fun_type = g fun_type in
  let%bind lambda = lambda f g l in
  ok @@ {fun_name;fun_type;lambda}

let accessor : ('a -> ('b,_) result) -> 'a accessor -> ('b accessor, _) result
= fun f {record;path=p} ->
  let%bind record = f record in
  let%bind path   = path f p in
  ok @@ ({record;path} : 'b accessor)

let update : ('a -> ('b,_) result) -> 'a update -> ('b update, _) result
= fun f {record;path=p;update} ->
  let%bind record = f record in
  let%bind path   = path f p in
  let%bind update = f update in
  ok @@ ({record;path;update} : 'b update)

let record_accessor : ('a -> ('b,_) result) -> 'a record_accessor -> ('b record_accessor, _) result
= fun f {record;path} ->
  let%bind record = f record in
  ok @@ ({record;path} : 'b record_accessor)

let record_update : ('a -> ('b,_) result) -> 'a record_update -> ('b record_update, _) result
= fun f {record;path;update} ->
  let%bind record = f record in
  let%bind update = f update in
  ok @@ ({record;path;update} : 'b record_update)

let sequence : ('a -> ('b,_) result) -> 'a sequence -> ('b sequence, _) result
= fun f {expr1;expr2} ->
  let%bind expr1 = f expr1 in
  let%bind expr2 = f expr2 in
  ok @@ {expr1;expr2}

let ascription : ('a -> ('b,_) result) -> ('c -> ('d,_) result) -> ('a,'c) ascription -> (('b,'d) ascription, _) result
= fun f g {anno_expr; type_annotation} ->
  let%bind anno_expr = f anno_expr in
  let%bind type_annotation = g type_annotation in
  ok @@ {anno_expr; type_annotation}

let raw_code : ('a -> ('b,_) result) -> 'a raw_code -> ('b raw_code, _) result
= fun f {language;code} ->
  let%bind code = f code in
  ok @@ {language;code}

let conditional : ('a -> ('b,_) result) -> 'a conditional -> ('b conditional, _) result
= fun f {condition;then_clause;else_clause} ->
  let%bind condition   = f condition in
  let%bind then_clause = f then_clause in
  let%bind else_clause = f else_clause in
  ok @@ {condition;then_clause;else_clause}

let assign : ('a -> ('b,_) result) -> 'a assign -> ('b assign, _) result
= fun f {variable; access_path; expression} ->
  let%bind access_path = path f access_path in
  let%bind expression  = f expression in
  ok @@ {variable; access_path; expression}

let for_
= fun f {binder; start; final; incr; f_body} ->
  let%bind f_body = f f_body in
  ok @@ {binder; start; final; incr; f_body}

let for_each
= fun f {fe_binder; collection; collection_type; fe_body} ->
  let%bind collection = f collection in
  let%bind fe_body    = f fe_body in
  ok @@ {fe_binder; collection; collection_type; fe_body}

let while_loop
= fun f {cond; body} ->
  let%bind cond = f cond in
  let%bind body = f body in
  ok @@ {cond; body}

(* Declaration *)
let declaration_type : ('a -> ('b, _) result) -> 'a declaration_type -> ('b declaration_type, _) result
= fun g {type_binder; type_expr} ->
  let%bind type_expr = g type_expr in
  ok @@ {type_binder; type_expr}

let declaration_constant : ('a -> ('b,_) result) -> ('c -> ('d,_) result) -> ('a,'c) declaration_constant -> (('b,'d) declaration_constant, _) result
= fun f g {binder=b; attr; expr} ->
  let%bind binder = binder g b in
  let%bind expr   = f expr     in
  ok @@ {binder;attr;expr}

let program : ('a -> ('b,_) result) -> 'a list -> ('b list, _) result
= fun d prg ->
  bind_map_list d prg
