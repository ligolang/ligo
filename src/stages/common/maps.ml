open Types

(* Types level *)

let for_all : ('a -> 'b) -> 'a abstraction -> 'b abstraction =
  fun f ({ type_ ; _} as fa) ->
    let type_ = f type_ in
    { fa with type_}

let type_app : ('a -> 'b) -> 'a type_app -> 'b type_app
= fun g {type_operator;arguments} ->
  let arguments = List.map ~f:g arguments in
  {type_operator; arguments}

let row_element : ('a -> 'b) -> 'a row_element -> 'b row_element
= fun g {associated_type ; attributes ; decl_pos}  ->
  let associated_type = g associated_type in
  ({associated_type ; attributes ; decl_pos}: 'b row_element)

let rows : ('a -> 'b) -> 'a rows -> 'b rows
= fun g {fields; attributes} ->
  let fields = LMap.map (row_element g) fields in
  {fields; attributes}

let arrow : ('a -> 'b) -> 'a arrow -> 'b arrow
= fun g {type1;type2} ->
  let type1 = g type1 in
  let type2 = g type2 in
  {type1;type2}

(* Expression level *)

let constant : ('a ->  'b) -> 'a constant -> 'b constant
= fun f {cons_name;arguments} ->
  let arguments = List.map ~f arguments in
  {cons_name;arguments}

let constructor : ('a -> 'b) -> 'a constructor -> 'b constructor
= fun f {constructor;element} ->
  let element = f element in
  {constructor; element}

let application : ('a -> 'b) -> 'a application -> 'b application
= fun f {lamb;args} ->
  let lamb = f lamb in
  let args = f args in
  {lamb; args}

and binder : ('a -> 'b) -> 'a binder -> 'b binder
= fun f {var; ascr; attributes} ->
  let ascr = Option.map ~f ascr in
  {var; ascr; attributes}

let let_in :  ('a -> 'b) -> ('c -> 'd) -> ('a,'c) let_in -> ('b,'d) let_in
= fun f g {let_binder; rhs; let_result; attributes} ->
  let let_binder = binder g let_binder in
  let rhs        = f rhs in
  let let_result = f let_result in
  {let_binder; rhs; let_result; attributes}

let type_in :  ('a -> 'b) -> ('c -> 'd) -> ('a,'c) type_in -> ('b,'d) type_in
= fun f g {type_binder; rhs; let_result} ->
  let rhs        = g rhs in
  let let_result = f let_result in
  {type_binder; rhs; let_result}

let lambda : ('a -> 'b) -> ('c -> 'd) -> ('a,'c) lambda -> ('b,'d) lambda
= fun f g {binder=b;output_type;result}->
  let binder = binder g b in
  let output_type = Option.map ~f:g output_type in
  let result = f result in
  {binder;output_type;result}

let type_abs : ('a -> 'b) -> 'a type_abs -> 'b type_abs
= fun f {type_binder;result}->
  let result = f result in
  {type_binder;result}

let path : ('a -> 'b) -> 'a access list -> 'b access list
= fun f path ->
  let aux a = match a with
    | Access_record s -> Access_record s
    | Access_tuple  i -> Access_tuple  i
    | Access_map e ->
      let e = f e in
      Access_map e
  in
  List.map ~f:aux path

let record : ('a -> 'b) -> 'a label_map -> 'b label_map
= fun f record ->
  LMap.map f record

let recursive : ('a -> 'b) -> ('c -> 'd) -> ('a,'c) recursive -> ('b,'d) recursive
= fun f g {fun_name;fun_type;lambda=l} ->
  let fun_type = g fun_type in
  let lambda = lambda f g l in
  {fun_name;fun_type;lambda}

let accessor : ('a -> 'b) -> 'a accessor -> 'b accessor
= fun f {record;path=p} ->
  let record = f record in
  let path   = path f p in
  ({record;path} : 'b accessor)

let update : ('a -> 'b) -> 'a update -> 'b update
= fun f {record;path=p;update} ->
  let record = f record in
  let path   = path f p in
  let update = f update in
  ({record;path;update} : 'b update)

let record_accessor : ('a -> 'b) -> 'a record_accessor -> 'b record_accessor
= fun f {record;path} ->
  let record = f record in
  ({record;path} : 'b record_accessor)

let record_update : ('a -> 'b) -> 'a record_update -> 'b record_update
= fun f {record;path;update} ->
  let record = f record in
  let update = f update in
  ({record;path;update} : 'b record_update)

let sequence : ('a -> 'b) -> 'a sequence -> 'b sequence
= fun f {expr1;expr2} ->
  let expr1 = f expr1 in
  let expr2 = f expr2 in
  {expr1;expr2}

let ascription : ('a -> 'b) -> ('c -> 'd) -> ('a,'c) ascription -> ('b,'d) ascription
= fun f g {anno_expr; type_annotation} ->
  let anno_expr = f anno_expr in
  let type_annotation = g type_annotation in
  {anno_expr; type_annotation}

let raw_code : ('a -> 'b) -> 'a raw_code -> 'b raw_code
= fun f {language;code} ->
  let code = f code in
  {language;code}

let conditional : ('a -> 'b) -> 'a conditional -> 'b conditional
= fun f {condition;then_clause;else_clause} ->
  let condition   = f condition in
  let then_clause = f then_clause in
  let else_clause = f else_clause in
  {condition;then_clause;else_clause}

let assign : ('a -> 'b) -> ('c -> 'd) -> ('a,'c) assign -> ('b,'d) assign
= fun f g {binder=b; expression} ->
  let binder      = binder g b in
  let expression  = f expression in
  {binder; expression}

let for_
= fun f {binder; start; final; incr; f_body} ->
  let f_body = f f_body in
  {binder; start; final; incr; f_body}

let for_each
= fun f {fe_binder; collection; fe_body; collection_type} ->
  let collection = f collection in
  let fe_body    = f fe_body in
  {fe_binder; collection; fe_body ; collection_type}

let while_loop
= fun f {cond; body} ->
  let cond = f cond in
  let body = f body in
  {cond; body}

(* Declaration *)
let declaration_type : ('a -> 'b) -> ('attra -> 'attrb) -> ('a,'attra) declaration_type' -> ('b,'attrb) declaration_type'
= fun g h {type_binder; type_expr; type_attr} ->
  let type_expr = g type_expr in
  let type_attr = h type_attr in
  {type_binder; type_expr; type_attr}

let declaration_constant : ('a -> 'b) -> ('c -> 'd) -> ('attra -> 'attrb) -> ('a,'c,'attra) declaration_constant' -> ('b,'d,'attrb) declaration_constant'
= fun map_e map_ty map_attr_e {binder=b; attr; expr} ->
  let binder = binder map_ty b in
  let expr   = map_e expr     in
  let attr = map_attr_e attr in
  {binder;attr;expr}

let rec declaration_module : ('e_src -> 'e_dst) -> ('ty_src -> 'ty_dst) -> ('ea_src -> 'ea_dst) -> ('ta_src -> 'ta_dst) -> ('ma_src -> 'ma_dst) ->
  ('e_src,'ty_src,'ea_src,'ta_src,'ma_src) declaration_module' -> ('e_dst,'ty_dst,'ea_dst,'ta_dst,'ma_dst) declaration_module'
= fun map_e map_ty map_attr_e map_attr_t map_attr_m {module_binder; module_; module_attr} ->
  let module_ = module_expr map_e map_ty map_attr_e map_attr_t map_attr_m module_ in
  let module_attr = map_attr_t module_attr in
  {module_binder;module_;module_attr}

and mod_in : ('e_src -> 'e_dst) -> ('ty_src -> 'ty_dst) -> ('ea_src -> 'ea_dst) -> ('ta_src -> 'ta_dst) -> ('ma_src -> 'ma_dst) ->
  ('e_src,'ty_src,'ea_src,'ta_src,'ma_src) mod_in' -> ('e_dst,'ty_dst,'ea_dst,'ta_dst,'ma_dst) mod_in'
= fun map_e map_ty map_attr_e map_attr_t map_attr_m { module_binder; rhs ; let_result} ->
  let rhs = module_expr map_e map_ty map_attr_e map_attr_t map_attr_m rhs in
  let let_result = map_e let_result in
  { module_binder; rhs ; let_result}
and module_alias
= fun ma -> ma

and declarations f g h i j prg =
List.map ~f:(Location.map (declaration f g h i j)) prg
and declaration
= fun map_e map_ty map_attr_e map_attr_t map_attr_m -> function
  Declaration_type    ty -> let ty = declaration_type     map_ty map_attr_t ty in Declaration_type ty
| Declaration_constant c -> let c  = declaration_constant map_e map_ty map_attr_e c in Declaration_constant c
| Declaration_module   m -> let m  = declaration_module   map_e map_ty map_attr_e map_attr_t map_attr_m m in Declaration_module m

and module_expr : ('e_src -> 'e_dst) -> ('ty_src -> 'ty_dst) -> ('ea_src -> 'ea_dst) -> ('ta_src -> 'ta_dst) -> ('ma_src -> 'ma_dst) ->
    ('a,'c,'ea_src,'ta_src,'ma_src) module_expr' -> ('b,'d,'ea_dst,'ta_dst,'ma_dst) module_expr' =
  fun map_e map_ty map_attr_e map_attr_t map_attr_m mexp ->
    Location.map
      (function
      | M_struct prg ->
        let prg = declarations map_e map_ty map_attr_e map_attr_t map_attr_m prg in
        M_struct prg
      | M_variable x -> M_variable x
      | M_module_path path -> M_module_path path
      )
      mexp
