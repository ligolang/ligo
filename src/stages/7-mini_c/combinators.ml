open Types

module Expression = struct
  type t' = expression_content
  type t = expression

  let get_type : t -> type_expression = fun e -> e.type_expression

  let make_t ?(loc = Location.generated) ?source_type tc =
    { type_content = tc; location = loc; source_type }


  let make ?(loc = Location.generated) e' t =
    { content = e'; type_expression = t; location = loc }


  let make_tpl ?(loc = Location.generated) (e', t) =
    { content = e'; type_expression = t; location = loc }
end

let get_bool (v : value) =
  match v with
  | D_bool b -> Some b
  | _ -> None


let get_int (v : value) =
  match v with
  | D_int n -> Some n
  | _ -> None


let get_nat (v : value) =
  match v with
  | D_nat n -> Some n
  | _ -> None


let get_mutez (v : value) =
  match v with
  | D_mutez n -> Some n
  | _ -> None


let get_timestamp (v : value) =
  match v with
  | D_timestamp n -> Some n
  | _ -> None


let get_string (v : value) =
  match v with
  | D_string s -> Some s
  | _ -> None


let get_bytes (v : value) =
  match v with
  | D_bytes b -> Some b
  | _ -> None


let get_unit (v : value) =
  match v with
  | D_unit -> Some ()
  | _ -> None


let get_map (v : value) =
  match v with
  | D_map lst -> Some lst
  | _ -> None


let get_big_map (v : value) =
  match v with
  | D_big_map lst -> Some lst
  | _ -> None


let get_list (v : value) =
  match v with
  | D_list lst -> Some lst
  | _ -> None


let get_set (v : value) =
  match v with
  | D_set lst -> Some lst
  | _ -> None


let get_t_function tv =
  match tv.type_content with
  | T_function ty -> Some ty
  | _ -> None


let get_t_option (v : type_expression) =
  match v.type_content with
  | T_option t -> Some t
  | _ -> None


let get_pair (v : value) =
  match v with
  | D_pair (a, b) -> Some (a, b)
  | _ -> None


let get_t_pair (t : type_expression) =
  match t.type_content with
  | T_tuple [ (_, a); (_, b) ] -> Some (a, b)
  | _ -> None


let get_t_tuple (t : type_expression) =
  match t.type_content with
  | T_tuple l -> Some (List.map ~f:snd l)
  | _ -> None


let get_t_or (t : type_expression) =
  match t.type_content with
  | T_or ((_, a), (_, b)) -> Some (a, b)
  | _ -> None


let get_t_map (t : type_expression) =
  match t.type_content with
  | T_map kv -> Some kv
  | _ -> None


let get_t_big_map (t : type_expression) =
  match t.type_content with
  | T_big_map kv -> Some kv
  | _ -> None


let get_t_list (t : type_expression) =
  match t.type_content with
  | T_list t -> Some t
  | _ -> None


let get_t_set (t : type_expression) =
  match t.type_content with
  | T_set t -> Some t
  | _ -> None


let get_t_collection (t : type_expression) =
  match t.type_content with
  | T_list t | T_set t | T_map (_, t) | T_big_map (_, t) -> Some t
  | _ -> None


let get_left (v : value) =
  match v with
  | D_left b -> Some b
  | _ -> None


let get_right (v : value) =
  match v with
  | D_right b -> Some b
  | _ -> None


let get_t_left t =
  match t.type_content with
  | T_or ((_, a), _) -> Some a
  | _ -> None


let get_t_right t =
  match t.type_content with
  | T_or (_, (_, b)) -> Some b
  | _ -> None


let get_operation (v : value) =
  match v with
  | D_operation x -> Some x
  | _ -> None


let t_unit ?loc () : type_expression = Expression.make_t ?loc @@ T_base TB_unit
let t_pair ?loc x y : type_expression = Expression.make_t ?loc @@ T_tuple [ x; y ]
let t_union ?loc ~source_type x y : type_expression = Expression.make_t ?loc ?source_type @@ T_or (x, y)
let t_tuple ?loc ~source_type xs : type_expression = Expression.make_t ?loc ?source_type @@ T_tuple xs

let e_proj ?loc exp ty index field_count : expression =
  Expression.make ?loc (E_proj (exp, index, field_count)) ty


let e_let_in ?loc v tv inline expr body : expression =
  Expression.(make_tpl ?loc (E_let_in (expr, inline, ((v, tv), body)), get_type body))


let e_let_mut_in ?loc v tv expr body : expression =
  Expression.(make_tpl ?loc (E_let_mut_in (expr, ((v, tv), body)), get_type body))


let e_application ?loc f t arg : expression =
  Expression.(make_tpl ?loc (E_application (f, arg), t))


let e_var ?loc vname t : expression = Expression.(make_tpl ?loc (E_variable vname, t))

let ec_pair a b : expression_content =
  E_constant { cons_name = C_PAIR; arguments = [ a; b ] }
