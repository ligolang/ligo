open Ligo_prim
open Ast_typed

type contract_pass_data = Contract_passes.contract_pass_data

module V = Ligo_prim.Value_var
module M = Simple_utils.Map.Make(V)

(* var -> # of uses * unused vars *)
type muchuse = int M.t * V.t list

let muchuse_neutral : muchuse = M.empty, []

(* class Dup _:
 *   Dup (never | unit | bool | nat | int | string | bytes | chain_id
 *    | mutez | key_hash | key | signature | timestamp | address
 *    | operation | bls12_381_g1 | bls12_381_g2 | bls12_381_fr
 *    | sapling_transaction _ | sapling_state _)
 *   Dup 'a => Dup (option 'a | list 'a | set 'a)
 *   Dup (contract 'a)
 *   Dup 'a, Dup 'b => Dup (pair 'a 'b | or 'a 'b | map 'a 'b | 'big_map 'a 'b)
 *   Dup (lambda 'a 'b) *)

let rec is_dup (t : type_expression) =
  let open Literal_types in
  match t.type_content with
  | T_constant {injection = (
    Never               |
    Int                 |
    Nat                 |
    Chest               |
    Chest_key           |
    Unit                |
    String              |
    Bytes               |
    Chain_id            |
    Tez                 |
    Key_hash            |
    Key                 |
    Signature           |
    Timestamp           |
    Address             |
    Operation           |
    Bls12_381_g1        |
    Bls12_381_g2        |
    Bls12_381_fr        |
    Sapling_transaction |
    Sapling_state       |
    (* Test primitives are dup *)
    Typed_address       |
    Mutation            |
    Tx_rollup_l2_address |
    Michelson_contract  |
    Ast_contract        |
    Michelson_program   |
    Gen                 |
    Int64               |
    (* Externals are dup *)
    External _
  ); _} ->
     true
  | T_constant {injection=
    (List    |
     Set    ); parameters = [t]; _} ->
      is_dup t
  | T_constant {injection=Contract;_} ->
      true
  | T_constant {injection=
      (Big_map |
       Map    ); parameters = [t1;t2]; _} ->
      is_dup t1 && is_dup t2
  | T_record rows
  | T_sum rows ->
     let row_types = Record.LMap.to_list rows.fields
                     |> List.map ~f:(fun (v:row_element) -> v.associated_type)
                     |> List.filter ~f:(fun v -> not (is_dup v)) in
     List.is_empty row_types
  | T_arrow _ -> true
  | T_variable _ -> true
  | T_abstraction {type_;ty_binder=_;kind=_} -> is_dup type_
  | T_for_all {type_;ty_binder=_;kind=_} -> is_dup type_
  | T_constant { injection=(
                     Map              | Big_map              | List               |
                     Set              |                        Michelson_or       |
    Michelson_pair | Pvss_key         | Baker_operation      |
    Ticket         |                    Chest_opening_result | Baker_hash);_ }  -> false
  | T_singleton _ -> false

let muchuse_union (x,a) (y,b) =
  M.union (fun _ x y -> Some (x + y)) x y, a@b

let muchuse_max (x,a) (y,b) =
  M.union (fun _ x y -> if x > y then Some x else Some y) x y, a@b

let muchuse_unions =
  List.fold_left ~f:muchuse_union ~init:muchuse_neutral

let muchuse_maxs =
  List.fold_left ~f:muchuse_max ~init:muchuse_neutral

let add_if_not_dup xs b v t =
  if not (is_dup t) && b then
    v :: xs
  else
    xs

let is_much_used countuse v =
  match M.find_opt countuse v with
  | None -> false
  | Some n -> (n > 1)

let muchuse_of_binder v t (countuse, muchused) =
  let muchused = add_if_not_dup muchused (is_much_used v countuse) v t in
  let countuse = M.remove v countuse in
  (countuse, muchused)

let rec muchuse_of_expr expr : muchuse =
  match expr.expression_content with
  | E_literal _ ->
     muchuse_neutral
  | E_constructor {element;_} ->
     muchuse_of_expr element
  | E_constant {arguments;_} ->
     muchuse_unions (List.map ~f:muchuse_of_expr arguments)
  | E_variable v ->
     M.add v 1 M.empty,[]
  | E_application {lamb;args} ->
     muchuse_union (muchuse_of_expr lamb) (muchuse_of_expr args)
  | E_lambda _ ->
     begin
       match get_lambda_with_type expr with
       | None -> muchuse_neutral (* something's wrong in the tree? *)
       | Some (l, (t, _))  -> muchuse_of_lambda t l
     end
  | E_type_abstraction {result;_} ->
     muchuse_of_expr result
  | E_let_in {let_binder;rhs;let_result;_} ->
     muchuse_union (muchuse_of_expr rhs)
       (muchuse_of_binder (Binder.get_var let_binder) rhs.type_expression
          (muchuse_of_expr let_result))
  | E_recursive {fun_name;lambda;fun_type} ->
     muchuse_of_binder fun_name fun_type (muchuse_of_lambda fun_type lambda)
  | E_matching {matchee;cases} ->
     muchuse_union (muchuse_of_expr matchee) (muchuse_of_cases cases)
  | E_record re ->
     Record.fold
       (fun acc x -> muchuse_union (muchuse_of_expr x) acc) muchuse_neutral re
  | E_raw_code {code;_} ->
     muchuse_of_expr code
  | E_accessor {struct_;_} ->
     muchuse_of_expr struct_
  | E_update {struct_;update;_} ->
     muchuse_union (muchuse_of_expr struct_) (muchuse_of_expr update)
  | E_mod_in {let_result;_} ->
     muchuse_of_expr let_result
  | E_type_inst {forall;_} ->
     muchuse_of_expr forall
  | E_module_accessor {module_path;element} ->
    let pref = Format.asprintf "%a" (Simple_utils.PP_helpers.list_sep Module_var.pp (Simple_utils.PP_helpers.tag ".")) module_path in
    let name = V.of_input_var ~loc:expr.location @@
      pref ^ "." ^ (Format.asprintf "%a" Value_var.pp element) in
    (M.add name 1 M.empty,[])
  | E_assign { binder; expression } ->
    muchuse_union
    (M.add (Binder.get_var binder) 1 M.empty, [])
    (muchuse_of_expr expression)
  | E_deref var -> M.add var 1 M.empty, []
  | E_let_mut_in {let_binder;rhs;let_result;_} ->
    muchuse_union (muchuse_of_expr rhs)
      (muchuse_of_binder (Binder.get_var let_binder) rhs.type_expression
         (muchuse_of_expr let_result))
  | E_for { binder; start; final; incr; f_body } ->
    muchuse_unions
      [ muchuse_of_expr start
      ; muchuse_of_expr final
      ; muchuse_of_expr incr
      ; muchuse_of_binder binder start.type_expression (muchuse_of_expr f_body) 
      ]
  | E_for_each { fe_binder = binder1, binder2; collection; fe_body; _ } ->
    (* Recover type of binders *)
    let binders = 
      let type_ = collection.type_expression in
      if is_t_map type_ then
        let key_type, val_type = get_t_map_exn type_ in
        (match binder2 with
        | None ->
          [ binder1, t_pair key_type val_type ]
        | Some binder2 -> 
          [ binder1, key_type; binder2, val_type ]) 
      else if is_t_set type_ then
        [ binder1, get_t_set_exn type_ ]
      else if is_t_list type_ then
        [ binder1, get_t_list_exn type_ ]
      else
        failwith "corner case"
    in
    muchuse_unions
      [ muchuse_of_expr collection
      ; List.fold_left binders ~init:(muchuse_of_expr fe_body) ~f:(fun muchuse (binder, type_) -> muchuse_of_binder binder type_ muchuse) 
      ]
  | E_while { cond; body } ->
    muchuse_unions
      [ muchuse_of_expr cond
      ; muchuse_of_expr body 
      ]

and muchuse_of_lambda t {binder; output_type = _; result} =
  muchuse_of_binder (Param.get_var binder) t (muchuse_of_expr result)

and muchuse_of_cases = function
  | Match_variant x -> muchuse_of_variant x
  | Match_record  x -> muchuse_of_record x

and muchuse_of_variant {cases;tv} =
  match get_t_sum tv with
  | None -> begin
      match get_t_list tv with
      | None -> muchuse_neutral
      | Some tv' ->
         let get_c_body (case : _ matching_content_case) = (case.constructor, (case.body, case.pattern)) in
         let c_body_lst = Record.of_list (List.map ~f:get_c_body cases) in
         let get_case c =  Record.LMap.find (Label c) c_body_lst in
         let match_nil,_ = get_case "Nil" in
         let match_cons,v = get_case "Cons" in
         muchuse_max (muchuse_of_binder v (t_pair tv' tv) (muchuse_of_expr match_cons)) (muchuse_of_expr match_nil)
    end
  | Some ts ->
     let case_ts ({constructor;_} : _ matching_content_case) =
       let row_element = Record.LMap.find constructor ts.fields in
       row_element.associated_type in
     let cases_ts = List.map ~f:case_ts cases in
     muchuse_maxs @@
       Stdlib.List.map2
         (fun t ({pattern;body;_} : _ matching_content_case) ->
           muchuse_of_binder pattern t (muchuse_of_expr body))
         cases_ts cases

and muchuse_of_record {body;fields;_} =
  let typed_vars = Record.LMap.to_list fields in
  List.fold_left ~f:(fun (c,m) b -> muchuse_of_binder (Binder.get_var b) (Binder.get_ascr b) (c,m))
    ~init:(muchuse_of_expr body) typed_vars

let rec get_all_declarations (module_name : Module_var.t) : module_ ->
                               (Value_var.t * type_expression) list =
  function m ->
    let aux = fun ({wrap_content=x;location} : decl) ->
      match x with
      | D_value {binder;expr;_} ->
          let name = V.of_input_var ~loc:location @@ (Format.asprintf "%a" Module_var.pp module_name) ^ "." ^ (Format.asprintf "%a" Value_var.pp @@ Binder.get_var binder) in
          [(name, expr.type_expression)]
      | D_module {module_binder;module_ = { wrap_content = M_struct module_ ; _ } ;module_attr=_} ->
         let recs = get_all_declarations module_binder module_ in
         let add_module_name (v, t) =
          let name = V.of_input_var ~loc:location @@ (Format.asprintf "%a" Module_var.pp module_name) ^ "." ^ (Format.asprintf "%a" Value_var.pp v) in
          (name, t) in
         recs |> List.map ~f:add_module_name
      | _ -> [] in
    m |> List.map ~f:aux |> List.concat

let rec muchused_helper (muchuse : muchuse) =
  function m ->
  let map,muchused = List.fold_right ~f:muchuse_decl m ~init:muchuse in
  (*Put the variable in order : *)
  map,List.rev muchused

and muchuse_declaration = fun (x : declaration) s ->
  match Location.unwrap x with
  | D_value {expr ; binder; _} ->
      muchuse_union (muchuse_of_expr expr)
        (muchuse_of_binder (Binder.get_var binder) expr.type_expression s)
  | D_module {module_ = { wrap_content = M_struct module_ ; _ } ;module_binder;module_attr=_} ->
      let decls = get_all_declarations module_binder module_ in
      List.fold_right ~f:(fun (v, t) (c,m) -> muchuse_of_binder v t (c, m))
        decls ~init:(muchused_helper s module_)
  | _ -> s

and muchuse_decl = fun x s -> muchuse_declaration x s

let muchused_map_module ~raise : module_ -> module_ = function module_ ->
  let update_annotations annots =
    List.iter ~f:raise.Simple_utils.Trace.warning annots in
  let _,muchused = muchused_helper muchuse_neutral module_ in
  let warn_var v =
    `Self_ast_typed_warning_muchused
      (V.get_location v, Format.asprintf "%a" V.pp v) in
  let () = update_annotations @@ List.map ~f:warn_var muchused in
  module_

let muchused_helper (muchuse : muchuse) =
  function m ->
  let map,muchused = List.fold_right ~f:muchuse_declaration m ~init:muchuse in
  (*Put the variable in order : *)
  map,List.rev muchused

let muchused_map_program ~raise : program -> program = function p ->
  let update_annotations annots =
    List.iter ~f:raise.Simple_utils.Trace.warning annots in
  let _,muchused = muchused_helper muchuse_neutral p in
  let warn_var v =
    `Self_ast_typed_warning_muchused
      (V.get_location v, Format.asprintf "%a" V.pp v) in
  let () = update_annotations @@ List.map ~f:warn_var muchused in
  p
