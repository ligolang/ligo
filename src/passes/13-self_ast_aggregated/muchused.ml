open Core
open Ligo_prim
open Ast_aggregated
open Errors
module Ligo_map = Simple_utils.Ligo_map
module Trace = Simple_utils.Trace
module V = Ligo_prim.Value_var
module M = Map.Make (V)

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

let rec is_dup ~(raise : _ Trace.raise) (t : type_expression) =
  let open Literal_types in
  let is_dup = is_dup ~raise in
  match t.type_content with
  | T_constant
      { injection =
          ( Never
          | Int
          | Nat
          | Unit
          | String
          | Bytes
          | Chain_id
          | Tez
          | Key_hash
          | Key
          | Signature
          | Timestamp
          | Address
          | Operation
          | Bls12_381_g1
          | Bls12_381_g2
          | Bls12_381_fr
          | Sapling_transaction
          | Sapling_state
          | Chest
          | Chest_key
          (* Test primitives are dup *)
          | Typed_address
          | Mutation
          | Tx_rollup_l2_address
          | Michelson_contract
          | Michelson_program
          | Gen
          | Int64
          | Views
          | Dynamic_entrypoint
          (* Externals are dup *)
          | External _ )
      ; _
      } -> true
  | T_singleton _ -> true
  | T_constant { injection = List | Set; parameters = [ t ]; _ } -> is_dup t
  | T_constant { injection = Contract; _ } -> true
  | T_constant { injection = Big_map | Map; parameters = [ t1; t2 ]; _ } ->
    is_dup t1 && is_dup t2
  | T_record row | T_sum row ->
    row.fields |> Record.values |> List.exists ~f:(fun v -> not (is_dup v)) |> not
  | T_arrow _ -> true
  | T_variable _ -> true
  | T_for_all { type_; ty_binder = _; kind = _ }
  | T_abstraction { type_; ty_binder = _; kind = _ } -> is_dup type_
  | T_constant
      { injection =
          ( Map
          | Big_map
          | List
          | Set
          | Michelson_or
          | Michelson_pair
          | Pvss_key
          | Baker_operation
          | Ticket
          | Baker_hash )
      ; _
      } -> false
  | T_exists _ -> raise.error @@ unexpected_texists t t.location
  | T_union _ -> impossible_because_no_union_in_ast_aggregated ()


let muchuse_union (x, a) (y, b) = Ligo_map.union (fun _ x y -> Some (x + y)) x y, a @ b

let muchuse_max (x, a) (y, b) =
  Ligo_map.union (fun _ x y -> if x > y then Some x else Some y) x y, a @ b


let muchuse_unions = List.fold_left ~f:muchuse_union ~init:muchuse_neutral
let muchuse_maxs = List.fold_left ~f:muchuse_max ~init:muchuse_neutral

let add_if_not_dup ~(raise : _ Trace.raise) xs b v t =
  if (not (is_dup ~raise t)) && b then v :: xs else xs


let is_much_used countuse v =
  match Map.find v countuse with
  | None -> false
  | Some n -> n > 1


let muchuse_of_binder ~(raise : _ Trace.raise) v t (countuse, muchused) =
  let muchused = add_if_not_dup ~raise muchused (is_much_used v countuse) v t in
  let countuse = Map.remove countuse v in
  countuse, muchused


let rec muchuse_of_expr ~(raise : _ Trace.raise) expr : muchuse =
  let muchuse_of_expr = muchuse_of_expr ~raise in
  match expr.expression_content with
  | E_literal _ -> muchuse_neutral
  | E_constructor { element; _ } -> muchuse_of_expr element
  | E_constant { arguments; _ } -> muchuse_unions (List.map ~f:muchuse_of_expr arguments)
  | E_variable v -> Map.set M.empty ~key:v ~data:1, []
  | E_application { lamb; args } ->
    muchuse_union (muchuse_of_expr lamb) (muchuse_of_expr args)
  | E_lambda _ ->
    (match get_lambda_with_type expr with
    | None -> muchuse_neutral (* something's wrong in the tree? *)
    | Some (l, (t, _)) -> muchuse_of_lambda ~raise t l)
  | E_type_abstraction { result; _ } -> muchuse_of_expr result
  | E_let_in { let_binder; rhs; let_result; _ } ->
    let binders = Pattern.binders let_binder in
    let muchuse_let_result = muchuse_of_expr let_result in
    let muchuse_let_binder =
      List.fold binders ~init:muchuse_let_result ~f:(fun m b ->
          muchuse_of_binder ~raise (Binder.get_var b) (Binder.get_ascr b) m)
    in
    muchuse_union (muchuse_of_expr rhs) muchuse_let_binder
  | E_recursive { fun_name; lambda; fun_type; force_lambdarec = _ } ->
    muchuse_of_binder ~raise fun_name fun_type (muchuse_of_lambda ~raise fun_type lambda)
  | E_matching { matchee; cases } ->
    muchuse_union (muchuse_of_expr matchee) (muchuse_of_cases ~raise cases)
  | E_record re ->
    Record.fold
      ~f:(fun acc x -> muchuse_union (muchuse_of_expr x) acc)
      ~init:muchuse_neutral
      re
  | E_raw_code { code; _ } -> muchuse_of_expr code
  | E_accessor { struct_; _ } -> muchuse_of_expr struct_
  | E_update { struct_; update; _ } ->
    muchuse_union (muchuse_of_expr struct_) (muchuse_of_expr update)
  | E_type_inst { forall; _ } -> muchuse_of_expr forall
  | E_assign { binder; expression } ->
    muchuse_union
      (Map.set M.empty ~key:(Binder.get_var binder) ~data:1, [])
      (muchuse_of_expr expression)
  | E_coerce { anno_expr; _ } -> muchuse_of_expr anno_expr
  | E_deref var -> Map.set M.empty ~key:var ~data:1, []
  | E_let_mut_in { let_binder; rhs; let_result; _ } ->
    let binders = Pattern.binders let_binder in
    let muchuse_let_result = muchuse_of_expr let_result in
    let muchuse_let_binder =
      List.fold binders ~init:muchuse_let_result ~f:(fun m b ->
          muchuse_of_binder ~raise (Binder.get_var b) (Binder.get_ascr b) m)
    in
    muchuse_union (muchuse_of_expr rhs) muchuse_let_binder
  | E_for { binder; start; final; incr; f_body } ->
    muchuse_unions
      [ muchuse_of_expr start
      ; muchuse_of_expr final
      ; muchuse_of_expr incr
      ; muchuse_of_binder ~raise binder start.type_expression (muchuse_of_expr f_body)
      ]
  | E_for_each { fe_binder = binder1, binder2; collection; fe_body; _ } ->
    (* Recover type of binders *)
    let binders =
      let type_ = collection.type_expression in
      if is_t_map type_
      then (
        let loc = type_.location in
        let key_type, val_type = get_t_map_exn type_ in
        match binder2 with
        | None -> [ binder1, t_pair ~loc key_type val_type ]
        | Some binder2 -> [ binder1, key_type; binder2, val_type ])
      else if is_t_set type_
      then [ binder1, get_t_set_exn type_ ]
      else if is_t_list type_
      then [ binder1, get_t_list_exn type_ ]
      else failwith "corner case"
    in
    muchuse_unions
      [ muchuse_of_expr collection
      ; List.fold_left
          binders
          ~init:(muchuse_of_expr fe_body)
          ~f:(fun muchuse (binder, type_) ->
            muchuse_of_binder ~raise binder type_ muchuse)
      ]
  | E_while { cond; body } ->
    muchuse_unions [ muchuse_of_expr cond; muchuse_of_expr body ]


and muchuse_of_lambda ~(raise : _ Trace.raise) t { binder; output_type = _; result } =
  muchuse_of_binder ~raise (Param.get_var binder) t (muchuse_of_expr ~raise result)


and muchuse_of_cases ~(raise : _ Trace.raise) cases =
  muchuse_maxs
  @@ List.map cases ~f:(fun { pattern; body } ->
         let muchuse = muchuse_of_expr ~raise body in
         let binders = Pattern.binders pattern in
         let muchuse =
           List.fold_left binders ~init:muchuse ~f:(fun muchuse b ->
               muchuse_of_binder ~raise (Binder.get_var b) (Binder.get_ascr b) muchuse)
         in
         muchuse)


and muchuse_declaration ~(raise : _ Trace.raise) (x : declaration) s =
  match Location.unwrap x with
  | D_value { expr; binder; _ } ->
    muchuse_union
      (muchuse_of_expr ~raise expr)
      (muchuse_of_binder ~raise (Binder.get_var binder) expr.type_expression s)
  | D_irrefutable_match { expr; pattern; _ } ->
    let binders = Pattern.binders pattern in
    let muchuse_expr = muchuse_of_expr ~raise expr in
    let muchuse_pattern =
      List.map binders ~f:(fun b ->
          muchuse_of_binder ~raise (Binder.get_var b) expr.type_expression s)
    in
    muchuse_union muchuse_expr (muchuse_maxs muchuse_pattern)


and muchused_declarations ~(raise : _ Trace.raise) (muchuse : muchuse) = function
  | m ->
    let map, muchused = List.fold_right ~f:(muchuse_declaration ~raise) m ~init:muchuse in
    (*Put the variable in order : *)
    map, List.rev muchused


let muchused_map_program ~raise : program -> program = function
  | p, e ->
    let update_annotations annots =
      List.iter ~f:raise.Simple_utils.Trace.warning annots
    in
    let _, muchused = muchused_declarations ~raise muchuse_neutral p in
    let warn_var v =
      `Self_ast_aggregated_warning_muchused (V.get_location v, Format.asprintf "%a" V.pp v)
    in
    let () = update_annotations @@ List.map ~f:warn_var muchused in
    p, e
