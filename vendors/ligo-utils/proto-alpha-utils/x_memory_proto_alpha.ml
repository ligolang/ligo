module Michelson = Tezos_utils.Michelson

include Memory_proto_alpha
let init_environment = Init_proto_alpha.init_environment
let dummy_environment = Init_proto_alpha.dummy_environment


open Protocol
open Script_typed_ir
open Script_ir_translator
open Script_interpreter

module X = struct
  open Alpha_context
  open Script_tc_errors
  open Alpha_environment.Error_monad

  let rec stack_ty_eq
    : type ta tb. context -> int -> ta stack_ty -> tb stack_ty ->
      ((ta stack_ty, tb stack_ty) eq * context) tzresult
    = fun ctxt lvl ta tb ->
      match ta, tb with
      | Item_t (tva, ra, _), Item_t (tvb, rb, _) ->
        ty_eq ctxt tva tvb |>
        record_trace (Bad_stack_item lvl) >>? fun (Eq, ctxt) ->
        stack_ty_eq ctxt (lvl + 1) ra rb >>? fun (Eq, ctxt) ->
        (Ok (Eq, ctxt) : ((ta stack_ty, tb stack_ty) eq * context) tzresult)
      | Empty_t, Empty_t -> Ok (Eq, ctxt)
      | _, _ -> error Bad_stack_length



  open Script_typed_ir
  open Protocol.Environment.Error_monad
  module Unparse_costs = Michelson_v1_gas.Cost_of.Unparse
  open Tezos_micheline.Micheline
  open Michelson_v1_primitives
  open Protocol.Environment

  type ex_typed_value =
    Ex_typed_value : ('a Script_typed_ir.ty * 'a) -> ex_typed_value

  let rec unparse_data_generic
    : type a. context -> ?mapper:_ -> unparsing_mode -> a ty -> a -> (Script.node * context) tzresult Lwt.t
    = fun ctxt ?(mapper = fun _ -> return None) mode ty a ->
      Lwt.return (Gas.consume ctxt Unparse_costs.cycle) >>=? fun ctxt ->
      mapper (Ex_typed_value (ty, a)) >>=? function
      | Some x -> return (x , ctxt)
      | None -> (
          match ty, a with
          | Unit_t _, () ->
            Lwt.return (Gas.consume ctxt Unparse_costs.unit) >>=? fun ctxt ->
            return (Prim (-1, D_Unit, [], []), ctxt)
          | Int_t _, v ->
            Lwt.return (Gas.consume ctxt (Unparse_costs.int v)) >>=? fun ctxt ->
            return (Int (-1, Script_int.to_zint v), ctxt)
          | Nat_t _, v ->
            Lwt.return (Gas.consume ctxt (Unparse_costs.int v)) >>=? fun ctxt ->
            return (Int (-1, Script_int.to_zint v), ctxt)
          | String_t _, s ->
            Lwt.return (Gas.consume ctxt (Unparse_costs.string s)) >>=? fun ctxt ->
            return (String (-1, s), ctxt)
          | Bytes_t _, s ->
            Lwt.return (Gas.consume ctxt (Unparse_costs.bytes s)) >>=? fun ctxt ->
            return (Bytes (-1, s), ctxt)
          | Bool_t _, true ->
            Lwt.return (Gas.consume ctxt Unparse_costs.bool) >>=? fun ctxt ->
            return (Prim (-1, D_True, [], []), ctxt)
          | Bool_t _, false ->
            Lwt.return (Gas.consume ctxt Unparse_costs.bool) >>=? fun ctxt ->
            return (Prim (-1, D_False, [], []), ctxt)
          | Timestamp_t _, t ->
            Lwt.return (Gas.consume ctxt (Unparse_costs.timestamp t)) >>=? fun ctxt ->
            begin
              match mode with
              | Optimized -> return (Int (-1, Script_timestamp.to_zint t), ctxt)
              | Readable ->
                match Script_timestamp.to_notation t with
                | None -> return (Int (-1, Script_timestamp.to_zint t), ctxt)
                | Some s -> return (String (-1, s), ctxt)
            end
          | Address_t _, (c, entrypoint)  ->
            Lwt.return (Gas.consume ctxt Unparse_costs.contract) >>=? fun ctxt ->
            begin
              match mode with
              | Optimized ->
                let entrypoint = match entrypoint with "default" -> "" | name -> name in
                let bytes = Data_encoding.Binary.to_bytes_exn
                    Data_encoding.(tup2 Contract.encoding Variable.string)
                    (c, entrypoint) in
                return (Bytes (-1, bytes), ctxt)
              | Readable ->
                let notation = match entrypoint with
                  | "default" -> Contract.to_b58check c
                  | entrypoint -> Contract.to_b58check c ^ "%" ^ entrypoint in
                return (String (-1, notation), ctxt)
            end
          | Contract_t _, (_, (c, entrypoint))  ->
            Lwt.return (Gas.consume ctxt Unparse_costs.contract) >>=? fun ctxt ->
            begin
              match mode with
              | Optimized ->
                let entrypoint = match entrypoint with "default" -> "" | name -> name in
                let bytes = Data_encoding.Binary.to_bytes_exn
                    Data_encoding.(tup2 Contract.encoding Variable.string)
                    (c, entrypoint) in
                return (Bytes (-1, bytes), ctxt)
              | Readable ->
                let notation = match entrypoint with
                  | "default" -> Contract.to_b58check c
                  | entrypoint -> Contract.to_b58check c ^ "%" ^ entrypoint in
                return (String (-1, notation), ctxt)
            end
          | Signature_t _, s ->
            Lwt.return (Gas.consume ctxt Unparse_costs.signature) >>=? fun ctxt ->
            begin
              match mode with
              | Optimized ->
                let bytes = Data_encoding.Binary.to_bytes_exn Signature.encoding s in
                return (Bytes (-1, bytes), ctxt)
              | Readable ->
                return (String (-1, Signature.to_b58check s), ctxt)
            end
          | Mutez_t _, v ->
            Lwt.return (Gas.consume ctxt Unparse_costs.tez) >>=? fun ctxt ->
            return (Int (-1, Z.of_int64 (Tez.to_mutez v)), ctxt)
          | Key_t _, k ->
            Lwt.return (Gas.consume ctxt Unparse_costs.key) >>=? fun ctxt ->
            begin
              match mode with
              | Optimized ->
                let bytes = Data_encoding.Binary.to_bytes_exn Signature.Public_key.encoding k in
                return (Bytes (-1, bytes), ctxt)
              | Readable ->
                return (String (-1, Signature.Public_key.to_b58check k), ctxt)
            end
          | Key_hash_t _, k ->
            Lwt.return (Gas.consume ctxt Unparse_costs.key_hash) >>=? fun ctxt ->
            begin
              match mode with
              | Optimized ->
                let bytes = Data_encoding.Binary.to_bytes_exn Signature.Public_key_hash.encoding k in
                return (Bytes (-1, bytes), ctxt)
              | Readable ->
                return (String (-1, Signature.Public_key_hash.to_b58check k), ctxt)
            end
          | Operation_t _, (op, _big_map_diff) ->
            let bytes = Data_encoding.Binary.to_bytes_exn Alpha_context.Operation.internal_operation_encoding op in
            Lwt.return (Gas.consume ctxt (Unparse_costs.operation bytes)) >>=? fun ctxt ->
            return (Bytes (-1, bytes), ctxt)
          | Chain_id_t _, chain_id ->
            let bytes = Data_encoding.Binary.to_bytes_exn Chain_id.encoding chain_id in
            Lwt.return (Gas.consume ctxt (Unparse_costs.chain_id bytes)) >>=? fun ctxt ->
            return (Bytes (-1, bytes), ctxt)
          | Pair_t ((tl, _, _), (tr, _, _), _, _), (l, r) ->
            Lwt.return (Gas.consume ctxt Unparse_costs.pair) >>=? fun ctxt ->
            unparse_data_generic ctxt mode tl l >>=? fun (l, ctxt) ->
            unparse_data_generic ctxt mode tr r >>=? fun (r, ctxt) ->
            return (Prim (-1, D_Pair, [ l; r ], []), ctxt)
          | Union_t ((tl, _), _, _, _), L l ->
            Lwt.return (Gas.consume ctxt Unparse_costs.union) >>=? fun ctxt ->
            unparse_data_generic ctxt mode tl l >>=? fun (l, ctxt) ->
            return (Prim (-1, D_Left, [ l ], []), ctxt)
          | Union_t (_, (tr, _), _, _), R r ->
            Lwt.return (Gas.consume ctxt Unparse_costs.union) >>=? fun ctxt ->
            unparse_data_generic ctxt mode tr r >>=? fun (r, ctxt) ->
            return (Prim (-1, D_Right, [ r ], []), ctxt)
          | Option_t (t, _, _), Some v ->
            Lwt.return (Gas.consume ctxt Unparse_costs.some) >>=? fun ctxt ->
            unparse_data_generic ctxt mode t v >>=? fun (v, ctxt) ->
            return (Prim (-1, D_Some, [ v ], []), ctxt)
          | Option_t _, None ->
            Lwt.return (Gas.consume ctxt Unparse_costs.none) >>=? fun ctxt ->
            return (Prim (-1, D_None, [], []), ctxt)
          | List_t (t, _, _), items ->
            fold_left_s
              (fun (l, ctxt) element ->
                 Lwt.return (Gas.consume ctxt Unparse_costs.list_element) >>=? fun ctxt ->
                 unparse_data_generic ctxt mode t element >>=? fun (unparsed, ctxt) ->
                 return (unparsed :: l, ctxt))
              ([], ctxt)
              items >>=? fun (items, ctxt) ->
            return (Micheline.Seq (-1, List.rev items), ctxt)
          | Set_t (t, _), set ->
            let t = ty_of_comparable_ty t in
            fold_left_s
              (fun (l, ctxt) item ->
                 Lwt.return (Gas.consume ctxt Unparse_costs.set_element) >>=? fun ctxt ->
                 unparse_data_generic ctxt mode t item >>=? fun (item, ctxt) ->
                 return (item :: l, ctxt))
              ([], ctxt)
              (set_fold (fun e acc -> e :: acc) set []) >>=? fun (items, ctxt) ->
            return (Micheline.Seq (-1, items), ctxt)
          | Map_t (kt, vt, _, _), map ->
            let kt = ty_of_comparable_ty kt in
            fold_left_s
              (fun (l, ctxt) (k, v) ->
                 Lwt.return (Gas.consume ctxt Unparse_costs.map_element) >>=? fun ctxt ->
                 unparse_data_generic ctxt mode kt k >>=? fun (key, ctxt) ->
                 unparse_data_generic ctxt mode vt v >>=? fun (value, ctxt) ->
                 return (Prim (-1, D_Elt, [ key ; value ], []) :: l, ctxt))
              ([], ctxt)
              (map_fold (fun k v acc -> (k, v) :: acc) map []) >>=? fun (items, ctxt) ->
            return (Micheline.Seq (-1, items), ctxt)
          | Big_map_t (kt, vt, _), { id = None ; diff = (module Diff) ; _ } ->
            (* this branch is to allow roundtrip of big map literals *)
            let kt = ty_of_comparable_ty kt in
            fold_left_s
              (fun (l, ctxt) (k, v) ->
                 Lwt.return (Gas.consume ctxt Unparse_costs.map_element) >>=? fun ctxt ->
                 unparse_data_generic ctxt mode kt k >>=? fun (key, ctxt) ->
                 unparse_data_generic ctxt mode vt v >>=? fun (value, ctxt) ->
                 return (Prim (-1, D_Elt, [ key ; value ], []) :: l, ctxt))
              ([], ctxt)
              (Diff.OPS.fold
                 (fun k v acc -> match v with | None -> acc | Some v -> (k, v) :: acc)
                 (fst Diff.boxed) []) >>=? fun (items, ctxt) ->
            return (Micheline.Seq (-1, items), ctxt)
          | Big_map_t (_kt, _kv, _), { id = Some id ; diff = (module Diff) ; _ } ->
            if Compare.Int.(Diff.OPS.cardinal (fst Diff.boxed) = 0) then
              return (Micheline.Int (-1, id), ctxt)
            else
              (* this can only be the result of an execution and the map
                 must have been flushed at this point *)
              assert false
          | Lambda_t _, Lam (_, original_code) ->
            unparse_code_generic ctxt ~mapper mode original_code
        )

  and unparse_code_generic ctxt ?mapper mode =
    let legacy = true in
    function
    | Prim (loc, I_PUSH, [ ty ; data ], annot) ->
      Lwt.return (parse_packable_ty ctxt ~legacy ty) >>=? fun (Ex_ty t, ctxt) ->
      parse_data ctxt ~legacy t data >>=? fun (data, ctxt) ->
      unparse_data_generic ctxt ?mapper mode t data >>=? fun (data, ctxt) ->
      Lwt.return (Gas.consume ctxt (Unparse_costs.prim_cost 2 annot)) >>=? fun ctxt ->
      return (Prim (loc, I_PUSH, [ ty ; data ], annot), ctxt)
    | Seq (loc, items) ->
      fold_left_s
        (fun (l, ctxt) item ->
           unparse_code_generic ctxt ?mapper mode item >>=? fun (item, ctxt) ->
           return (item :: l, ctxt))
        ([], ctxt) items >>=? fun (items, ctxt) ->
      Lwt.return (Gas.consume ctxt (Unparse_costs.seq_cost (List.length items))) >>=? fun ctxt ->
      return (Micheline.Seq (loc, List.rev items), ctxt)
    | Prim (loc, prim, items, annot) ->
      fold_left_s
        (fun (l, ctxt) item ->
           unparse_code_generic ctxt ?mapper mode item >>=? fun (item, ctxt) ->
           return (item :: l, ctxt))
        ([], ctxt) items >>=? fun (items, ctxt) ->
      Lwt.return (Gas.consume ctxt (Unparse_costs.prim_cost 3 annot)) >>=? fun ctxt ->
      return (Prim (loc, prim, List.rev items, annot), ctxt)
    | Int _ | String _ | Bytes _ as atom -> return (atom, ctxt)


module Interp_costs = Michelson_v1_gas.Cost_of
type ex_descr_stack = Ex_descr_stack : (('a, 'b) descr * 'a stack) -> ex_descr_stack

let unparse_stack ctxt (stack, stack_ty) =
  (* We drop the gas limit as this function is only used for debugging/errors. *)
  let ctxt = Gas.set_unlimited ctxt in
  let rec unparse_stack
    : type a. a stack * a stack_ty -> (Script.expr * string option) list tzresult Lwt.t
    = function
      | Empty, Empty_t -> return_nil
      | Item (v, rest), Item_t (ty, rest_ty, annot) ->
          unparse_data ctxt Readable ty v >>=? fun (data, _ctxt) ->
          unparse_stack (rest, rest_ty) >>=? fun rest ->
          let annot = match Script_ir_annot.unparse_var_annot annot with
            | [] -> None
            | [ a ] -> Some a
            | _ -> assert false in
          let data = Micheline.strip_locations data in
          return ((data, annot) :: rest) in
  unparse_stack (stack, stack_ty)

(* let rec step
 *   : type b a.
 *     (?log: execution_trace ref ->
 *      context ->
 *      source: Contract.t ->
 *      self: Contract.t ->
 *      payer: Contract.t ->
 *      ?visitor: (ex_descr_stack -> unit) ->
 *      Tez.t ->
 *      (b, a) descr -> b stack ->
 *      (a stack * context) tzresult Lwt.t) =
 *   fun ?log ctxt ~source ~self ~payer ?visitor amount ({ instr ; loc ; _ } as descr) stack ->
 *     Lwt.return (Gas.consume ctxt Interp_costs.cycle) >>=? fun ctxt ->
 *     (match visitor with
 *      | Some visitor -> visitor @@ Ex_descr_stack(descr, stack)
 *      | None -> ()) ;
 *     let step_same ctxt = step ?log ctxt ~source ~self ~payer ?visitor amount in
 *     let logged_return : type a b.
 *       (b, a) descr ->
 *       a stack * context ->
 *       (a stack * context) tzresult Lwt.t =
 *       fun descr (ret, ctxt) ->
 *         match log with
 *         | None -> return (ret, ctxt)
 *         | Some log ->
 *             trace
 *               Cannot_serialize_log
 *               (unparse_stack ctxt (ret, descr.aft)) >>=? fun stack ->
 *             log := (descr.loc, Gas.level ctxt, stack) :: !log ;
 *             return (ret, ctxt) in
 *     let get_log (log : execution_trace ref option) =
 *       Option.map ~f:(fun l -> List.rev !l) log in
 *     let consume_gas_terop : type ret arg1 arg2 arg3 rest.
 *       (_ * (_ * (_ * rest)), ret * rest) descr ->
 *       ((arg1 -> arg2 -> arg3 -> ret) * arg1 * arg2 * arg3) ->
 *       (arg1 -> arg2 -> arg3 -> Gas.cost) ->
 *       rest stack ->
 *       ((ret * rest) stack * context) tzresult Lwt.t =
 *       fun descr (op, x1, x2, x3) cost_func rest ->
 *         Lwt.return (Gas.consume ctxt (cost_func x1 x2 x3)) >>=? fun ctxt ->
 *         logged_return descr (Item (op x1 x2 x3, rest), ctxt) in
 *     let consume_gas_binop : type ret arg1 arg2 rest.
 *       (_ * (_ * rest), ret * rest) descr ->
 *       ((arg1 -> arg2 -> ret) * arg1 * arg2) ->
 *       (arg1 -> arg2 -> Gas.cost) ->
 *       rest stack ->
 *       context ->
 *       ((ret * rest) stack * context) tzresult Lwt.t =
 *       fun descr (op, x1, x2) cost_func rest ctxt ->
 *         Lwt.return (Gas.consume ctxt (cost_func x1 x2)) >>=? fun ctxt ->
 *         logged_return descr (Item (op x1 x2, rest), ctxt) in
 *     let consume_gas_unop : type ret arg rest.
 *       (_ * rest, ret * rest) descr ->
 *       ((arg -> ret) * arg) ->
 *       (arg -> Gas.cost) ->
 *       rest stack ->
 *       context ->
 *       ((ret * rest) stack * context) tzresult Lwt.t =
 *       fun descr (op, arg) cost_func rest ctxt ->
 *         Lwt.return (Gas.consume ctxt (cost_func arg)) >>=? fun ctxt ->
 *         logged_return descr (Item (op arg, rest), ctxt) in
 *     let consume_gaz_comparison :
 *       type t rest.
 *       (t * (t * rest), Script_int.z Script_int.num * rest) descr ->
 *       (t -> t -> int) ->
 *       (t -> t -> Gas.cost) ->
 *       t -> t ->
 *       rest stack ->
 *       ((Script_int.z Script_int.num * rest) stack * context) tzresult Lwt.t =
 *       fun descr op cost x1 x2 rest ->
 *         Lwt.return (Gas.consume ctxt (cost x1 x2)) >>=? fun ctxt ->
 *         logged_return descr (Item (Script_int.of_int @@ op x1 x2, rest), ctxt) in
 *     let logged_return :
 *       a stack * context ->
 *       (a stack * context) tzresult Lwt.t =
 *       logged_return descr in
 *     match instr, stack with
 *     (\* stack ops *\)
 *     | Drop, Item (_, rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.stack_op) >>=? fun ctxt ->
 *         logged_return (rest, ctxt)
 *     | Dup, Item (v, rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.stack_op) >>=? fun ctxt ->
 *         logged_return (Item (v, Item (v, rest)), ctxt)
 *     | Swap, Item (vi, Item (vo, rest)) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.stack_op) >>=? fun ctxt ->
 *         logged_return (Item (vo, Item (vi, rest)), ctxt)
 *     | Const v, rest ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.push) >>=? fun ctxt ->
 *         logged_return (Item (v, rest), ctxt)
 *     (\* options *\)
 *     | Cons_some, Item (v, rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.wrap) >>=? fun ctxt ->
 *         logged_return (Item (Some v, rest), ctxt)
 *     | Cons_none _, rest ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.variant_no_data) >>=? fun ctxt ->
 *         logged_return (Item (None, rest), ctxt)
 *     | If_none (bt, _), Item (None, rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.branch) >>=? fun ctxt ->
 *         step_same ctxt bt rest
 *     | If_none (_, bf), Item (Some v, rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.branch) >>=? fun ctxt ->
 *         step_same ctxt bf (Item (v, rest))
 *     (\* pairs *\)
 *     | Cons_pair, Item (a, Item (b, rest)) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.pair) >>=? fun ctxt ->
 *         logged_return (Item ((a, b), rest), ctxt)
 *     | Car, Item ((a, _), rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.pair_access) >>=? fun ctxt ->
 *         logged_return (Item (a, rest), ctxt)
 *     | Cdr, Item ((_, b), rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.pair_access) >>=? fun ctxt ->
 *         logged_return (Item (b, rest), ctxt)
 *     (\* unions *\)
 *     | Left, Item (v, rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.wrap) >>=? fun ctxt ->
 *         logged_return (Item (L v, rest), ctxt)
 *     | Right, Item (v, rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.wrap) >>=? fun ctxt ->
 *         logged_return (Item (R v, rest), ctxt)
 *     | If_left (bt, _), Item (L v, rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.branch) >>=? fun ctxt ->
 *         step_same ctxt bt (Item (v, rest))
 *     | If_left (_, bf), Item (R v, rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.branch) >>=? fun ctxt ->
 *         step_same ctxt bf (Item (v, rest))
 *     (\* lists *\)
 *     | Cons_list, Item (hd, Item (tl, rest)) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.cons) >>=? fun ctxt ->
 *         logged_return (Item (hd :: tl, rest), ctxt)
 *     | Nil, rest ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.variant_no_data) >>=? fun ctxt ->
 *         logged_return (Item ([], rest), ctxt)
 *     | If_cons (_, bf), Item ([], rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.branch) >>=? fun ctxt ->
 *         step_same ctxt bf rest
 *     | If_cons (bt, _), Item (hd :: tl, rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.branch) >>=? fun ctxt ->
 *         step_same ctxt bt (Item (hd, Item (tl, rest)))
 *     | List_map body, Item (l, rest) ->
 *         let rec loop rest ctxt l acc =
 *           Lwt.return (Gas.consume ctxt Interp_costs.loop_cycle) >>=? fun ctxt ->
 *           match l with
 *           | [] -> return (Item (List.rev acc, rest), ctxt)
 *           | hd :: tl ->
 *               step_same ctxt body (Item (hd, rest))
 *               >>=? fun (Item (hd, rest), ctxt) ->
 *               loop rest ctxt tl (hd :: acc)
 *         in loop rest ctxt l [] >>=? fun (res, ctxt) ->
 *         logged_return (res, ctxt)
 *     | List_size, Item (list, rest) ->
 *         Lwt.return
 *           (List.fold_left
 *              (fun acc _ ->
 *                 acc >>? fun (size, ctxt) ->
 *                 Gas.consume ctxt Interp_costs.list_size >>? fun ctxt ->
 *                 ok (size + 1 (\* FIXME: overflow *\), ctxt))
 *              (ok (0, ctxt)) list) >>=? fun (len, ctxt) ->
 *         logged_return (Item (Script_int.(abs (of_int len)), rest), ctxt)
 *     | List_iter body, Item (l, init) ->
 *         let rec loop ctxt l stack =
 *           Lwt.return (Gas.consume ctxt Interp_costs.loop_cycle) >>=? fun ctxt ->
 *           match l with
 *           | [] -> return (stack, ctxt)
 *           | hd :: tl ->
 *               step_same ctxt body (Item (hd, stack))
 *               >>=? fun (stack, ctxt) ->
 *               loop ctxt tl stack
 *         in loop ctxt l init >>=? fun (res, ctxt) ->
 *         logged_return (res, ctxt)
 *     (\* sets *\)
 *     | Empty_set t, rest ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.empty_set) >>=? fun ctxt ->
 *         logged_return (Item (empty_set t, rest), ctxt)
 *     | Set_iter body, Item (set, init) ->
 *         Lwt.return (Gas.consume ctxt (Interp_costs.set_to_list set)) >>=? fun ctxt ->
 *         let l = List.rev (set_fold (fun e acc -> e :: acc) set []) in
 *         let rec loop ctxt l stack =
 *           Lwt.return (Gas.consume ctxt Interp_costs.loop_cycle) >>=? fun ctxt ->
 *           match l with
 *           | [] -> return (stack, ctxt)
 *           | hd :: tl ->
 *               step_same ctxt body (Item (hd, stack))
 *               >>=? fun (stack, ctxt) ->
 *               loop ctxt tl stack
 *         in loop ctxt l init >>=? fun (res, ctxt) ->
 *         logged_return (res, ctxt)
 *     | Set_mem, Item (v, Item (set, rest)) ->
 *         consume_gas_binop descr (set_mem, v, set) Interp_costs.set_mem rest ctxt
 *     | Set_update, Item (v, Item (presence, Item (set, rest))) ->
 *         consume_gas_terop descr (set_update, v, presence, set) Interp_costs.set_update rest
 *     | Set_size, Item (set, rest) ->
 *         consume_gas_unop descr (set_size, set) (fun _ -> Interp_costs.set_size) rest ctxt
 *     (\* maps *\)
 *     | Empty_map (t, _), rest ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.empty_map) >>=? fun ctxt ->
 *         logged_return (Item (empty_map t, rest), ctxt)
 *     | Map_map body, Item (map, rest) ->
 *         Lwt.return (Gas.consume ctxt (Interp_costs.map_to_list map)) >>=? fun ctxt ->
 *         let l = List.rev (map_fold (fun k v acc -> (k, v) :: acc) map []) in
 *         let rec loop rest ctxt l acc =
 *           Lwt.return (Gas.consume ctxt Interp_costs.loop_cycle) >>=? fun ctxt ->
 *           match l with
 *           | [] -> return (acc, ctxt)
 *           | (k, _) as hd :: tl ->
 *               step_same ctxt body (Item (hd, rest))
 *               >>=? fun (Item (hd, rest), ctxt) ->
 *               loop rest ctxt tl (map_update k (Some hd) acc)
 *         in loop rest ctxt l (empty_map (map_key_ty map)) >>=? fun (res, ctxt) ->
 *         logged_return (Item (res, rest), ctxt)
 *     | Map_iter body, Item (map, init) ->
 *         Lwt.return (Gas.consume ctxt (Interp_costs.map_to_list map)) >>=? fun ctxt ->
 *         let l = List.rev (map_fold (fun k v acc -> (k, v) :: acc) map []) in
 *         let rec loop ctxt l stack =
 *           Lwt.return (Gas.consume ctxt Interp_costs.loop_cycle) >>=? fun ctxt ->
 *           match l with
 *           | [] -> return (stack, ctxt)
 *           | hd :: tl ->
 *               step_same ctxt body (Item (hd, stack))
 *               >>=? fun (stack, ctxt) ->
 *               loop ctxt tl stack
 *         in loop ctxt l init >>=? fun (res, ctxt) ->
 *         logged_return (res, ctxt)
 *     | Map_mem, Item (v, Item (map, rest)) ->
 *         consume_gas_binop descr (map_mem, v, map) Interp_costs.map_mem rest ctxt
 *     | Map_get, Item (v, Item (map, rest)) ->
 *         consume_gas_binop descr (map_get, v, map) Interp_costs.map_get rest ctxt
 *     | Map_update, Item (k, Item (v, Item (map, rest))) ->
 *         consume_gas_terop descr (map_update, k, v, map) Interp_costs.map_update rest
 *     | Map_size, Item (map, rest) ->
 *         consume_gas_unop descr (map_size, map) (fun _ -> Interp_costs.map_size) rest ctxt
 *     (\* Big map operations *\)
 *     | Big_map_mem, Item (key, Item (map, rest)) ->
 *         Lwt.return (Gas.consume ctxt (Interp_costs.big_map_mem key map)) >>=? fun ctxt ->
 *         Script_ir_translator.big_map_mem ctxt self key map >>=? fun (res, ctxt) ->
 *         logged_return (Item (res, rest), ctxt)
 *     | Big_map_get, Item (key, Item (map, rest)) ->
 *         Lwt.return (Gas.consume ctxt (Interp_costs.big_map_get key map)) >>=? fun ctxt ->
 *         Script_ir_translator.big_map_get ctxt self key map >>=? fun (res, ctxt) ->
 *         logged_return (Item (res, rest), ctxt)
 *     | Big_map_update, Item (key, Item (maybe_value, Item (map, rest))) ->
 *         consume_gas_terop descr
 *           (Script_ir_translator.big_map_update, key, maybe_value, map)
 *           Interp_costs.big_map_update rest
 *     (\* timestamp operations *\)
 *     | Add_seconds_to_timestamp, Item (n, Item (t, rest)) ->
 *         consume_gas_binop descr
 *           (Script_timestamp.add_delta, t, n)
 *           Interp_costs.add_timestamp rest ctxt
 *     | Add_timestamp_to_seconds, Item (t, Item (n, rest)) ->
 *         consume_gas_binop descr (Script_timestamp.add_delta, t, n)
 *           Interp_costs.add_timestamp rest ctxt
 *     | Sub_timestamp_seconds, Item (t, Item (s, rest)) ->
 *         consume_gas_binop descr (Script_timestamp.sub_delta, t, s)
 *           Interp_costs.sub_timestamp rest ctxt
 *     | Diff_timestamps, Item (t1, Item (t2, rest)) ->
 *         consume_gas_binop descr (Script_timestamp.diff, t1, t2)
 *           Interp_costs.diff_timestamps rest ctxt
 *     (\* string operations *\)
 *     | Concat_string_pair, Item (x, Item (y, rest)) ->
 *         Lwt.return (Gas.consume ctxt (Interp_costs.concat_string [x; y])) >>=? fun ctxt ->
 *         let s = String.concat "" [x; y] in
 *         logged_return (Item (s, rest), ctxt)
 *     | Concat_string, Item (ss, rest) ->
 *         Lwt.return (Gas.consume ctxt (Interp_costs.concat_string ss)) >>=? fun ctxt ->
 *         let s = String.concat "" ss in
 *         logged_return (Item (s, rest), ctxt)
 *     | Slice_string, Item (offset, Item (length, Item (s, rest))) ->
 *         let s_length = Z.of_int (String.length s) in
 *         let offset = Script_int.to_zint offset in
 *         let length = Script_int.to_zint length in
 *         if Compare.Z.(offset < s_length && Z.add offset length <= s_length) then
 *           Lwt.return (Gas.consume ctxt (Interp_costs.slice_string (Z.to_int length))) >>=? fun ctxt ->
 *           logged_return (Item (Some (String.sub s (Z.to_int offset) (Z.to_int length)), rest), ctxt)
 *         else
 *           Lwt.return (Gas.consume ctxt (Interp_costs.slice_string 0)) >>=? fun ctxt ->
 *           logged_return (Item (None, rest), ctxt)
 *     | String_size, Item (s, rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.push) >>=? fun ctxt ->
 *         logged_return (Item (Script_int.(abs (of_int (String.length s))), rest), ctxt)
 *     (\* bytes operations *\)
 *     | Concat_bytes_pair, Item (x, Item (y, rest)) ->
 *         Lwt.return (Gas.consume ctxt (Interp_costs.concat_bytes [x; y])) >>=? fun ctxt ->
 *         let s = MBytes.concat "" [x; y] in
 *         logged_return (Item (s, rest), ctxt)
 *     | Concat_bytes, Item (ss, rest) ->
 *         Lwt.return (Gas.consume ctxt (Interp_costs.concat_bytes ss)) >>=? fun ctxt ->
 *         let s = MBytes.concat "" ss in
 *         logged_return (Item (s, rest), ctxt)
 *     | Slice_bytes, Item (offset, Item (length, Item (s, rest))) ->
 *         let s_length = Z.of_int (MBytes.length s) in
 *         let offset = Script_int.to_zint offset in
 *         let length = Script_int.to_zint length in
 *         if Compare.Z.(offset < s_length && Z.add offset length <= s_length) then
 *           Lwt.return (Gas.consume ctxt (Interp_costs.slice_string (Z.to_int length))) >>=? fun ctxt ->
 *           logged_return (Item (Some (MBytes.sub s (Z.to_int offset) (Z.to_int length)), rest), ctxt)
 *         else
 *           Lwt.return (Gas.consume ctxt (Interp_costs.slice_string 0)) >>=? fun ctxt ->
 *           logged_return (Item (None, rest), ctxt)
 *     | Bytes_size, Item (s, rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.push) >>=? fun ctxt ->
 *         logged_return (Item (Script_int.(abs (of_int (MBytes.length s))), rest), ctxt)
 *     (\* currency operations *\)
 *     | Add_tez, Item (x, Item (y, rest)) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.int64_op) >>=? fun ctxt ->
 *         Lwt.return Tez.(x +? y) >>=? fun res ->
 *         logged_return (Item (res, rest), ctxt)
 *     | Sub_tez, Item (x, Item (y, rest)) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.int64_op) >>=? fun ctxt ->
 *         Lwt.return Tez.(x -? y) >>=? fun res ->
 *         logged_return (Item (res, rest), ctxt)
 *     | Mul_teznat, Item (x, Item (y, rest)) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.int64_op) >>=? fun ctxt ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.z_to_int64) >>=? fun ctxt ->
 *         begin
 *           match Script_int.to_int64 y with
 *           | None -> fail (Overflow (loc, get_log log))
 *           | Some y ->
 *               Lwt.return Tez.(x *? y) >>=? fun res ->
 *               logged_return (Item (res, rest), ctxt)
 *         end
 *     | Mul_nattez, Item (y, Item (x, rest)) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.int64_op) >>=? fun ctxt ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.z_to_int64) >>=? fun ctxt ->
 *         begin
 *           match Script_int.to_int64 y with
 *           | None -> fail (Overflow (loc, get_log log))
 *           | Some y ->
 *               Lwt.return Tez.(x *? y) >>=? fun res ->
 *               logged_return (Item (res, rest), ctxt)
 *         end
 *     (\* boolean operations *\)
 *     | Or, Item (x, Item (y, rest)) ->
 *         consume_gas_binop descr ((||), x, y) Interp_costs.bool_binop rest ctxt
 *     | And, Item (x, Item (y, rest)) ->
 *         consume_gas_binop descr ((&&), x, y) Interp_costs.bool_binop rest ctxt
 *     | Xor, Item (x, Item (y, rest)) ->
 *         consume_gas_binop descr (Compare.Bool.(<>), x, y) Interp_costs.bool_binop rest ctxt
 *     | Not, Item (x, rest) ->
 *         consume_gas_unop descr (not, x) Interp_costs.bool_unop rest ctxt
 *     (\* integer operations *\)
 *     | Is_nat, Item (x, rest) ->
 *         consume_gas_unop descr (Script_int.is_nat, x) Interp_costs.abs rest ctxt
 *     | Abs_int, Item (x, rest) ->
 *         consume_gas_unop descr (Script_int.abs, x) Interp_costs.abs rest ctxt
 *     | Int_nat, Item (x, rest) ->
 *         consume_gas_unop descr (Script_int.int, x) Interp_costs.int rest ctxt
 *     | Neg_int, Item (x, rest) ->
 *         consume_gas_unop descr (Script_int.neg, x) Interp_costs.neg rest ctxt
 *     | Neg_nat, Item (x, rest) ->
 *         consume_gas_unop descr (Script_int.neg, x) Interp_costs.neg rest ctxt
 *     | Add_intint, Item (x, Item (y, rest)) ->
 *         consume_gas_binop descr (Script_int.add, x, y) Interp_costs.add rest ctxt
 *     | Add_intnat, Item (x, Item (y, rest)) ->
 *         consume_gas_binop descr (Script_int.add, x, y) Interp_costs.add rest ctxt
 *     | Add_natint, Item (x, Item (y, rest)) ->
 *         consume_gas_binop descr (Script_int.add, x, y) Interp_costs.add rest ctxt
 *     | Add_natnat, Item (x, Item (y, rest)) ->
 *         consume_gas_binop descr (Script_int.add_n, x, y) Interp_costs.add rest ctxt
 *     | Sub_int, Item (x, Item (y, rest)) ->
 *         consume_gas_binop descr (Script_int.sub, x, y) Interp_costs.sub rest ctxt
 *     | Mul_intint, Item (x, Item (y, rest)) ->
 *         consume_gas_binop descr (Script_int.mul, x, y) Interp_costs.mul rest ctxt
 *     | Mul_intnat, Item (x, Item (y, rest)) ->
 *         consume_gas_binop descr (Script_int.mul, x, y) Interp_costs.mul rest ctxt
 *     | Mul_natint, Item (x, Item (y, rest)) ->
 *         consume_gas_binop descr (Script_int.mul, x, y) Interp_costs.mul rest ctxt
 *     | Mul_natnat, Item (x, Item (y, rest)) ->
 *         consume_gas_binop descr (Script_int.mul_n, x, y) Interp_costs.mul rest ctxt
 *     | Ediv_teznat, Item (x, Item (y, rest)) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.int64_to_z) >>=? fun ctxt ->
 *         let x = Script_int.of_int64 (Tez.to_mutez x) in
 *         consume_gas_binop descr
 *           ((fun x y ->
 *               match Script_int.ediv x y with
 *               | None -> None
 *               | Some (q, r) ->
 *                   match Script_int.to_int64 q,
 *                         Script_int.to_int64 r with
 *                   | Some q, Some r ->
 *                       begin
 *                         match Tez.of_mutez q, Tez.of_mutez r with
 *                         | Some q, Some r -> Some (q,r)
 *                         (\* Cannot overflow *\)
 *                         | _ -> assert false
 *                       end
 *                   (\* Cannot overflow *\)
 *                   | _ -> assert false),
 *            x, y)
 *           Interp_costs.div
 *           rest
 *           ctxt
 *     | Ediv_tez, Item (x, Item (y, rest)) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.int64_to_z) >>=? fun ctxt ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.int64_to_z) >>=? fun ctxt ->
 *         let x = Script_int.abs (Script_int.of_int64 (Tez.to_mutez x)) in
 *         let y = Script_int.abs (Script_int.of_int64 (Tez.to_mutez y)) in
 *         consume_gas_binop descr
 *           ((fun x y -> match Script_int.ediv_n x y with
 *               | None -> None
 *               | Some (q, r) ->
 *                   match Script_int.to_int64 r with
 *                   | None -> assert false (\* Cannot overflow *\)
 *                   | Some r ->
 *                       match Tez.of_mutez r with
 *                       | None -> assert false (\* Cannot overflow *\)
 *                       | Some r -> Some (q, r)),
 *            x, y)
 *           Interp_costs.div
 *           rest
 *           ctxt
 *     | Ediv_intint, Item (x, Item (y, rest)) ->
 *         consume_gas_binop descr (Script_int.ediv, x, y) Interp_costs.div rest ctxt
 *     | Ediv_intnat, Item (x, Item (y, rest)) ->
 *         consume_gas_binop descr (Script_int.ediv, x, y) Interp_costs.div rest ctxt
 *     | Ediv_natint, Item (x, Item (y, rest)) ->
 *         consume_gas_binop descr (Script_int.ediv, x, y) Interp_costs.div rest ctxt
 *     | Ediv_natnat, Item (x, Item (y, rest)) ->
 *         consume_gas_binop descr (Script_int.ediv_n, x, y) Interp_costs.div rest ctxt
 *     | Lsl_nat, Item (x, Item (y, rest)) ->
 *         Lwt.return (Gas.consume ctxt (Interp_costs.shift_left x y)) >>=? fun ctxt ->
 *         begin
 *           match Script_int.shift_left_n x y with
 *           | None -> fail (Overflow (loc, get_log log))
 *           | Some x -> logged_return (Item (x, rest), ctxt)
 *         end
 *     | Lsr_nat, Item (x, Item (y, rest)) ->
 *         Lwt.return (Gas.consume ctxt (Interp_costs.shift_right x y)) >>=? fun ctxt ->
 *         begin
 *           match Script_int.shift_right_n x y with
 *           | None -> fail (Overflow (loc, get_log log))
 *           | Some r -> logged_return (Item (r, rest), ctxt)
 *         end
 *     | Or_nat, Item (x, Item (y, rest)) ->
 *         consume_gas_binop descr (Script_int.logor, x, y) Interp_costs.logor rest ctxt
 *     | And_nat, Item (x, Item (y, rest)) ->
 *         consume_gas_binop descr (Script_int.logand, x, y) Interp_costs.logand rest ctxt
 *     | And_int_nat, Item (x, Item (y, rest)) ->
 *         consume_gas_binop descr (Script_int.logand, x, y) Interp_costs.logand rest ctxt
 *     | Xor_nat, Item (x, Item (y, rest)) ->
 *         consume_gas_binop descr (Script_int.logxor, x, y) Interp_costs.logxor rest ctxt
 *     | Not_int, Item (x, rest) ->
 *         consume_gas_unop descr (Script_int.lognot, x) Interp_costs.lognot rest ctxt
 *     | Not_nat, Item (x, rest) ->
 *         consume_gas_unop descr (Script_int.lognot, x) Interp_costs.lognot rest ctxt
 *     (\* control *\)
 *     | Seq (hd, tl), stack ->
 *         step_same ctxt hd stack >>=? fun (trans, ctxt) ->
 *         step_same ctxt tl trans
 *     | If (bt, _), Item (true, rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.branch) >>=? fun ctxt ->
 *         step_same ctxt bt rest
 *     | If (_, bf), Item (false, rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.branch) >>=? fun ctxt ->
 *         step_same ctxt bf rest
 *     | Loop body, Item (true, rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.loop_cycle) >>=? fun ctxt ->
 *         step_same ctxt body rest >>=? fun (trans, ctxt) ->
 *         step_same ctxt descr trans
 *     | Loop _, Item (false, rest) ->
 *         logged_return (rest, ctxt)
 *     | Loop_left body, Item (L v, rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.loop_cycle) >>=? fun ctxt ->
 *         step_same ctxt body (Item (v, rest)) >>=? fun (trans, ctxt) ->
 *         step_same ctxt descr trans
 *     | Loop_left _, Item (R v, rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.loop_cycle) >>=? fun ctxt ->
 *         logged_return (Item (v, rest), ctxt)
 *     | Dip b, Item (ign, rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.stack_op) >>=? fun ctxt ->
 *         step_same ctxt b rest >>=? fun (res, ctxt) ->
 *         logged_return (Item (ign, res), ctxt)
 *     | Exec, Item (arg, Item (lam, rest)) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.exec) >>=? fun ctxt ->
 *         interp ?log ctxt ~source ~payer ~self amount lam arg >>=? fun (res, ctxt) ->
 *         logged_return (Item (res, rest), ctxt)
 *     | Lambda lam, rest ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.push) >>=? fun ctxt ->
 *         logged_return (Item (lam, rest), ctxt)
 *     | Failwith tv, Item (v, _) ->
 *         trace Cannot_serialize_failure
 *           (unparse_data ctxt Optimized tv v) >>=? fun (v, _ctxt) ->
 *         let v = Micheline.strip_locations v in
 *         fail (Reject (loc, v, get_log log))
 *     | Nop, stack ->
 *         logged_return (stack, ctxt)
 *     (\* comparison *\)
 *     | Compare (Bool_key _), Item (a, Item (b, rest)) ->
 *         consume_gaz_comparison descr Compare.Bool.compare Interp_costs.compare_bool a b rest
 *     | Compare (String_key _), Item (a, Item (b, rest)) ->
 *         consume_gaz_comparison descr Compare.String.compare Interp_costs.compare_string a b rest
 *     | Compare (Bytes_key _), Item (a, Item (b, rest)) ->
 *         consume_gaz_comparison descr MBytes.compare Interp_costs.compare_bytes a b rest
 *     | Compare (Mutez_key _), Item (a, Item (b, rest)) ->
 *         consume_gaz_comparison descr Tez.compare Interp_costs.compare_tez a b rest
 *     | Compare (Int_key _), Item (a, Item (b, rest)) ->
 *         consume_gaz_comparison descr Script_int.compare Interp_costs.compare_int a b rest
 *     | Compare (Nat_key _), Item (a, Item (b, rest)) ->
 *         consume_gaz_comparison descr Script_int.compare Interp_costs.compare_nat a b rest
 *     | Compare (Key_hash_key _), Item (a, Item (b, rest)) ->
 *         consume_gaz_comparison descr Signature.Public_key_hash.compare
 *           Interp_costs.compare_key_hash a b rest
 *     | Compare (Timestamp_key _), Item (a, Item (b, rest)) ->
 *         consume_gaz_comparison descr Script_timestamp.compare Interp_costs.compare_timestamp a b rest
 *     | Compare (Address_key _), Item (a, Item (b, rest)) ->
 *         consume_gaz_comparison descr Contract.compare Interp_costs.compare_address a b rest
 *     (\* comparators *\)
 *     | Eq, Item (cmpres, rest) ->
 *         let cmpres = Script_int.compare cmpres Script_int.zero in
 *         let cmpres = Compare.Int.(cmpres = 0) in
 *         Lwt.return (Gas.consume ctxt Interp_costs.compare_res) >>=? fun ctxt ->
 *         logged_return (Item (cmpres, rest), ctxt)
 *     | Neq, Item (cmpres, rest) ->
 *         let cmpres = Script_int.compare cmpres Script_int.zero in
 *         let cmpres = Compare.Int.(cmpres <> 0) in
 *         Lwt.return (Gas.consume ctxt Interp_costs.compare_res) >>=? fun ctxt ->
 *         logged_return (Item (cmpres, rest), ctxt)
 *     | Lt, Item (cmpres, rest) ->
 *         let cmpres = Script_int.compare cmpres Script_int.zero in
 *         let cmpres = Compare.Int.(cmpres < 0) in
 *         Lwt.return (Gas.consume ctxt Interp_costs.compare_res) >>=? fun ctxt ->
 *         logged_return (Item (cmpres, rest), ctxt)
 *     | Le, Item (cmpres, rest) ->
 *         let cmpres = Script_int.compare cmpres Script_int.zero in
 *         let cmpres = Compare.Int.(cmpres <= 0) in
 *         Lwt.return (Gas.consume ctxt Interp_costs.compare_res) >>=? fun ctxt ->
 *         logged_return (Item (cmpres, rest), ctxt)
 *     | Gt, Item (cmpres, rest) ->
 *         let cmpres = Script_int.compare cmpres Script_int.zero in
 *         let cmpres = Compare.Int.(cmpres > 0) in
 *         Lwt.return (Gas.consume ctxt Interp_costs.compare_res) >>=? fun ctxt ->
 *         logged_return (Item (cmpres, rest), ctxt)
 *     | Ge, Item (cmpres, rest) ->
 *         let cmpres = Script_int.compare cmpres Script_int.zero in
 *         let cmpres = Compare.Int.(cmpres >= 0) in
 *         Lwt.return (Gas.consume ctxt Interp_costs.compare_res) >>=? fun ctxt ->
 *         logged_return (Item (cmpres, rest), ctxt)
 *     (\* packing *\)
 *     | Pack t, Item (value, rest) ->
 *         Script_ir_translator.pack_data ctxt t value >>=? fun (bytes, ctxt) ->
 *         logged_return (Item (bytes, rest), ctxt)
 *     | Unpack t, Item (bytes, rest) ->
 *         Lwt.return (Gas.check_enough ctxt (Script.serialized_cost bytes)) >>=? fun () ->
 *         if Compare.Int.(MBytes.length bytes >= 1) &&
 *            Compare.Int.(MBytes.get_uint8 bytes 0 = 0x05) then
 *           let bytes = MBytes.sub bytes 1 (MBytes.length bytes - 1) in
 *           match Data_encoding.Binary.of_bytes Script.expr_encoding bytes with
 *           | None ->
 *               Lwt.return (Gas.consume ctxt (Interp_costs.unpack_failed bytes)) >>=? fun ctxt ->
 *               logged_return (Item (None, rest), ctxt)
 *           | Some expr ->
 *               Lwt.return (Gas.consume ctxt (Script.deserialized_cost expr)) >>=? fun ctxt ->
 *               parse_data ctxt t (Micheline.root expr) >>= function
 *               | Ok (value, ctxt) ->
 *                   logged_return (Item (Some value, rest), ctxt)
 *               | Error _ignored ->
 *                   Lwt.return (Gas.consume ctxt (Interp_costs.unpack_failed bytes)) >>=? fun ctxt ->
 *                   logged_return (Item (None, rest), ctxt)
 *         else
 *           logged_return (Item (None, rest), ctxt)
 *     (\* protocol *\)
 *     | Address, Item ((_, contract), rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.address) >>=? fun ctxt ->
 *         logged_return (Item (contract, rest), ctxt)
 *     | Contract t, Item (contract, rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.contract) >>=? fun ctxt ->
 *         Script_ir_translator.parse_contract_for_script ctxt loc t contract >>=? fun (ctxt, maybe_contract) ->
 *         logged_return (Item (maybe_contract, rest), ctxt)
 *     | Transfer_tokens,
 *       Item (p, Item (amount, Item ((tp, destination), rest))) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.transfer) >>=? fun ctxt ->
 *         unparse_data ctxt Optimized tp p >>=? fun (p, ctxt) ->
 *         let operation =
 *           Transaction
 *             { amount ; destination ;
 *               parameters = Some (Script.lazy_expr (Micheline.strip_locations p)) } in
 *         Lwt.return (fresh_internal_nonce ctxt) >>=? fun (ctxt, nonce) ->
 *         logged_return (Item (Internal_operation { source = self ; operation ; nonce }, rest), ctxt)
 *     | Create_account,
 *       Item (manager, Item (delegate, Item (delegatable, Item (credit, rest)))) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.create_account) >>=? fun ctxt ->
 *         Contract.fresh_contract_from_current_nonce ctxt >>=? fun (ctxt, contract) ->
 *         let operation =
 *           Origination
 *             { credit ; manager ; delegate ; preorigination = Some contract ;
 *               delegatable ; script = None ; spendable = true } in
 *         Lwt.return (fresh_internal_nonce ctxt) >>=? fun (ctxt, nonce) ->
 *         logged_return (Item (Internal_operation { source = self ; operation ; nonce },
 *                              Item (contract, rest)), ctxt)
 *     | Implicit_account, Item (key, rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.implicit_account) >>=? fun ctxt ->
 *         let contract = Contract.implicit_contract key in
 *         logged_return (Item ((Unit_t None, contract), rest), ctxt)
 *     | Create_contract (storage_type, param_type, Lam (_, code)),
 *       Item (manager, Item
 *               (delegate, Item
 *                  (spendable, Item
 *                     (delegatable, Item
 *                        (credit, Item
 *                           (init, rest)))))) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.create_contract) >>=? fun ctxt ->
 *         unparse_ty ctxt param_type >>=? fun (unparsed_param_type, ctxt) ->
 *         unparse_ty ctxt storage_type >>=? fun (unparsed_storage_type, ctxt) ->
 *         let code =
 *           Micheline.strip_locations
 *             (Seq (0, [ Prim (0, K_parameter, [ unparsed_param_type ], []) ;
 *                        Prim (0, K_storage, [ unparsed_storage_type ], []) ;
 *                        Prim (0, K_code, [ Micheline.root code ], []) ])) in
 *         unparse_data ctxt Optimized storage_type init >>=? fun (storage, ctxt) ->
 *         let storage = Micheline.strip_locations storage in
 *         Contract.fresh_contract_from_current_nonce ctxt >>=? fun (ctxt, contract) ->
 *         let operation =
 *           Origination
 *             { credit ; manager ; delegate ; preorigination = Some contract ;
 *               delegatable ; spendable ;
 *               script = Some { code = Script.lazy_expr code ;
 *                               storage = Script.lazy_expr storage } } in
 *         Lwt.return (fresh_internal_nonce ctxt) >>=? fun (ctxt, nonce) ->
 *         logged_return
 *           (Item (Internal_operation { source = self ; operation ; nonce },
 *                  Item (contract, rest)), ctxt)
 *     | Set_delegate,
 *       Item (delegate, rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.create_account) >>=? fun ctxt ->
 *         let operation = Delegation delegate in
 *         Lwt.return (fresh_internal_nonce ctxt) >>=? fun (ctxt, nonce) ->
 *         logged_return (Item (Internal_operation { source = self ; operation ; nonce }, rest), ctxt)
 *     | Balance, rest ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.balance) >>=? fun ctxt ->
 *         Contract.get_balance ctxt self >>=? fun balance ->
 *         logged_return (Item (balance, rest), ctxt)
 *     | Now, rest ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.now) >>=? fun ctxt ->
 *         let now = Script_timestamp.now ctxt in
 *         logged_return (Item (now, rest), ctxt)
 *     | Check_signature, Item (key, Item (signature, Item (message, rest))) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.check_signature) >>=? fun ctxt ->
 *         let res = Signature.check key signature message in
 *         logged_return (Item (res, rest), ctxt)
 *     | Hash_key, Item (key, rest) ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.hash_key) >>=? fun ctxt ->
 *         logged_return (Item (Signature.Public_key.hash key, rest), ctxt)
 *     | Blake2b, Item (bytes, rest) ->
 *         Lwt.return (Gas.consume ctxt (Interp_costs.hash bytes 32)) >>=? fun ctxt ->
 *         let hash = Raw_hashes.blake2b bytes in
 *         logged_return (Item (hash, rest), ctxt)
 *     | Sha256, Item (bytes, rest) ->
 *         Lwt.return (Gas.consume ctxt (Interp_costs.hash bytes 32)) >>=? fun ctxt ->
 *         let hash = Raw_hashes.sha256 bytes in
 *         logged_return (Item (hash, rest), ctxt)
 *     | Sha512, Item (bytes, rest) ->
 *         Lwt.return (Gas.consume ctxt (Interp_costs.hash bytes 64)) >>=? fun ctxt ->
 *         let hash = Raw_hashes.sha512 bytes in
 *         logged_return (Item (hash, rest), ctxt)
 *     | Steps_to_quota, rest ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.steps_to_quota) >>=? fun ctxt ->
 *         let steps = match Gas.level ctxt with
 *           | Limited { remaining } -> remaining
 *           | Unaccounted -> Z.of_string "99999999" in
 *         logged_return (Item (Script_int.(abs (of_zint steps)), rest), ctxt)
 *     | Source, rest ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.source) >>=? fun ctxt ->
 *         logged_return (Item (payer, rest), ctxt)
 *     | Sender, rest ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.source) >>=? fun ctxt ->
 *         logged_return (Item (source, rest), ctxt)
 *     | Self t, rest ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.self) >>=? fun ctxt ->
 *         logged_return (Item ((t,self), rest), ctxt)
 *     | Amount, rest ->
 *         Lwt.return (Gas.consume ctxt Interp_costs.amount) >>=? fun ctxt ->
 *         logged_return (Item (amount, rest), ctxt) *)

(* and interp
 *   : type p r.
 *     (?log: execution_trace ref ->
 *      context ->
 *      source: Contract.t -> payer:Contract.t -> self: Contract.t -> Tez.t ->
 *      (p, r) lambda -> p ->
 *      (r * context) tzresult Lwt.t)
 *   = fun ?log ctxt ~source ~payer ~self amount (Lam (code, _)) arg ->
 *     let stack = (Item (arg, Empty)) in
 *     begin match log with
 *       | None -> return_unit
 *       | Some log ->
 *           trace Cannot_serialize_log
 *             (unparse_stack ctxt (stack, code.bef)) >>=? fun stack ->
 *           log := (code.loc, Gas.level ctxt, stack) :: !log ;
 *           return_unit
 *     end >>=? fun () ->
 *     step ctxt ~source ~payer ~self amount code stack >>=? fun (Item (ret, Empty), ctxt) ->
 *     return (ret, ctxt) *)



end

open X_error_monad

let stack_ty_eq (type a b)
    ?(tezos_context = dummy_environment.tezos_context)
    (a:a stack_ty) (b:b stack_ty) =
  alpha_wrap (X.stack_ty_eq tezos_context 0 a b) >>? fun (Eq, _) ->
  ok Eq

let ty_eq (type a b)
    ?(tezos_context = dummy_environment.tezos_context)
    (a:a ty) (b:b ty)
  =
  alpha_wrap (Script_ir_translator.ty_eq tezos_context a b) >>? fun (Eq, _) ->
  ok Eq

let parse_michelson (type aft)
    ?(tezos_context = dummy_environment.tezos_context)
    ?(top_level = Lambda) (michelson:Michelson.t)
    ?type_logger
    (bef:'a Script_typed_ir.stack_ty) (aft:aft Script_typed_ir.stack_ty)
  =
  parse_instr
    ?type_logger
    top_level tezos_context
    michelson bef ~legacy:false >>=?? fun (j, _) ->
  match j with
  | Typed descr -> (
      Lwt.return (
        alpha_wrap (X.stack_ty_eq tezos_context 0 descr.aft aft) >>? fun (Eq, _) ->
        let descr : (_, aft) Script_typed_ir.descr = {descr with aft} in
        Ok descr
      )
    )
  | _ -> Lwt.return @@ error_exn (Failure "Typing instr failed")

let parse_michelson_fail (type aft)
    ?(tezos_context = dummy_environment.tezos_context)
    ?(top_level = Lambda) (michelson:Michelson.t)
    ?type_logger
    (bef:'a Script_typed_ir.stack_ty) (aft:aft Script_typed_ir.stack_ty)
  =
  parse_instr
    ?type_logger
    top_level tezos_context
    michelson bef ~legacy:false >>=?? fun (j, _) ->
  match j with
  | Typed descr -> (
      Lwt.return (
        alpha_wrap (X.stack_ty_eq tezos_context 0 descr.aft aft) >>? fun (Eq, _) ->
        let descr : (_, aft) Script_typed_ir.descr = {descr with aft} in
        Ok descr
      )
    )
  | Failed { descr } ->
      Lwt.return (Ok (descr aft))

let parse_michelson_data
    ?(tezos_context = dummy_environment.tezos_context)
    michelson ty =
  parse_data tezos_context ty michelson ~legacy:false >>=?? fun (data, _) ->
  return data

let parse_michelson_ty
    ?(tezos_context = dummy_environment.tezos_context)
    ?(allow_big_map = true) ?(allow_operation = true) ?(allow_contract = true)
    michelson =
  Lwt.return @@ parse_ty tezos_context ~allow_big_map ~allow_operation michelson ~legacy:false ~allow_contract >>=?? fun (ty, _) ->
  return ty

let unparse_michelson_data
    ?(tezos_context = dummy_environment.tezos_context)
    ?mapper ty value : Michelson.t tzresult Lwt.t =
  X.unparse_data_generic tezos_context ?mapper
    Readable ty value >>=?? fun (michelson, _) ->
  return michelson

let unparse_michelson_ty
    ?(tezos_context = dummy_environment.tezos_context)
    ty : Michelson.t tzresult Lwt.t =
  Script_ir_translator.unparse_ty tezos_context ty >>=?? fun (michelson, _) ->
  return michelson

type options = {
  tezos_context: Alpha_context.t ;
  source: Alpha_context.Contract.t ;
  payer: Alpha_context.Contract.t ;
  self: Alpha_context.Contract.t ;
  amount: Alpha_context.Tez.t ;
  chain_id: Environment.Chain_id.t ;
}

let make_options
    ?(tezos_context = dummy_environment.tezos_context)
    ?(predecessor_timestamp = dummy_environment.tezos_context.predecessor_timestamp)
    ?(source = (List.nth dummy_environment.identities 0).implicit_contract)
    ?(self = (List.nth dummy_environment.identities 0).implicit_contract)
    ?(payer = (List.nth dummy_environment.identities 1).implicit_contract)
    ?(amount = Alpha_context.Tez.one)
    ?(chain_id = Environment.Chain_id.zero)
    ()
  =
  let tezos_context = { tezos_context with predecessor_timestamp } in
  {
    tezos_context ;
    source ;
    self ;
    payer ;
    amount ;
    chain_id ;
  }

let default_options = make_options ()

let interpret ?(options = default_options) (instr:('a, 'b) descr) (bef:'a stack) : 'b stack tzresult Lwt.t  =
  let {
    tezos_context ;
    source ;
    self ;
    payer ;
    amount ;
    chain_id ;
  } = options in
  let step_constants = { source ; self ; payer ; amount ; chain_id } in
  Script_interpreter.step tezos_context step_constants instr bef >>=??
  fun (stack, _) -> return stack

let unparse_ty_michelson ty =
  Script_ir_translator.unparse_ty dummy_environment.tezos_context ty >>=??
  fun (n,_) -> return n

type typecheck_res =
  | Type_checked
  | Err_parameter | Err_storage | Err_contract
  | Err_unknown

let typecheck_contract contract =
  let contract' = Tezos_micheline.Micheline.strip_locations contract in
  Script_ir_translator.typecheck_code dummy_environment.tezos_context contract' >>= fun x ->
  match x with
  | Ok _res -> return Type_checked
  | Error (Script_tc_errors.Ill_formed_type (Some "parameter", _code, _)::_) -> return Err_parameter
  | Error (Script_tc_errors.Ill_formed_type (Some "storage", _code, _)::_) -> return Err_storage
  | Error (Script_tc_errors.Ill_typed_contract (_code, _)::_) -> return @@ Err_contract
  | Error _ -> return Err_unknown

let assert_equal_michelson_type ty1 ty2 =
  (* alpha_wrap (Script_ir_translator.ty_eq tezos_context a b) >>? fun (Eq, _) -> *)
  alpha_wrap (Script_ir_translator.ty_eq dummy_environment.tezos_context ty1 ty2)

type 'a interpret_res =
  | Succeed of 'a stack
  | Fail of Script_repr.expr

let failure_interpret
    ?(options = default_options)
    (instr:('a, 'b) descr)
    (bef:'a stack) : 'b interpret_res tzresult Lwt.t =
  let {
    tezos_context ;
    source ;
    self ;
    payer ;
    amount ;
    chain_id ;
  } = options in
  let step_constants = { source ; self ; payer ; amount ; chain_id } in
  Script_interpreter.step tezos_context step_constants instr bef >>= fun x ->
  match x with
  | Ok (s , _ctxt) -> return @@ Succeed s
  | Error ((Reject (_, expr, _))::_t) -> return @@ Fail expr (* This catches failwith errors *)
  | Error errs -> Lwt.return @@ Error (List.map (alpha_error_wrap) errs)

let pack (data_ty: 'a ty) (data: 'a) : bytes tzresult Lwt.t =
  pack_data dummy_environment.tezos_context data_ty data >>=?? fun (packed,_) -> return packed

let strings_of_prims = Michelson_v1_primitives.strings_of_prims