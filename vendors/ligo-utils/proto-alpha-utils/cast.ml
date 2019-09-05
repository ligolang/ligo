module Error_monad = X_error_monad
open Tezos_micheline

let env = Error_monad.force_lwt ~msg:"Cast:init environment" @@ Init_proto_alpha.init_environment ()

open Memory_proto_alpha
open Protocol
open Alpha_context

exception Expr_from_string
let expr_of_string str =
  let (ast, errs) = Michelson_parser.V1.parse_expression ~check:false str in
  (match errs with
   | [] -> ()
   | lst -> (
       Format.printf "expr_from_string: %a\n" Error_monad.pp_print_error lst;
       raise Expr_from_string
     ));
  ast.expanded

let tl_of_string str =
  let (ast, errs) = Michelson_parser.V1.parse_toplevel ~check:false str in
  (match errs with
   | [] -> ()
   | lst -> (
       Format.printf "expr_from_string: %a\n" Error_monad.pp_print_error lst;
       raise Expr_from_string
     ));
  ast.expanded

let lexpr_of_string str =
  Script.lazy_expr @@ expr_of_string str

let ltl_of_string str =
  Script.lazy_expr @@ tl_of_string str

let node_of_string str =
  Micheline.root @@ expr_of_string str

let node_to_string (node:_ Micheline.node) =
  let stripped = Micheline.strip_locations node in
  let print_node = Micheline_printer.printable Michelson_v1_primitives.string_of_prim stripped in
  Micheline_printer.print_expr Format.str_formatter print_node ;
  Format.flush_str_formatter ()

open Script_ir_translator

type ex_typed_value =
  Ex_typed_value : ('a Script_typed_ir.ty * 'a) -> ex_typed_value

include struct
  open Script_typed_ir
  open Protocol.Environment.Error_monad
  module Unparse_costs = Michelson_v1_gas.Cost_of.Unparse
  open Micheline
  open Michelson_v1_primitives
  open Protocol.Environment

  let rec unparse_data_generic
          : type a. context -> ?mapper:(ex_typed_value -> Script.node option tzresult Lwt.t) ->
                 unparsing_mode -> a ty -> a -> (Script.node * context) tzresult Lwt.t
    = fun ctxt ?(mapper = fun _ -> return None) mode ty a ->
    Lwt.return (Gas.consume ctxt Unparse_costs.cycle) >>=? fun ctxt ->
    mapper (Ex_typed_value (ty, a)) >>=? function
    | Some x -> return (x, ctxt)
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
      | Address_t _, c  ->
         Lwt.return (Gas.consume ctxt Unparse_costs.contract) >>=? fun ctxt ->
         begin
           match mode with
           | Optimized ->
              let bytes = Data_encoding.Binary.to_bytes_exn Contract.encoding c in
              return (Bytes (-1, bytes), ctxt)
           | Readable -> return (String (-1, Contract.to_b58check c), ctxt)
         end
      | Contract_t _, (_, c)  ->
         Lwt.return (Gas.consume ctxt Unparse_costs.contract) >>=? fun ctxt ->
         begin
           match mode with
           | Optimized ->
              let bytes = Data_encoding.Binary.to_bytes_exn Contract.encoding c in
              return (Bytes (-1, bytes), ctxt)
           | Readable -> return (String (-1, Contract.to_b58check c), ctxt)
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
      | Operation_t _, op ->
         let bytes = Data_encoding.Binary.to_bytes_exn Alpha_context.Operation.internal_operation_encoding op in
         Lwt.return (Gas.consume ctxt (Unparse_costs.operation bytes)) >>=? fun ctxt ->
         return (Bytes (-1, bytes), ctxt)
      | Pair_t ((tl, _, _), (tr, _, _), _), (l, r) ->
         Lwt.return (Gas.consume ctxt Unparse_costs.pair) >>=? fun ctxt ->
         unparse_data_generic ~mapper ctxt mode tl l >>=? fun (l, ctxt) ->
         unparse_data_generic ~mapper ctxt mode tr r >>=? fun (r, ctxt) ->
         return (Prim (-1, D_Pair, [ l; r ], []), ctxt)
      | Union_t ((tl, _), _, _), L l ->
         Lwt.return (Gas.consume ctxt Unparse_costs.union) >>=? fun ctxt ->
         unparse_data_generic ~mapper ctxt mode tl l >>=? fun (l, ctxt) ->
         return (Prim (-1, D_Left, [ l ], []), ctxt)
      | Union_t (_, (tr, _), _), R r ->
         Lwt.return (Gas.consume ctxt Unparse_costs.union) >>=? fun ctxt ->
         unparse_data_generic ~mapper ctxt mode tr r >>=? fun (r, ctxt) ->
         return (Prim (-1, D_Right, [ r ], []), ctxt)
      | Option_t ((t, _), _, _), Some v ->
         Lwt.return (Gas.consume ctxt Unparse_costs.some) >>=? fun ctxt ->
         unparse_data_generic ~mapper ctxt mode t v >>=? fun (v, ctxt) ->
         return (Prim (-1, D_Some, [ v ], []), ctxt)
      | Option_t _, None ->
         Lwt.return (Gas.consume ctxt Unparse_costs.none) >>=? fun ctxt ->
         return (Prim (-1, D_None, [], []), ctxt)
      | List_t (t, _), items ->
         fold_left_s
           (fun (l, ctxt) element ->
             Lwt.return (Gas.consume ctxt Unparse_costs.list_element) >>=? fun ctxt ->
             unparse_data_generic ~mapper ctxt mode t element >>=? fun (unparsed, ctxt) ->
             return (unparsed :: l, ctxt))
           ([], ctxt)
           items >>=? fun (items, ctxt) ->
         return (Micheline.Seq (-1, List.rev items), ctxt)
      | Set_t (t, _), set ->
         let t = ty_of_comparable_ty t in
         fold_left_s
           (fun (l, ctxt) item ->
             Lwt.return (Gas.consume ctxt Unparse_costs.set_element) >>=? fun ctxt ->
             unparse_data_generic ~mapper ctxt mode t item >>=? fun (item, ctxt) ->
             return (item :: l, ctxt))
           ([], ctxt)
           (set_fold (fun e acc -> e :: acc) set []) >>=? fun (items, ctxt) ->
         return (Micheline.Seq (-1, items), ctxt)
      | Map_t (kt, vt, _), map ->
         let kt = ty_of_comparable_ty kt in
         fold_left_s
           (fun (l, ctxt) (k, v) ->
             Lwt.return (Gas.consume ctxt Unparse_costs.map_element) >>=? fun ctxt ->
             unparse_data_generic ~mapper ctxt mode kt k >>=? fun (key, ctxt) ->
             unparse_data_generic ~mapper ctxt mode vt v >>=? fun (value, ctxt) ->
             return (Prim (-1, D_Elt, [ key ; value ], []) :: l, ctxt))
           ([], ctxt)
           (map_fold (fun k v acc -> (k, v) :: acc) map []) >>=? fun (items, ctxt) ->
         return (Micheline.Seq (-1, items), ctxt)
      | Big_map_t (_kt, _kv, _), _map ->
         return (Micheline.Seq (-1, []), ctxt)
      | Lambda_t _, Lam (_, original_code) ->
         unparse_code_generic ~mapper ctxt mode (root original_code)
    )

  and unparse_code_generic ctxt ?mapper mode = function
    | Prim (loc, I_PUSH, [ ty ; data ], annot) ->
       Lwt.return (parse_ty ctxt ~allow_big_map:false ~allow_operation:false ty) >>=? fun (Ex_ty t, ctxt) ->
       parse_data ctxt t data >>=? fun (data, ctxt) ->
       unparse_data_generic ?mapper ctxt mode t data >>=? fun (data, ctxt) ->
       Lwt.return (Gas.consume ctxt (Unparse_costs.prim_cost 2 annot)) >>=? fun ctxt ->
       return (Prim (loc, I_PUSH, [ ty ; data ], annot), ctxt)
    | Seq (loc, items) ->
       fold_left_s
         (fun (l, ctxt) item ->
           unparse_code_generic ?mapper ctxt mode item >>=? fun (item, ctxt) ->
           return (item :: l, ctxt))
         ([], ctxt) items >>=? fun (items, ctxt) ->
       Lwt.return (Gas.consume ctxt (Unparse_costs.seq_cost (List.length items))) >>=? fun ctxt ->
       return (Micheline.Seq (loc, List.rev items), ctxt)
    | Prim (loc, prim, items, annot) ->
       fold_left_s
         (fun (l, ctxt) item ->
           unparse_code_generic ?mapper ctxt mode item >>=? fun (item, ctxt) ->
           return (item :: l, ctxt))
         ([], ctxt) items >>=? fun (items, ctxt) ->
       Lwt.return (Gas.consume ctxt (Unparse_costs.prim_cost 3 annot)) >>=? fun ctxt ->
       return (Prim (loc, prim, List.rev items, annot), ctxt)
    | Int _ | String _ | Bytes _ as atom -> return (atom, ctxt)


end

let rec mapper (Ex_typed_value (ty, a)) =
  let open Alpha_environment.Error_monad in
  let open Script_typed_ir in
  let open Micheline in
  match ty, a with
  | Big_map_t (kt, vt, Some (`Type_annot "toto")), map ->
    let kt = ty_of_comparable_ty kt in
    fold_left_s
      (fun l (k, v) ->
         match v with
         | None -> return l
         | Some v -> (
             let key = data_to_node (Ex_typed_value (kt, k)) in
             let value = data_to_node (Ex_typed_value (vt, v)) in
             return (Prim (-1, Michelson_v1_primitives.D_Elt, [ key ; value ], []) :: l))
      )
      []
      (map_fold (fun k v acc -> (k, v) :: acc) map.diff []) >>=? fun items ->
    return (Some (Micheline.Seq (-1, String (-1, "...") :: items)))
  | _ -> return None

and data_to_node (Ex_typed_value (ty, data)) =
  let tc = env.tezos_context in
  let node_lwt = unparse_data_generic tc ~mapper Readable ty data in
  let node = fst @@ Error_monad.force_lwt_alpha ~msg:"data to string" node_lwt in
  node

let data_to_string ty data =
  let node = data_to_node (Ex_typed_value (ty, data)) in
  node_to_string node

open Script_typed_ir
open Script_interpreter
type ex_typed_stack =
    Ex_typed_stack : ('a stack_ty * 'a stack) -> ex_typed_stack

let stack_to_string stack_ty stack =
  let rec aux acc fst (Ex_typed_stack(stack_ty,stack)) =
    match (stack_ty, stack) with
    | Item_t (hd_ty, tl_ty, _), Item (hd, tl) -> (
        let separator = if not fst then " ; " else "" in
        let str = data_to_string hd_ty hd in
        let acc = acc ^ separator ^ str in
        let new_value = aux acc false (Ex_typed_stack (tl_ty, tl)) in
        new_value
      )
    | _ -> acc in
  aux "" true @@ Ex_typed_stack(stack_ty, stack)

let ty_to_node ty =
  let (node, _) = Error_monad.force_lwt_alpha ~msg:"ty to node" @@ Script_ir_translator.unparse_ty env.tezos_context ty in
  node

type ex_descr =
    Ex_descr : (_, _) Script_typed_ir.descr -> ex_descr

let descr_to_node x =
  let open Alpha_context.Script in
  let open Micheline in
  let open! Script_typed_ir in
  let rec f : ex_descr -> Script.node = fun descr ->
    let prim ?children ?children_nodes p =
      match (children, children_nodes) with
      | Some children, None ->
        Prim (0, p, List.map f children, [])
      | Some _, Some _ ->
        raise @@ Failure "descr_to_node: too many parameters"
      | None, Some children_nodes ->
        Prim (0, p, children_nodes, [])
      | None, None ->
        Prim (0, p, [], [])
    in
    let (Ex_descr descr) = descr in
    match descr.instr with
    | Dup -> prim I_DUP
    | Drop -> prim I_DROP
    | Swap -> prim I_SWAP
    | Dip c -> prim ~children:[Ex_descr c] I_DIP
    | Car -> prim I_CAR
    | Cdr -> prim I_CDR
    | Cons_pair -> prim I_PAIR
    | Nop -> Micheline.Seq (0, [prim I_UNIT ; prim I_DROP])
    | Seq (a, b) -> Micheline.Seq (0, List.map f [Ex_descr a ; Ex_descr b])
    | Const v -> (
        let (Item_t (ty, _, _)) = descr.aft in
        prim  ~children_nodes:[data_to_node (Ex_typed_value (ty, v))] I_PUSH
      )
    | Failwith _ -> prim I_FAILWITH
    | If (a, b) -> prim ~children:[Ex_descr a ; Ex_descr b] I_IF
    | Loop c -> prim ~children:[Ex_descr c] I_LOOP
    | If_left (a, b) -> prim ~children:[Ex_descr a ; Ex_descr b] I_IF_LEFT
    | Left -> prim I_LEFT
    | Right -> prim I_RIGHT
    | Loop_left c -> prim ~children:[Ex_descr c] I_LOOP_LEFT
    | If_none (a, b) -> prim ~children:[Ex_descr a ; Ex_descr b] I_IF_NONE
    | Cons_none _ -> prim I_NONE
    | Cons_some -> prim I_SOME
    | Nil -> prim I_NIL
    | Cons_list -> prim I_CONS
    | If_cons (a, b) -> prim ~children:[Ex_descr a ; Ex_descr b] I_IF_CONS
    | List_iter _ -> prim I_ITER
    | Compare _ -> prim I_COMPARE
    | Int_nat -> prim I_INT
    | Add_natnat -> prim I_ADD
    | Add_natint -> prim I_ADD
    | Add_intnat -> prim I_ADD
    | Sub_int -> prim I_SUB
    | Mul_natnat -> prim I_MUL
    | Ediv_natnat -> prim I_MUL
    | Map_get -> prim I_GET
    | Map_update -> prim I_UPDATE
    | Big_map_get -> prim I_GET
    | Big_map_update -> prim I_UPDATE
    | Gt -> prim I_GT
    | Ge -> prim I_GE
    | Pack _ -> prim I_PACK
    | Unpack _ -> prim I_UNPACK
    | Blake2b -> prim I_BLAKE2B
    | And -> prim I_AND
    | Xor -> prim I_XOR
    | _ -> raise @@ Failure "descr to node" in
  f @@ Ex_descr x

let rec flatten_node =
  let open! Micheline in
  function
  | Micheline.Seq (a, lst) -> (
      let aux = function
        | Prim (loc, p, children, annot) -> [ Prim (loc, p, List.map flatten_node children, annot) ]
        | Seq (_, lst) -> List.map flatten_node lst
        | x -> [ x ] in
      let seqs = List.map aux @@ List.map flatten_node lst in
      Seq (a, List.concat seqs) )
  | x -> x

let descr_to_string descr =
  let node = descr_to_node descr in
  let node = flatten_node node in
  node_to_string node

let n_of_int n =
  match Script_int.is_nat @@ Script_int.of_int n with
  | None -> raise @@ Failure "n_of_int"
  | Some n -> n
