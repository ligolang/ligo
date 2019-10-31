(* This file attempts to optimize Michelson code. The goal is to
   reduce the code size (the size of the binary Micheline.)

   I have ignored the 'execution gas' completely, because it seems
   that users will encounter code size problems earlier and more
   often.
*)

open Tezos_micheline.Micheline
open Tezos_utils.Michelson

(* `arity p` should be `Some n` only if p is (always) an instruction
   which removes n items from the stack and uses them to push 1 item,
   without effects other than gas consumption. It must never fail. *)

let arity : prim -> int option = function
 (* stack things *)
 | I_DIP -> None
 | I_DROP -> None
 | I_DUP -> None
 | I_SWAP -> None
 | I_DIG -> None
 | I_DUG -> None
 (* control *)
 | I_FAILWITH -> None
 | I_EXEC -> None
 | I_IF -> None
 | I_IF_CONS -> None
 | I_IF_LEFT -> None
 | I_IF_NONE -> None
 | I_LOOP -> None
 | I_MAP -> None
 | I_ITER -> None
 | I_LOOP_LEFT -> None
 (* internal ops *)
 | I_CREATE_ACCOUNT -> None
 | I_CREATE_CONTRACT -> None
 | I_TRANSFER_TOKENS -> None
 | I_SET_DELEGATE -> None
 (* tez arithmetic (can fail) *)
 | I_ADD -> None
 | I_MUL -> None
 | I_SUB -> None (* can fail for tez *)
 (* etc *)
 | I_CONCAT -> None (* sometimes 1, sometimes 2 :( *)
 | I_CAST -> None
 | I_RENAME -> None
 (* stuff *)
 | I_PACK -> Some 1
 | I_UNPACK -> Some 1
 | I_BLAKE2B -> Some 1
 | I_SHA256 -> Some 1
 | I_SHA512 -> Some 1
 | I_ABS -> Some 1
 | I_AMOUNT -> Some 0
 | I_AND -> Some 2
 | I_BALANCE -> Some 0
 | I_CAR -> Some 1
 | I_CDR -> Some 1
 | I_CHECK_SIGNATURE -> Some 3
 | I_COMPARE -> Some 2
 | I_CONS -> Some 2
 | I_IMPLICIT_ACCOUNT -> Some 1
 | I_EDIV -> Some 2
 | I_EMPTY_MAP -> Some 0
 | I_EMPTY_SET -> Some 0
 | I_EQ -> Some 1
 | I_GE -> Some 1
 | I_GET -> Some 2
 | I_GT -> Some 1
 | I_HASH_KEY -> Some 1
 | I_INT -> Some 1
 | I_LAMBDA -> Some 0
 | I_LE -> Some 1
 | I_LEFT -> Some 1
 | I_LSL -> Some 1
 | I_LSR -> Some 1
 | I_LT -> Some 1
 | I_MEM -> Some 2
 | I_NEG -> Some 1
 | I_NEQ -> Some 1
 | I_NIL -> Some 0
 | I_NONE -> Some 0
 | I_NOT -> Some 1
 | I_NOW -> Some 0
 | I_OR -> Some 2
 | I_PAIR -> Some 2
 | I_PUSH -> Some 0
 | I_RIGHT -> Some 1
 | I_SIZE -> Some 1
 | I_SOME -> Some 1
 | I_SOURCE -> Some 0
 | I_SENDER -> Some 0
 | I_SELF -> Some 0
 | I_SLICE -> Some 3
 | I_STEPS_TO_QUOTA -> Some 0
 | I_UNIT -> Some 0
 | I_UPDATE -> Some 3
 | I_XOR -> Some 2
 | I_ADDRESS -> Some 1
 | I_CONTRACT -> Some 1
 | I_ISNAT -> Some 1
 | I_CHAIN_ID -> Some 0
 | I_EMPTY_BIG_MAP -> Some 0
 | I_APPLY -> Some 2

 (* not instructions *)
 | K_parameter
 | K_storage
 | K_code
 | D_False
 | D_Elt
 | D_Left
 | D_None
 | D_Pair
 | D_Right
 | D_Some
 | D_True
 | D_Unit
 | T_bool
 | T_contract
 | T_int
 | T_key
 | T_key_hash
 | T_lambda
 | T_list
 | T_map
 | T_big_map
 | T_nat
 | T_option
 | T_or
 | T_pair
 | T_set
 | T_signature
 | T_string
 | T_bytes
 | T_mutez
 | T_timestamp
 | T_unit
 | T_operation
 | T_address
 | T_chain_id
   -> None

let is_nullary_op (p : prim) : bool =
  match arity p with
  | Some 0 -> true
  | _ -> false

let is_unary_op (p : prim) : bool =
  match arity p with
  | Some 1 -> true
  | _ -> false

let is_binary_op (p : prim) : bool =
  match arity p with
  | Some 2 -> true
  | _ -> false

let is_ternary_op (p : prim) : bool =
  match arity p with
  | Some 3 -> true
  | _ -> false

let unseq : michelson -> michelson list = function
  | Seq (_, args) -> args
  | x -> [x]

(* Replace `PUSH (lambda a b) {}` with `LAMBDA a b {}` *)
let rec use_lambda_instr : michelson -> michelson =
  fun x ->
  match x with
  | Seq (l, args) ->
    Seq (l, List.map use_lambda_instr args)
  | Prim (_, I_PUSH, [Prim (_, T_lambda, [arg; ret], _); code], _) ->
    i_lambda arg ret code
  | Prim (_, I_PUSH, _, _) ->
    x (* possibly missing some nested lambdas *)
  | Prim (l, p, args, annot) ->
    Prim (l, p, List.map use_lambda_instr args, annot)
  | _ -> x

(* This flattens nested seqs. {} is erased, { { code1 } ; { code2 } }
   becomes { code1 ; code2 }, etc. This is important because each seq
   costs 5 bytes, for the "Seq" tag and a 4 byte length. *)
let rec flatten_seqs : michelson -> michelson =
  fun x ->
  match x with
  | Seq (l, args) ->
     let args = List.concat @@ List.map (fun x -> unseq (flatten_seqs x)) args in
     Seq (l, args)
  (* Should not flatten literal seq data in PUSH. Ugh... *)
  | Prim (_, I_PUSH, _, _) -> x
  | Prim (l, p, args, annot) -> Prim (l, p, List.map flatten_seqs args, annot)
  | _ -> x

type peep1 = michelson -> michelson list option
type peep2 = michelson * michelson -> michelson list option
type peep3 = michelson * michelson * michelson -> michelson list option
type peep4 = michelson * michelson * michelson * michelson -> michelson list option

let rec peep1 (f : peep1) : michelson list -> bool * michelson list = function
  | [] -> (false, [])
  | x1 :: xs ->
     match f x1 with
     | Some xs' -> let (_, xs') = peep1 f (xs' @ xs) in
                   (true, xs')
     | None -> let (changed, xs) = peep1 f xs in
               (changed, x1 :: xs)

let rec peep2 (f : peep2) : michelson list -> bool * michelson list = function
  | [] -> (false, [])
  | [x] -> (false, [x])
  | x1 :: x2 :: xs ->
     match f (x1, x2) with
     | Some xs' -> let (_, xs') = peep2 f (xs' @ xs) in
                   (true, xs')
     | None -> let (changed, xs') = peep2 f (x2 :: xs) in
               (changed, x1 :: xs')

let rec peep3 (f : peep3) : michelson list -> bool * michelson list = function
  | [] -> (false, [])
  | [x] -> (false, [x])
  | [x ; y] -> (false, [x ; y])
  | x1 :: x2 :: x3 :: xs ->
     match f (x1, x2, x3) with
     | Some xs' -> let (_, xs') = peep3 f (xs' @ xs) in
                   (true, xs')
     | None -> let (changed, xs') = peep3 f (x2 :: x3 :: xs) in
               (changed, x1 :: xs')

let rec peep4 (f : peep4) : michelson list -> bool * michelson list = function
  | [] -> (false, [])
  | [x] -> (false, [x])
  | [x ; y] -> (false, [x ; y])
  | [x ; y ; z] -> (false, [x ; y ; z])
  | x1 :: x2 :: x3 :: x4 :: xs ->
     match f (x1, x2, x3, x4) with
     | Some xs' -> let (_, xs') = peep4 f (xs' @ xs) in
                   (true, xs')
     | None -> let (changed, xs') = peep4 f (x2 :: x3 :: x4 :: xs) in
               (changed, x1 :: xs')

(* apply f to all seqs *)
let rec peephole (f : michelson list -> bool * michelson list) : michelson -> bool * michelson =
  let peep_args ~seq args =
    let (changed, args) = if seq
                          then f args
                          else (false, args) in
    List.fold_map_acc
      (fun changed1 arg ->
        let (changed2, arg) = peephole f arg in
        (changed1 || changed2, arg))
      changed
      args in
  function
  | Seq (l, args) -> let (changed, args) = peep_args ~seq:true args in
                     (changed, Seq (l, args))
  | Prim (l, p, args, annot) -> let (changed, args) = peep_args ~seq:false args in
                                (changed, Prim (l, p, args, annot))
  | x -> (false, x)

(* apply the optimizers in order *)
let rec sequence_optimizers (fs : (michelson -> bool * michelson) list) : michelson -> bool * michelson =
  match fs with
  | [] -> fun x -> (false, x)
  | f :: fs -> fun x -> let (changed1, x) = f x in
                        let (changed2, x) = sequence_optimizers fs x in
                        (changed1 || changed2, x)

(* take the fixed point of an optimizer (!) *)
let rec iterate_optimizer (f : michelson -> bool * michelson) : michelson -> michelson =
  fun x ->
  let (changed, x) = f x in
  if changed
  then iterate_optimizer f x
  else x

let opt_drop2 : peep2 = function
  (* nullary_op ; DROP  ↦  *)
  | Prim (_, p, _, _), Prim (_, I_DROP, [], _) when is_nullary_op p -> Some []
  (* DUP ; DROP  ↦  *)
  | Prim (_, I_DUP, _, _), Prim (_, I_DROP, [], _) -> Some []
  (* unary_op ; DROP  ↦  DROP *)
  | Prim (_, p, _, _), Prim (_, I_DROP, [], _) when is_unary_op p -> Some [i_drop]
  (* binary_op ; DROP  ↦  DROP ; DROP *)
  | Prim (_, p, _, _), Prim (_, I_DROP, [], _) when is_binary_op p -> Some [i_drop; i_drop]
  (* ternary_op ; DROP  ↦  DROP ; DROP ; DROP *)
  | Prim (_, p, _, _), Prim (_, I_DROP, [], _) when is_ternary_op p -> Some [i_drop; i_drop; i_drop]
  | _ -> None

let opt_drop4 : peep4 = function
  (* DUP; unary_op; SWAP; DROP  ↦  unary_op *)
  | Prim (_, I_DUP, _, _),
    (Prim (_, p, _, _) as unary_op),
    Prim (_, I_SWAP, _, _),
    Prim (_, I_DROP, [], _)
    when is_unary_op p ->
    Some [unary_op]
  | _ -> None

let opt_dip1 : peep1 = function
  (* DIP {}  ↦  *)
  | Prim (_, I_DIP, [Seq (_, [])], _) -> Some []
  (* DIP { nullary_op }  ↦  nullary_op ; SWAP *)
  | Prim (_, I_DIP, [Seq (_, [(Prim (_, p, _, _) as push)])], _) when is_nullary_op p ->
     Some [push ; i_swap]
  (* DIP { unary_op }  ↦  SWAP ; unary_op ; SWAP *)
  | Prim (_, I_DIP, [Seq (_, [(Prim (_, p, _, _) as unary_op)])], _) when is_unary_op p ->
     Some [i_swap ; unary_op ; i_swap]
  | _ -> None

let opt_dip2 : peep2 = function
  (* combine adjacent dips, shaving a seq and enabling further
     optimization inside the DIP: *)
  (* DIP { code1 } ; DIP { code2 }  ↦  DIP { code1 ; code2 } *)
  | Prim (_, I_DIP, [Seq (_, code1)], _), Prim (_, I_DIP, [Seq (_, code2)], _) ->
     Some [Prim (0, I_DIP, [Seq (0, code1 @ code2)], [])]
  (* DIP { code } ; DROP  ↦  DROP ; code *)
  | Prim (_, I_DIP, [Seq (_, code)], _), (Prim (_, I_DROP, [], _) as drop) ->
     Some (drop :: code)
  (* nullary_op ; DIP { code }  ↦  code ; nullary_op *)
  | (Prim (_, p, _, _) as nullary_op), Prim (_, I_DIP, [Seq (_, code)], _) when is_nullary_op p ->
     Some (code @ [nullary_op])
  (* DIP { code } ; unary_op  ↦  unary_op ; DIP { code } *)
  | (Prim (_, I_DIP, [Seq _], _) as dip), (Prim (_, p, _, _) as unary_op) when is_unary_op p ->
     Some [unary_op; dip]
  (* unary_op ; DIP { code }  ↦  DIP { code } ; unary_op *)
  (* | (Prim (_, p, _, _) as unary_op), (Prim (_, I_DIP, [Seq _], _) as dip) when is_unary_op p ->
   *    Some [dip; unary_op] *)
  | _ -> None

let opt_dip3 : peep3 = function
  (* replace UNPAIR/UNPIAR with a smaller version *)
  (* TODO probably better to implement optimal UNPAIR in the compiler *)
  (* DUP ; CAR ; DIP { CDR }  ↦  DUP ; CDR ; SWAP ; CAR *)
  | Prim (_, I_DUP, _, _),
    (Prim (_, (I_CAR | I_CDR), _, _) as proj1),
    Prim (_, I_DIP, [Seq (_, [(Prim (_, (I_CAR | I_CDR), _, _) as proj2)])], _) ->
     Some [ i_dup ; proj2 ; i_swap ; proj1 ]
  | _ -> None

let opt_swap2 : peep2 = function
  (* SWAP ; SWAP  ↦  *)
  | Prim (_, I_SWAP, _, _), Prim (_, I_SWAP, _, _) ->
     Some []
  (* DUP ; SWAP  ↦  DUP *)
  | Prim (_, I_DUP, _, _), Prim (_, I_SWAP, _, _) ->
     Some [i_dup]
  (* SWAP ; ADD  ↦  ADD *)
  (* etc *)
  | Prim (_, I_SWAP, _, _), (Prim (_, (I_ADD | I_OR | I_AND | I_XOR), _, _) as comm_op) ->
     Some [comm_op]
  | _ -> None

(* This "optimization" deletes dead code produced by the compiler
   after a FAILWITH, which is illegal in Michelson. This means we are
   thwarting the intent of the Michelson tail fail restriction -- the
   LIGO _user_ might accidentally write dead code immediately after a
   failure, and we will simply erase it. *)
let rec opt_tail_fail : michelson -> michelson =
  function
  | Seq (l, args) ->
     let rec aux args =
       match args with
       | [] -> []
       | Prim (l, I_FAILWITH, args, annot) :: _ -> [ Prim (l, I_FAILWITH, args, annot) ]
       | arg :: args -> arg :: aux args in
     let args = aux args in
     Seq (l, List.map opt_tail_fail args)
  | Prim (l, p, args, annot) ->
     Prim (l, p, List.map opt_tail_fail args, annot)
  | x -> x

let rec opt_combine_drops (x : michelson) : michelson =
  let rec combine : michelson list -> michelson list = function
    | [] -> []
    | Prim (_, I_DROP, [], []) :: xs ->
      let xs' = combine xs in
      begin match xs' with
      | [] -> [Prim (-1, I_DROP, [], [])]
      | Prim (_, I_DROP, [], []) :: xs' -> Prim (-1, I_DROP, [Int (-1, Z.of_int 2)], []) :: xs'
      | Prim (_, I_DROP, [Int (_, n)], []) :: xs' -> Prim (-1, I_DROP, [Int (-1, Z.of_int (1 + Z.to_int n))], []) :: xs'
      | x' :: xs' -> Prim (-1, I_DROP, [], []) :: x' :: xs'
      end
    | x :: xs -> x :: combine xs in
  match x with
  | Seq (l, args) -> Seq (l, combine (List.map opt_combine_drops args))
  | Prim (l, p, args, annot) ->
    Prim (l, p, List.map opt_combine_drops args, annot)
  | x -> x

let optimize : michelson -> michelson =
  fun x ->
  let x = use_lambda_instr x in
  let x = flatten_seqs x in
  let x = opt_tail_fail x in
  let optimizers = [ peephole @@ peep2 opt_drop2 ;
                     peephole @@ peep4 opt_drop4 ;
                     peephole @@ peep3 opt_dip3 ;
                     peephole @@ peep2 opt_dip2 ;
                     peephole @@ peep1 opt_dip1 ;
                     peephole @@ peep2 opt_swap2 ;
                   ] in
  let x = iterate_optimizer (sequence_optimizers optimizers) x in
  let x = opt_combine_drops x in
  x
