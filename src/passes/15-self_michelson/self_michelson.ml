(* This file attempts to optimize Michelson code. The goal is to
   reduce the code size (the size of the binary Micheline.)

   I have ignored the 'execution gas' completely, because it seems
   that users will encounter code size problems earlier and more
   often.
*)

open Tezos_micheline.Micheline
open Tezos_utils.Michelson
include Helpers

(* `arity p` should be `Some n` only if p is (always) an instruction
   which removes n items from the stack and uses them to push 1 item,
   without effects other than gas consumption. It must never fail. *)

let arity : string -> int option = function
 (* stack things *)
 | "DIP" -> None
 | "DROP" -> None
 | "DUP" -> None
 | "SWAP" -> None
 | "DIG" -> None
 | "DUG" -> None
 (* control *)
 | "FAILWITH" -> None
 | "EXEC" -> None
 | "IF" -> None
 | "IF_CONS" -> None
 | "IF_LEFT" -> None
 | "IF_NONE" -> None
 | "LOOP" -> None
 | "MAP" -> None
 | "ITER" -> None
 | "LOOP_LEFT" -> None
 (* internal ops *)
 | "CREATE_ACCOUNT" -> None
 | "CREATE_CONTRACT" -> None
 | "TRANSFER_TOKENS" -> None
 | "SET_DELEGATE" -> None
 (* tez arithmetic (can fail) *)
 | "ADD" -> None
 | "MUL" -> None
 | "SUB" -> None (* can fail for tez *)
 (* etc *)
 | "CONCAT" -> None (* sometimes 1, sometimes 2 :( *)
 | "CAST" -> None
 | "RENAME" -> None
 (* stuff *)
 | "PACK" -> Some 1
 | "UNPACK" -> Some 1
 | "BLAKE2B" -> Some 1
 | "SHA256" -> Some 1
 | "SHA512" -> Some 1
 | "ABS" -> Some 1
 | "AMOUNT" -> Some 0
 | "AND" -> Some 2
 | "BALANCE" -> Some 0
 | "CAR" -> Some 1
 | "CDR" -> Some 1
 | "CHECK_SIGNATURE" -> Some 3
 | "COMPARE" -> Some 2
 | "CONS" -> Some 2
 | "IMPLICIT_ACCOUNT" -> Some 1
 | "EDIV" -> Some 2
 | "EMPTY_MAP" -> Some 0
 | "EMPTY_SET" -> Some 0
 | "EQ" -> Some 1
 | "GE" -> Some 1
 | "GET" -> Some 2
 | "GT" -> Some 1
 | "HASH_KEY" -> Some 1
 | "INT" -> Some 1
 | "LAMBDA" -> Some 0
 | "LE" -> Some 1
 | "LEFT" -> Some 1
 | "LSL" -> Some 1
 | "LSR" -> Some 1
 | "LT" -> Some 1
 | "MEM" -> Some 2
 | "NEG" -> Some 1
 | "NEQ" -> Some 1
 | "NIL" -> Some 0
 | "NONE" -> Some 0
 | "NOT" -> Some 1
 | "NOW" -> Some 0
 | "OR" -> Some 2
 | "PAIR" -> Some 2
 | "PUSH" -> Some 0
 | "RIGHT" -> Some 1
 | "SIZE" -> Some 1
 | "SOME" -> Some 1
 | "SOURCE" -> Some 0
 | "SENDER" -> Some 0
 | "SELF" -> Some 0
 | "SLICE" -> Some 3
 | "STEPS_TO_QUOTA" -> Some 0
 | "UNIT" -> Some 0
 | "UPDATE" -> Some 3
 | "XOR" -> Some 2
 | "ADDRESS" -> Some 1
 | "CONTRACT" -> Some 1
 | "ISNAT" -> Some 1
 | "CHAIN_ID" -> Some 0
 | "EMPTY_BIG_MAP" -> Some 0
 | "APPLY" -> Some 2
 | _ -> None

let is_nullary_op (p : string) : bool =
  match arity p with
  | Some 0 -> true
  | _ -> false

let is_unary_op (p : string) : bool =
  match arity p with
  | Some 1 -> true
  | _ -> false

let is_binary_op (p : string) : bool =
  match arity p with
  | Some 2 -> true
  | _ -> false

let is_ternary_op (p : string) : bool =
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
  | Prim (_, "PUSH", [Prim (_, "lambda", [arg; ret], _); code], _) ->
    i_lambda arg ret code
  | Prim (_, "PUSH", _, _) ->
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
  | Prim (_, "PUSH", _, _) -> x
  | Prim (l, p, args, annot) -> Prim (l, p, List.map flatten_seqs args, annot)
  | _ -> x

type peep1 = michelson -> michelson list option
type peep2 = michelson * michelson -> michelson list option
type peep3 = michelson * michelson * michelson -> michelson list option
type peep4 = michelson * michelson * michelson * michelson -> michelson list option
type peep5 = michelson * michelson * michelson * michelson * michelson -> michelson list option

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

let rec peep5 (f : peep5) : michelson list -> bool * michelson list = function
  | [] -> (false, [])
  | [x] -> (false, [x])
  | [x ; y] -> (false, [x ; y])
  | [x ; y ; z] -> (false, [x ; y ; z])
  | [x ; y ; z ; w] -> (false, [x ; y ; z ; w])
  | x1 :: x2 :: x3 :: x4 :: x5 :: xs ->
     match f (x1, x2, x3, x4, x5) with
     | Some xs' -> let (_, xs') = peep5 f (xs' @ xs) in
                   (true, xs')
     | None -> let (changed, xs') = peep5 f (x2 :: x3 :: x4 :: x5 :: xs) in
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

let rec is_failing : michelson -> bool =
  function
  | Seq (_, []) -> false
  | Seq (_, [arg]) -> is_failing arg
  | Seq (l, _ :: args) -> is_failing (Seq (l, args))
  | Prim (_, "FAILWITH", _, _) -> true
  | Prim (_, "IF", [bt; bf], _)
  | Prim (_, "IF_CONS", [bt; bf], _)
  | Prim (_, "IF_LEFT", [bt; bf], _)
  | Prim (_, "IF_NONE", [bt; bf], _) ->
    is_failing bt && is_failing bf
  (* Note: the body of ITER, LOOP, LOOP_LEFT _can_ be
     failing. However, the loop will _not_ be failing, because the
     body might never be executed. The body of MAP _cannot_ be
     failing. *)
  | _ -> false

let is_cond : string -> bool = function
  | "IF"
  | "IF_NONE"
  | "IF_CONS"
  | "IF_LEFT" -> true
  | _ -> false

let opt_drop2 : peep2 = function
  (* nullary_op ; DROP  ↦  *)
  | Prim (_, p, _, _), Prim (_, "DROP", [], _) when is_nullary_op p -> Some []
  (* DUP ; DROP  ↦  *)
  | Prim (_, "DUP", _, _), Prim (_, "DROP", [], _) -> Some []
  (* unary_op ; DROP  ↦  DROP *)
  | Prim (_, p, _, _), Prim (_, "DROP", [], _) when is_unary_op p -> Some [i_drop]
  (* binary_op ; DROP  ↦  DROP ; DROP *)
  | Prim (_, p, _, _), Prim (_, "DROP", [], _) when is_binary_op p -> Some [i_drop; i_drop]
  (* ternary_op ; DROP  ↦  DROP ; DROP ; DROP *)
  | Prim (_, p, _, _), Prim (_, "DROP", [], _) when is_ternary_op p -> Some [i_drop; i_drop; i_drop]
  (* IF { ... ; FAILWITH } { ... } ; DROP  ↦  IF { ... ; FAILWITH } { ... ; DROP } *)
  | Prim (l1, p, [bt; Seq (l2, bf)], annot1), (Prim (_, "DROP", [], _) as drop)
    when is_cond p && is_failing bt ->
    Some [Prim (l1, p, [bt; Seq (l2, bf @ [drop])], annot1)]
  (* IF { ... } { ... ; FAILWITH } ; DROP  ↦  IF { ... ; DROP } { ... ; FAILWITH } *)
  | Prim (l1, p, [Seq (l2, bt); bf], annot1), (Prim (_, "DROP", [], _) as drop)
    when is_cond p && is_failing bf ->
    Some [Prim (l1, p, [Seq (l2, bt @ [drop]); bf], annot1)]
  | _ -> None

let opt_drop4 : peep4 = function
  (* DUP; unary_op; SWAP; DROP  ↦  unary_op *)
  | Prim (_, "DUP", _, _),
    (Prim (_, p, _, _) as unary_op),
    Prim (_, "SWAP", _, _),
    Prim (_, "DROP", [], _)
    when is_unary_op p ->
    Some [unary_op]
  | _ -> None

let opt_dip1 : peep1 = function
  (* DIP {}  ↦  *)
  | Prim (_, "DIP", [Seq (_, [])], _) -> Some []
  (* DIP { nullary_op }  ↦  nullary_op ; SWAP *)
  | Prim (_, "DIP", [Seq (_, [(Prim (_, p, _, _) as push)])], _) when is_nullary_op p ->
     Some [push ; i_swap]
  (* DIP { unary_op }  ↦  SWAP ; unary_op ; SWAP *)
  | Prim (_, "DIP", [Seq (_, [(Prim (_, p, _, _) as unary_op)])], _) when is_unary_op p ->
     Some [i_swap ; unary_op ; i_swap]
  | _ -> None

let opt_dip2 : peep2 = function
  (* combine adjacent dips, shaving a seq and enabling further
     optimization inside the DIP: *)
  (* DIP { code1 } ; DIP { code2 }  ↦  DIP { code1 ; code2 } *)
  | Prim (_, "DIP", [Seq (_, code1)], _), Prim (_, "DIP", [Seq (_, code2)], _) ->
     Some [Prim (0, "DIP", [Seq (0, code1 @ code2)], [])]
  (* DIP { code } ; DROP  ↦  DROP ; code *)
  | Prim (_, "DIP", [Seq (_, code)], _), (Prim (_, "DROP", [], _) as drop) ->
     Some (drop :: code)
  (* nullary_op ; DIP { code }  ↦  code ; nullary_op *)
  | (Prim (_, p, _, _) as nullary_op), Prim (_, "DIP", [Seq (_, code)], _) when is_nullary_op p ->
     Some (code @ [nullary_op])
  (* DIP { code } ; unary_op  ↦  unary_op ; DIP { code } *)
  | (Prim (_, "DIP", [Seq _], _) as dip), (Prim (_, p, _, _) as unary_op) when is_unary_op p ->
     Some [unary_op; dip]
  (* unary_op ; DIP { code }  ↦  DIP { code } ; unary_op *)
  (* | (Prim (_, p, _, _) as unary_op), (Prim (_, "DIP", [Seq _], _) as dip) when is_unary_op p ->
   *    Some [dip; unary_op] *)
  | _ -> None

let opt_dip3 : peep3 = function
  (* replace UNPAIR/UNPIAR with a smaller version *)
  (* TODO probably better to implement optimal UNPAIR in the compiler *)
  (* DUP ; CAR ; DIP { CDR }  ↦  DUP ; CDR ; SWAP ; CAR *)
  | Prim (_, "DUP", _, _),
    (Prim (_, ("CAR" | "CDR"), _, _) as proj1),
    Prim (_, "DIP", [Seq (_, [(Prim (_, ("CAR" | "CDR"), _, _) as proj2)])], _) ->
     Some [ i_dup ; proj2 ; i_swap ; proj1 ]
  | _ -> None

let opt_swap2 : peep2 = function
  (* SWAP ; SWAP  ↦  *)
  | Prim (_, "SWAP", _, _), Prim (_, "SWAP", _, _) ->
     Some []
  (* DUP ; SWAP  ↦  DUP *)
  | Prim (_, "DUP", _, _), Prim (_, "SWAP", _, _) ->
     Some [i_dup]
  (* SWAP ; ADD  ↦  ADD *)
  (* etc *)
  | Prim (_, "SWAP", _, _), (Prim (_, ("ADD" | "OR" | "AND" | "XOR"), _, _) as comm_op) ->
     Some [comm_op]
  | _ -> None

(* for inserted Michelson lambdas *)
let opt_beta3 : peep3 = function
  (* PUSH (lambda ...) code ; SWAP ; EXEC  ↦  f *)
  | Prim (_, "PUSH", [Prim(_, "lambda", _, _); code], _),
    Prim (_, "SWAP", _, _),
    Prim (_, "EXEC", _, _) ->
      (match flatten_seqs code with
       | Seq (_, code) -> Some code
       | _ -> None)
  | _ -> None

let opt_beta5 : peep5 = function
  (* PAIR ; DUP ; CDR ; SWAP ; CAR  ↦  *)
  | Prim (_, "PAIR", _, _),
    Prim (_, "DUP", _, _),
    Prim (_, "CDR", _, _),
    Prim (_, "SWAP", _, _),
    Prim (_, "CAR", _, _) ->
    Some []
  (* PAIR ; DUP ; CAR ; SWAP ; CDR  ↦  SWAP *)
  | Prim (_, "PAIR", _, _),
    Prim (_, "DUP", _, _),
    Prim (_, "CAR", _, _),
    Prim (_, "SWAP", _, _),
    Prim (_, "CDR", _, _) ->
    Some [Prim(-1, "SWAP", [], [])]
  | _ -> None

let opt_digdug1 : peep1 = function
  (* DUG/DIG 0  ↦   *)
  | Prim (_, ("DIG"|"DUG"), [Int (_, n)], _) when Z.equal n Z.zero ->
     Some []
  (* DUG/DIG 1  ↦  SWAP *)
  | Prim (_, ("DIG"|"DUG"), [Int (_, n)], _) when Z.equal n Z.one ->
     Some [i_swap]
  | _ -> None

let opt_digdug2 : peep2 = function
  (* DIG k ; DUG k  ↦   *)
  | (Prim (_, "DIG", [Int (_, k1)], _), Prim (_, "DUG", [Int (_, k2)], _)) when Z.equal k1 k2 ->
     Some []
  (* DUG k ; DIG k  ↦   *)
  | (Prim (_, "DUG", [Int (_, k1)], _), Prim (_, "DIG", [Int (_, k2)], _)) when Z.equal k1 k2 ->
     Some []
  (* DIG 2 ; DIG 2  ↦  DUG 2 *)
  | (Prim (_, "DIG", [Int (_, k1)], _), Prim (_, "DIG", [Int (_, k2)], _)) when Z.equal k1 k2 && Z.equal k1 (Z.of_int 2) ->
     Some [Prim (-1, "DUG", [Int (-1, Z.of_int 2)], [])]
  (* DUG 2 ; DUG 2  ↦  DIG 2 *)
  | (Prim (_, "DUG", [Int (_, k1)], _), Prim (_, "DUG", [Int (_, k2)], _)) when Z.equal k1 k2 && Z.equal k1 (Z.of_int 2) ->
     Some [Prim (-1, "DIG", [Int (-1, Z.of_int 2)], [])]
  | _ -> None

let opt_digdug3 : peep3 = function
  (* DIG 3 ; DIG 3 ; DIG 3  ↦  DUG 3 *)
  | (Prim (_, "DIG", [Int (_, k1)], _), Prim (_, "DIG", [Int (_, k2)], _), Prim (_, "DIG", [Int (_, k3)], _)) when Z.equal k1 k2 && Z.equal k2 k3 && Z.equal k1 (Z.of_int 3) ->
     Some [Prim (-1, "DUG", [Int (-1, Z.of_int 3)], [])]
  (* DUG 3 ; DUG 3 ; DUG 3  ↦  DIG 3 *)
  | (Prim (_, "DUG", [Int (_, k1)], _), Prim (_, "DUG", [Int (_, k2)], _), Prim (_, "DUG", [Int (_, k3)], _)) when Z.equal k1 k2 && Z.equal k2 k3 && Z.equal k1 (Z.of_int 3) ->
     Some [Prim (-1, "DIG", [Int (-1, Z.of_int 3)], [])]
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
       | arg :: args ->
         let arg = opt_tail_fail arg in
         if is_failing arg
         then [arg]
         else arg :: aux args in
     Seq (l, aux args)
  | Prim (l, p, args, annot) ->
     Prim (l, p, List.map opt_tail_fail args, annot)
  | x -> x

let%expect_test _ =
  let seq args = Seq(-1, args) in
  let prim p args = Prim(-1, p, args, []) in
  let code = seq [ prim "IF_LEFT" [ seq [ prim "FAILWITH" [] ; prim "DROP" [] ]
                                  ; seq [ prim "FAILWITH" [] ; prim "DROP" [] ]
                                  ]
                 ; prim "DROP" []
                 ] in
  let code = opt_tail_fail code in
  Format.printf "%a" Tezos_utils.Michelson.pp code ;
  [%expect {| { IF_LEFT { FAILWITH } { FAILWITH } } |}]

let rec opt_combine_drops (x : michelson) : michelson =
  let rec combine : michelson list -> michelson list = function
    | [] -> []
    | Prim (_, "DROP", [], []) :: xs ->
      let xs' = combine xs in
      begin match xs' with
      | [] -> [Prim (-1, "DROP", [], [])]
      | Prim (_, "DROP", [], []) :: xs' -> Prim (-1, "DROP", [Int (-1, Z.of_int 2)], []) :: xs'
      | Prim (_, "DROP", [Int (_, n)], []) :: xs' -> Prim (-1, "DROP", [Int (-1, Z.of_int (1 + Z.to_int n))], []) :: xs'
      | x' :: xs' -> Prim (-1, "DROP", [], []) :: x' :: xs'
      end
    | x :: xs -> x :: combine xs in
  match x with
  | Seq (l, args) -> Seq (l, combine (List.map opt_combine_drops args))
  | Prim (l, p, args, annot) ->
    Prim (l, p, List.map opt_combine_drops args, annot)
  | x -> x

(* number of type arguments for (some) prims, where we will strip
   annots *)
let prim_type_args : string -> int option = function
  | "NONE" -> Some 1
  | "NIL" -> Some 1
  | "EMPTY_SET" -> Some 1
  | "EMPTY_MAP" -> Some 2
  | "EMPTY_BIG_MAP" -> Some 2
  | "LAMBDA" -> Some 2
  | "PUSH" -> Some 1
  | "LEFT" -> Some 1
  | "RIGHT" -> Some 1
  (* _not_ "CONTRACT"! annot is important there *)
  (* but could include "SELF", maybe? *)
  | _ -> None

(* returns (List.firstn n xs, List.skipn n xs) as in Coq (OCaml stdlib
   does not have those...) *)
let split_at (n : int) (xs : 'a list) : 'a list * 'a list =
  let rec aux n acc =
    if n <= 0
    then acc
    else
      let (bef, aft) = acc in
      match aft with
      | [] -> acc
      | x :: aft ->
        aux (n - 1) (x :: bef, aft) in
  let (bef, aft) = aux n ([], xs) in
  (List.rev bef, aft)

(* strip annots from type arguments in some instructions *)
let rec opt_strip_annots (x : michelson) : michelson =
  match x with
  | Seq (l, args) ->
    let args = List.map opt_strip_annots args in
    Seq (l, args)
  | Prim (l, p, args, annot) ->
    begin
      match prim_type_args p with
      | Some n ->
        let (type_args, args) = split_at n args in
        (* strip annots from type args *)
        let type_args = List.map strip_annots type_args in
        (* recur into remaining args *)
        let args = List.map opt_strip_annots args in
        Prim (l, p, type_args @ args, annot)
      | None ->
        let args = List.map opt_strip_annots args in
        Prim (l, p, args, annot)
    end
  | x -> x

let optimize : michelson -> michelson =
  fun x ->
  let x = flatten_seqs x in
  let x = opt_tail_fail x in
  let optimizers = [ peephole @@ peep2 opt_drop2 ;
                     peephole @@ peep4 opt_drop4 ;
                     peephole @@ peep3 opt_dip3 ;
                     peephole @@ peep2 opt_dip2 ;
                     peephole @@ peep1 opt_dip1 ;
                     peephole @@ peep2 opt_swap2 ;
                     peephole @@ peep3 opt_beta3 ;
                     peephole @@ peep5 opt_beta5 ;
                     peephole @@ peep1 opt_digdug1 ;
                     peephole @@ peep2 opt_digdug2 ;
                     peephole @@ peep3 opt_digdug3 ;
                   ] in
  let x = iterate_optimizer (sequence_optimizers optimizers) x in
  let x = opt_combine_drops x in
  let x = opt_strip_annots x in
  let x = use_lambda_instr x in
  x
