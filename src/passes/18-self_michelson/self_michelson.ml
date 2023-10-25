(* This file attempts to optimize Michelson code. The goal is to
   reduce the code size (the size of the binary Micheline.)

   I have ignored the 'execution gas' completely, because it seems
   that users will encounter code size problems earlier and more
   often.
*)

open Tezos_micheline.Micheline
open Tezos_utils.Michelson
include Helpers
open Peephole
open Peephole.Let_syntax

type proto = Environment.Protocols.t

(* `arity p` should be `Some n` only if p is (always) an instruction
   which removes n items from the stack and uses them to push 1 item,
   without effects other than gas consumption. It must never fail. *)

let arity : _ node list -> string -> int option =
 fun args -> function
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
  | "GET" ->
    (match args with
    | [] -> Some 2
    | [ _k ] -> Some 1
    | _ -> None)
  | "GT" -> Some 1
  | "HASH_KEY" -> Some 1
  | "INT" -> Some 1
  | "LAMBDA" -> Some 0
  | "LE" -> Some 1
  | "LEFT" -> Some 1
  | "LSL" -> Some 2
  | "LSR" -> Some 2
  | "LT" -> Some 1
  | "MEM" -> Some 2
  | "NEG" -> Some 1
  | "NEQ" -> Some 1
  | "NIL" -> Some 0
  | "NONE" -> Some 0
  | "NOT" -> Some 1
  | "NOW" -> Some 0
  | "OR" -> Some 2
  | "PAIR" ->
    (match args with
    | [] -> Some 2
    | [ Int (_, k) ] -> Some (Z.to_int k)
    | _ -> None)
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
  | "UPDATE" ->
    (match args with
    | [] -> Some 3
    | [ _k ] -> Some 2
    | _ -> None)
  | "XOR" -> Some 2
  | "ADDRESS" -> Some 1
  | "CONTRACT" -> Some 1
  | "ISNAT" -> Some 1
  | "CHAIN_ID" -> Some 0
  | "EMPTY_BIG_MAP" -> Some 0
  | "APPLY" -> Some 2
  | _ -> None


let get_arity (op : _ node) : int option =
  match op with
  | Prim (_, p, args, _) -> arity args p
  | _ -> None


let is_nullary_op op : bool = get_arity op |> Option.exists ~f:(( = ) 0)
let is_unary_op op : bool = get_arity op |> Option.exists ~f:(( = ) 1)
let is_binary_op op : bool = get_arity op |> Option.exists ~f:(( = ) 2)
let is_ternary_op op : bool = get_arity op |> Option.exists ~f:(( = ) 3)

let is_injective : string -> bool = function
  (* stack things *)
  | "DIP" -> false
  | "DROP" -> false
  | "DUP" -> true
  | "SWAP" -> true
  | "DIG" -> true
  | "DUG" -> true
  (* control *)
  | "FAILWITH" -> false
  | "EXEC" -> false
  | "IF" -> false
  | "IF_CONS" -> false
  | "IF_LEFT" -> false
  | "IF_NONE" -> false
  | "LOOP" -> false
  | "MAP" -> false
  | "ITER" -> false
  | "LOOP_LEFT" -> false
  (* internal ops *)
  | "CREATE_ACCOUNT" -> true
  | "CREATE_CONTRACT" -> false
  | "TRANSFER_TOKENS" -> false
  | "SET_DELEGATE" -> true
  (* tez arithmetic (can fail) *)
  | "ADD" -> false
  | "MUL" -> false
  | "SUB" -> false (* can fail for tez *)
  (* etc *)
  | "CONCAT" -> false (* sometimes 1, sometimes 2 :( *)
  | "CAST" -> false
  | "RENAME" -> true
  (* stuff *)
  | "PACK" -> false
  | "UNPACK" -> true
  | "BLAKE2B" -> true
  | "SHA256" -> true
  | "SHA512" -> true
  | "ABS" -> true
  | "AMOUNT" -> true
  | "AND" -> false
  | "BALANCE" -> true
  | "CAR" -> false
  | "CDR" -> false
  | "CHECK_SIGNATURE" -> true
  | "COMPARE" -> false
  | "CONS" -> true
  | "IMPLICIT_ACCOUNT" -> true
  | "EDIV" -> false
  | "EMPTY_MAP" -> true
  | "EMPTY_SET" -> true
  | "EQ" -> false
  | "GE" -> false
  | "GET" -> false
  | "GT" -> false
  | "HASH_KEY" -> true
  | "INT" -> true
  | "LAMBDA" -> true
  | "LE" -> false
  | "LEFT" -> true
  | "LSL" -> true
  | "LSR" -> true
  | "LT" -> false
  | "MEM" -> false
  | "NEG" -> false
  | "NEQ" -> false
  | "NIL" -> true
  | "NONE" -> true
  | "NOT" -> false
  | "NOW" -> true
  | "OR" -> false
  | "PAIR" -> true
  | "PUSH" -> true
  | "RIGHT" -> true
  | "SIZE" -> false
  | "SOME" -> true
  | "SOURCE" -> true
  | "SENDER" -> true
  | "SELF" -> true
  | "SLICE" -> true
  | "STEPS_TO_QUOTA" -> true
  | "UNIT" -> true
  | "UPDATE" -> false
  | "XOR" -> true
  | "ADDRESS" -> false
  | "CONTRACT" -> true
  | "ISNAT" -> true
  | "CHAIN_ID" -> true
  | "EMPTY_BIG_MAP" -> true
  | "APPLY" -> false
  | _ -> false


let eq_type ll lr =
  let rec compare_list l r =
    match List.zip l r with
    | List.Or_unequal_lengths.Unequal_lengths -> false
    | Ok lr ->
      List.for_all
        ~f:(fun v -> Bool.equal true v)
        (List.map ~f:(fun (a, b) -> aux_eq a b) lr)
  and aux_eq l r =
    let open Tezos_micheline.Micheline in
    match l, r with
    | Prim (_, s, l, a), Prim (_, s', l', a')
      when String.equal s s' && List.equal String.equal a a' -> compare_list l l'
    | Seq (_, l), Seq (_, l') -> compare_list l l'
    | _, _ -> false
  in
  compare_list ll lr


let unseq : type meta. has_comment:(meta -> bool) -> meta michelson -> meta michelson list
  =
 fun ~has_comment -> function
  | Seq (m, args) when not (has_comment m) -> args
  | x -> [ x ]


(* Replace `PUSH (lambda a b) {}` with `LAMBDA a b {}` *)
let rec use_lambda_instr : _ michelson -> _ michelson =
 fun x ->
  match x with
  | Seq (l, args) -> Seq (l, List.map ~f:use_lambda_instr args)
  | Prim (_, "PUSH", [ Prim (_, "lambda", [ _; _ ], _); Prim (_, "constant", _, _) ], _)
    -> x
  | Prim (l, "PUSH", [ Prim (_, "lambda", [ arg; ret ], _); code ], _) ->
    Prim (l, "LAMBDA", [ arg; ret; code ], [])
  | Prim (_, "PUSH", _, _) -> x (* possibly missing some nested lambdas *)
  | Prim (l, p, args, annot) -> Prim (l, p, List.map ~f:use_lambda_instr args, annot)
  | _ -> x


(* This flattens nested seqs. {} is erased, { { code1 } ; { code2 } }
   becomes { code1 ; code2 }, etc. This is important because each seq
   costs 5 bytes, for the "Seq" tag and a 4 byte length. *)
let rec flatten_seqs
    : type meta. has_comment:(meta -> bool) -> meta michelson -> meta michelson
  =
 fun ~has_comment x ->
  match x with
  | Seq (l, args) ->
    let args =
      List.concat
      @@ List.map ~f:(fun x -> unseq ~has_comment (flatten_seqs ~has_comment x)) args
    in
    Seq (l, args)
  (* Should not flatten literal seq data in PUSH. Ugh... *)
  | Prim (_, "PUSH", _, _) -> x
  | Prim (l, p, args, annot) ->
    Prim (l, p, List.map ~f:(flatten_seqs ~has_comment) args, annot)
  | _ -> x


(* apply f to all seqs *)
let rec on_seqs (f : 'l michelson list -> bool * 'l michelson list)
    : 'l michelson -> bool * 'l michelson
  =
  let peep_args ~seq args =
    let changed, args = if seq then f args else false, args in
    List.fold_map
      ~f:(fun changed1 arg ->
        let changed2, arg = on_seqs f arg in
        changed1 || changed2, arg)
      ~init:changed
      args
  in
  function
  | Seq (l, args) ->
    let changed, args = peep_args ~seq:true args in
    changed, Seq (l, args)
  (* Should not optimize seqs (even code seqs) under PUSH. Ugh... *)
  | Prim (_, "PUSH", _, _) as x -> false, x
  | Prim (l, p, args, annot) ->
    let changed, args = peep_args ~seq:false args in
    changed, Prim (l, p, args, annot)
  | x -> false, x


(* apply the optimizers in order *)
let rec sequence_optimizers (fs : ('l michelson -> bool * 'l michelson) list)
    : 'l michelson -> bool * 'l michelson
  =
  match fs with
  | [] -> fun x -> false, x
  | f :: fs ->
    fun x ->
      let changed1, x = f x in
      let changed2, x = sequence_optimizers fs x in
      changed1 || changed2, x


(* take the fixed point of an optimizer (!) *)
let rec iterate_optimizer (f : 'l michelson -> bool * 'l michelson)
    : 'l michelson -> 'l michelson
  =
 fun x ->
  let changed, x = f x in
  if changed then iterate_optimizer f x else x


let rec is_failing : _ michelson -> bool = function
  | Seq (_, []) -> false
  | Seq (_, [ arg ]) -> is_failing arg
  | Seq (l, _ :: args) -> is_failing (Seq (l, args))
  | Prim (_, "FAILWITH", _, _) -> true
  | Prim (_, "IF", [ bt; bf ], _)
  | Prim (_, "IF_CONS", [ bt; bf ], _)
  | Prim (_, "IF_LEFT", [ bt; bf ], _)
  | Prim (_, "IF_NONE", [ bt; bf ], _) -> is_failing bt && is_failing bf
  (* Note: the body of ITER, LOOP, LOOP_LEFT _can_ be
     failing. However, the loop will _not_ be failing, because the
     body might never be executed. The body of MAP _cannot_ be
     failing. *)
  | _ -> false


let is_cond : string -> bool = function
  | "IF" | "IF_NONE" | "IF_CONS" | "IF_LEFT" -> true
  | _ -> false


let rec last_is
    :  (_ michelson -> _ michelson -> bool) -> (_ michelson -> bool) -> _ michelson
    -> _ option
  =
 fun eq pred -> function
  | Seq (_, []) -> None
  | Seq (_, [ arg ]) -> last_is eq pred arg
  | Seq (l, _ :: args) -> last_is eq pred (Seq (l, args))
  | Prim (_, p, [ bt; bf ], _) when is_cond p ->
    let ( let+ ) v f = Option.bind v ~f in
    let+ bt = last_is eq pred bt in
    let+ bf = last_is eq pred bf in
    if eq bt bf then Some bt else None
  | Prim _ as prim when pred prim -> Some prim
  | _ -> None


let rec remove_last : (_ michelson -> bool) -> _ michelson -> _ michelson =
 fun pred -> function
  | Seq (l, []) -> Seq (l, [])
  | Seq (l, ls) ->
    let init, last = List.drop_last_exn ls, List.last_exn ls in
    let last =
      match last with
      | Prim (l, p, [ bt; bf ], t) when is_cond p ->
        let bt = remove_last pred bt in
        let bf = remove_last pred bf in
        [ Prim (l, p, [ bt; bf ], t) ]
      | Prim _ as prim when pred prim -> []
      | Seq _ ->
        let last = remove_last pred last in
        [ last ]
      (* if _don't_ remove something (should be impossible) we will
          wind up in an infinite loop, so better to raise an error: *)
      | _ -> failwith ("Internal error: " ^ __LOC__)
    in
    Seq (l, init @ last)
  | t -> t


let opt_drop1 : _ peep1 = function
  | Prim (l, "DROP", [ Int (_, n) ], annot) when Z.equal n Z.one ->
    Some [ Prim (l, "DROP", [], annot) ]
  | _ -> None


let opt_drop2 : _ peep2 = function
  (* nullary_op ; DROP  ↦  *)
  | op, Prim (_, "DROP", [], _) when is_nullary_op op -> Some []
  (* DUP ; DROP  ↦  *)
  | Prim (_, "DUP", [], _), Prim (_, "DROP", [], _) -> Some []
  (* unary_op ; DROP  ↦  DROP *)
  | op, (Prim (_, "DROP", [], _) as drop) when is_unary_op op -> Some [ drop ]
  (* binary_op ; DROP  ↦  DROP ; DROP *)
  | op, (Prim (_, "DROP", [], _) as drop) when is_binary_op op -> Some [ drop; drop ]
  (* ternary_op ; DROP  ↦  DROP ; DROP ; DROP *)
  | op, (Prim (_, "DROP", [], _) as drop) when is_ternary_op op ->
    Some [ drop; drop; drop ]
  (* IF { ... ; FAILWITH } { ... } ; DROP  ↦  IF { ... ; FAILWITH } { ... ; DROP } *)
  | Prim (l1, p, [ bt; Seq (l2, bf) ], annot1), (Prim (_, "DROP", [], _) as drop)
    when is_cond p && is_failing bt ->
    Some [ Prim (l1, p, [ bt; Seq (l2, bf @ [ drop ]) ], annot1) ]
  (* IF { ... } { ... ; FAILWITH } ; DROP  ↦  IF { ... ; DROP } { ... ; FAILWITH } *)
  | Prim (l1, p, [ Seq (l2, bt); bf ], annot1), (Prim (_, "DROP", [], _) as drop)
    when is_cond p && is_failing bf ->
    Some [ Prim (l1, p, [ Seq (l2, bt @ [ drop ]); bf ], annot1) ]
  | _ -> None


(* "depth" is the length of the prefix of the stack which an
   instruction affects. *)
let digdug_depth : _ michelson -> int option = function
  | Prim (_, "SWAP", [], _) -> Some 2
  | Prim (_, ("DIG" | "DUG"), [ Int (_, k) ], _) -> Some (Z.to_int (Z.succ k))
  | _ -> None


(* elide SWAP/DIG/DUG when followed by sufficiently many DROP *)
let opt_digdug_drop () : _ peep =
  let* x = peep in
  match digdug_depth x with
  | None -> No_change
  | Some depth ->
    let rec aux acc depth =
      if depth = 0
      then Changed (List.rev acc)
      else
        let* x = peep in
        match x with
        | Prim (_, "DROP", [], _) as drop -> aux (drop :: acc) (depth - 1)
        | _ -> No_change
    in
    aux [] depth


let opt_drop3 : _ peep3 = function
  (* SWAP ; DROP ; DROP  ↦  DROP ; DROP *)
  | ( Prim (_, "SWAP", [], _)
    , (Prim (_, "DROP", [], _) as drop1)
    , (Prim (_, "DROP", [], _) as drop2) ) -> Some [ drop1; drop2 ]
  | _ -> None


let opt_drop4 : _ peep4 = function
  (* DUP; unary_op; SWAP; DROP  ↦  unary_op *)
  | Prim (_, "DUP", [], _), unary_op, Prim (_, "SWAP", _, _), Prim (_, "DROP", [], _)
    when is_unary_op unary_op -> Some [ unary_op ]
  | _ -> None


let opt_dip1 : _ peep1 = function
  (* DIP {}  ↦  *)
  | Prim (_, "DIP", [ Seq (_, []) ], _) -> Some []
  (* DIP { nullary_op }  ↦  nullary_op ; SWAP *)
  | Prim (l, "DIP", [ Seq (_, [ push ]) ], _) when is_nullary_op push ->
    Some [ push; Prim (l, "SWAP", [], []) ]
  (* DIP { unary_op }  ↦  SWAP ; unary_op ; SWAP *)
  | Prim (l, "DIP", [ Seq (_, [ unary_op ]) ], _) when is_unary_op unary_op ->
    Some [ Prim (l, "SWAP", [], []); unary_op; Prim (l, "SWAP", [], []) ]
  | _ -> None


let opt_dip2 : _ peep2 = function
  (* combine adjacent dips, shaving a seq and enabling further
     optimization inside the DIP: *)
  (* DIP { code1 } ; DIP { code2 }  ↦  DIP { code1 ; code2 } *)
  | Prim (l1, "DIP", [ Seq (l2, code1) ], _), Prim (_, "DIP", [ Seq (_, code2) ], _) ->
    Some [ Prim (l1, "DIP", [ Seq (l2, code1 @ code2) ], []) ]
  (* DIP { code } ; DROP  ↦  DROP ; code *)
  | Prim (_, "DIP", [ Seq (_, code) ], _), (Prim (_, "DROP", [], _) as drop) ->
    Some (drop :: code)
  (* nullary_op ; DIP { code }  ↦  code ; nullary_op *)
  | nullary_op, Prim (_, "DIP", [ Seq (_, code) ], _) when is_nullary_op nullary_op ->
    Some (code @ [ nullary_op ])
  (* DIP { code } ; unary_op  ↦  unary_op ; DIP { code } *)
  | (Prim (_, "DIP", [ Seq _ ], _) as dip), unary_op when is_unary_op unary_op ->
    Some [ unary_op; dip ]
  (* unary_op ; DIP { code }  ↦  DIP { code } ; unary_op *)
  (* | (Prim (_, p, _, _) as unary_op), (Prim (_, "DIP", [Seq _], _) as dip) when is_unary_op p ->
   *    Some [dip; unary_op] *)
  | _ -> None


let opt_dip3 : _ peep3 = function
  (* replace UNPAIR/UNPIAR with a smaller version *)
  (* TODO probably better to implement optimal UNPAIR in the compiler *)
  (* DUP ; CAR ; DIP { CDR }  ↦  DUP ; CDR ; SWAP ; CAR *)
  | ( Prim (_, "DUP", [], _)
    , (Prim (_, ("CAR" | "CDR"), [], _) as proj1)
    , Prim (l, "DIP", [ Seq (_, [ (Prim (_, ("CAR" | "CDR"), [], _) as proj2) ]) ], _) )
    -> Some [ Prim (l, "DUP", [], []); proj2; Prim (l, "SWAP", [], []); proj1 ]
  | _ -> None


let opt_cond ?pre_type : _ peep1 = function
  | Prim (l, p, [ bt; bf ], annot) when is_cond p ->
    let eq_type ll lr =
      match pre_type with
      | None -> true
      | Some pre_type -> eq_type (pre_type ll) (pre_type lr)
    in
    let f =
      match pre_type with
      | Some _ -> fun _ -> true
      | None -> is_injective
    in
    let force_nil =
      List.filter
        ~f
        [ "SWAP"
        ; "PAIR"
        ; "UNPAIR"
        ; "CAR"
        ; "CDR"
        ; "DUP"
        ; "DROP"
        ; "UNIT"
        ; "SOME"
        ; "CONS"
        ; "SIZE"
        ; "UPDATE"
        ; "ADD"
        ; "SUB"
        ; "MUL"
        ; "EDIV"
        ; "ABS"
        ; "ISNAT"
        ; "INT"
        ; "NEG"
        ; "LSL"
        ; "LSR"
        ; "OR"
        ; "AND"
        ; "XOR"
        ; "NOT"
        ; "COMPARE"
        ; "EQ"
        ; "NEQ"
        ; "LT"
        ; "GT"
        ; "LE"
        ; "GE"
        ; "SLICE"
        ; "CONCAT"
        ; "PACK"
        ; "SENDER"
        ; "AMOUNT"
        ; "ADDRESS"
        ; "SOURCE"
        ; "BALANCE"
        ; "LEVEL"
        ; "NOW"
        ]
    in
    let force_z =
      List.filter
        ~f
        [ "PAIR"; "UNPAIR"; "CAR"; "CDR"; "DUP"; "DROP"; "DIG"; "DUG"; "UPDATE" ]
    in
    let no_force = List.filter ~f [ "NIL"; "NONE"; "LEFT"; "RIGHT" ] in
    let pred = function
      | Prim (_, l, _, _)
        when List.mem ~equal:String.equal (force_nil @ force_z @ no_force) l -> true
      | _ -> false
    in
    let eq m1 m2 =
      match m1, m2 with
      | Prim (ll, l, [], _), Prim (lr, r, [], _)
        when List.mem ~equal:String.equal force_nil l && String.equal l r && eq_type ll lr
        -> true
      | Prim (ll, l, _, _), Prim (lr, r, _, _)
        when List.mem ~equal:String.equal no_force l && String.equal l r && eq_type ll lr
        -> true
      | Prim (ll, l, [ Int (_, n) ], _), Prim (lr, r, [ Int (_, m) ], _)
        when List.mem ~equal:String.equal force_z l
             && String.equal l r
             && Z.equal n m
             && eq_type ll lr -> true
      | _ -> false
    in
    (match last_is eq pred bt, last_is eq pred bf with
    | Some l_op, Some r_op when eq l_op r_op ->
      let bt = remove_last pred bt in
      let bf = remove_last pred bf in
      Some [ Prim (l, p, [ bt; bf ], annot); l_op ]
    | _ -> None)
  | _ -> None


let opt_swap2 : _ peep2 = function
  (* SWAP ; SWAP  ↦  *)
  | Prim (_, "SWAP", _, _), Prim (_, "SWAP", _, _) -> Some []
  (* DUP ; SWAP  ↦  DUP *)
  | (Prim (_, "DUP", [], _) as dup), Prim (_, "SWAP", _, _) -> Some [ dup ]
  (* SWAP ; ADD  ↦  ADD *)
  (* etc *)
  | Prim (_, "SWAP", _, _), (Prim (_, ("ADD" | "OR" | "AND" | "XOR"), _, _) as comm_op) ->
    Some [ comm_op ]
  | _ -> None


(* for inserted Michelson lambdas *)
let opt_beta3 : _ peep3 = function
  (* PUSH (lambda ...) code ; SWAP ; EXEC  ↦  code *)
  (* LAMBDA a b code ; SWAP ; EXEC  ↦  code *)
  | ( ( Prim (_, "PUSH", [ Prim (_, "lambda", _, _); code ], _)
      | Prim (_, "LAMBDA", [ _; _; code ], _) )
    , Prim (_, "SWAP", _, _)
    , Prim (_, "EXEC", _, _) ) ->
    (match code with
    | Seq (_, code) -> Some code
    | Prim (_, "constant", _, _) -> Some [ code ]
    | _ -> None)
  | _ -> None


let opt_beta5 : _ peep5 = function
  (* PAIR ; DUP ; CDR ; SWAP ; CAR  ↦  *)
  | ( Prim (_, "PAIR", [], _)
    , Prim (_, "DUP", [], _)
    , Prim (_, "CDR", [], _)
    , Prim (_, "SWAP", _, _)
    , Prim (_, "CAR", [], _) ) -> Some []
  (* PAIR ; DUP ; CAR ; SWAP ; CDR  ↦  SWAP *)
  | ( Prim (l, "PAIR", [], _)
    , Prim (_, "DUP", [], _)
    , Prim (_, "CAR", [], _)
    , Prim (_, "SWAP", _, _)
    , Prim (_, "CDR", [], _) ) -> Some [ Prim (l, "SWAP", [], []) ]
  | _ -> None


let opt_digdug1 : _ peep1 = function
  (* DUG/DIG 0  ↦   *)
  | Prim (_, ("DIG" | "DUG"), [ Int (_, n) ], _) when Z.equal n Z.zero -> Some []
  (* DUG/DIG 1  ↦  SWAP *)
  | Prim (l, ("DIG" | "DUG"), [ Int (_, n) ], _) when Z.equal n Z.one ->
    Some [ Prim (l, "SWAP", [], []) ]
  | _ -> None


let opt_digdug2 : _ peep2 = function
  (* DIG k ; DUG k  ↦   *)
  | Prim (_, "DIG", [ Int (_, k1) ], _), Prim (_, "DUG", [ Int (_, k2) ], _)
    when Z.equal k1 k2 -> Some []
  (* DUG k ; DIG k  ↦   *)
  | Prim (_, "DUG", [ Int (_, k1) ], _), Prim (_, "DIG", [ Int (_, k2) ], _)
    when Z.equal k1 k2 -> Some []
  | _ -> None


let flip_digdug : string -> string = function
  | "DIG" -> "DUG"
  | "DUG" -> "DIG"
  | s -> s


(* DIG k ; ...k times  ↦  DUG k
   DUG k ; ...k times  ↦  DIG k *)
let opt_digdug_cycles () =
  let* x = peep in
  match x with
  | Prim (l1, (("DIG" | "DUG") as p), [ Int (l2, k) ], _) when Z.geq k (Z.of_int 2) ->
    let rec aux n =
      if Z.equal k n
      then Changed [ Prim (l1, flip_digdug p, [ Int (l2, k) ], []) ]
      else
        let* x = peep in
        match x with
        | Prim (_, (("DIG" | "DUG") as p'), [ Int (_, k') ], _)
          when String.equal p p' && Z.equal k k' -> aux (Z.succ n)
        | _ -> No_change
    in
    aux Z.one
  | _ -> No_change


(* remove dead "UNPAIR" *)
let opt_dead_unpair : _ peep3 = function
  | Prim (l, "UNPAIR", [], _), Prim (_, "DROP", [], _), Prim (_, "DROP", [], _) ->
    Some [ Prim (l, "DROP", [], []) ]
  | _ -> None


let opt_beta2 : _ peep2 = function
  (* PAIR ; UNPAIR  ↦  *)
  | Prim (_, "PAIR", [], _), Prim (_, "UNPAIR", [], _) -> Some []
  (* PAIR n ; UNPAIR n  ↦  *)
  | Prim (_, "PAIR", [ Int (_, n1) ], _), Prim (_, "UNPAIR", [ Int (_, n2) ], _)
    when Z.equal n1 n2 -> Some []
  (* PAIR ; CAR  ↦  SWAP ; DROP *)
  (* not really any better but looks less stupid *)
  | Prim (l1, "PAIR", [], _), Prim (l2, "CAR", [], _) ->
    Some [ Prim (l1, "SWAP", [], []); Prim (l2, "DROP", [], []) ]
  (* PAIR ; CDR  ↦  DROP *)
  | Prim (_, "PAIR", [], _), Prim (l, "CDR", [], _) -> Some [ Prim (l, "DROP", [], []) ]
  | _ -> None


let opt_eta2 : _ peep2 = function
  (* UNPAIR ; PAIR  ↦  *)
  | Prim (_, "UNPAIR", [], _), Prim (_, "PAIR", [], _) -> Some []
  (* UNPAIR n ; PAIR n  ↦  *)
  | Prim (_, "UNPAIR", [ Int (_, n1) ], _), Prim (_, "PAIR", [ Int (_, n2) ], _)
    when Z.equal n1 n2 -> Some []
  | _ -> None


(* PUSH int 1 ; NEG  ↦  PUSH int -1 *)
(* PUSH nat 1 ; NEG  ↦  PUSH int -1 *)
let opt_neg_int : _ peep2 = function
  | ( Prim (l1, "PUSH", [ Prim (l2, ("int" | "nat"), [], a1); Int (l3, n) ], a2)
    , Prim (_, "NEG", [], _) ) ->
    Some [ Prim (l1, "PUSH", [ Prim (l2, "int", [], a1); Int (l3, Z.neg n) ], a2) ]
  | _ -> None


(* PUSH nat 1 ; INT ; NEG  ↦  PUSH int -1 *)
let opt_nat_int_neg : _ peep3 = function
  | ( Prim (l1, "PUSH", [ Prim (l2, "nat", [], a1); Int (l3, n) ], a2)
    , Prim (_, "INT", [], _)
    , Prim (_, "NEG", [], _) ) ->
    Some [ Prim (l1, "PUSH", [ Prim (l2, "int", [], a1); Int (l3, Z.neg n) ], a2) ]
  | _ -> None


let opt_unpair_edo : _ peep4 = function
  | ( Prim (l, "DUP", [], [])
    , Prim (_, "CDR", [], [])
    , Prim (_, "SWAP", [], [])
    , Prim (_, "CAR", [], []) ) -> Some [ Prim (l, "UNPAIR", [], []) ]
  | _ -> None


let opt_dup1 : _ peep1 = function
  | Prim (l, "DUP", [ Int (_, n) ], annot) when Z.equal n Z.one ->
    Some [ Prim (l, "DUP", [], annot) ]
  | _ -> None


let opt_dupn_edo : _ peep3 = function
  | ( Prim (l1, "DIG", [ Int (l2, n) ], [])
    , Prim (_, "DUP", [], [])
    , Prim (_, "DUG", [ Int (_, m) ], []) )
    when Z.equal (Z.succ n) m -> Some [ Prim (l1, "DUP", [ Int (l2, m) ], []) ]
  | ( Prim (_, "SWAP", [], [])
    , Prim (l1, "DUP", [], [])
    , Prim (_, "DUG", [ Int (l2, m) ], []) )
    when Z.equal (Z.of_int 2) m -> Some [ Prim (l1, "DUP", [ Int (l2, m) ], []) ]
  | _ -> None


let opt_unpair_cdr () : _ peep =
  let* x = peep in
  match x with
  | Prim (l, "UNPAIR", [], _) ->
    let* x = peep in
    (match x with
    | Prim (_, "DROP", [], _) -> Changed [ Prim (l, "CDR", [], []) ]
    | _ -> No_change)
  | _ -> No_change


let opt_unpair_car () : _ peep =
  let* x = peep in
  match x with
  | Prim (l, "UNPAIR", [], _) ->
    let* x = peep in
    (match x with
    | Prim (_, "SWAP", [], _) ->
      let* x = peep in
      (match x with
      | Prim (_, "DROP", [], _) -> Changed [ Prim (l, "CAR", [], []) ]
      | _ -> No_change)
    | _ -> No_change)
  | _ -> No_change


(* UNPAIR 2  ↦  UNPAIR *)
let opt_unpair2 () : _ peep =
  let* x = peep in
  match x with
  | Prim (l, "UNPAIR", [ Int (_, k) ], _) when Z.(equal k (of_int 2)) ->
    Changed [ Prim (l, "UNPAIR", [], []) ]
  | _ -> No_change


(* PAIR 2  ↦  PAIR *)
let opt_pair2 () : _ peep =
  let* x = peep in
  match x with
  | Prim (l, "PAIR", [ Int (_, k) ], _) when Z.(equal k (of_int 2)) ->
    Changed [ Prim (l, "PAIR", [], []) ]
  | _ -> No_change


(* GET 0  ↦  *)
(* GET 1  ↦  CAR *)
(* GET 2  ↦  CDR *)
let opt_get () : _ peep =
  let* x = peep in
  match x with
  | Prim (l, "GET", [ Int (_, k) ], _) ->
    if Z.(equal k (of_int 0))
    then Changed []
    else if Z.(equal k (of_int 1))
    then Changed [ Prim (l, "CAR", [], []) ]
    else if Z.(equal k (of_int 2))
    then Changed [ Prim (l, "CDR", [], []) ]
    else No_change
  | _ -> No_change


(* PUSH unit Unit  ↦  UNIT *)
let opt_push () : _ peep =
  let* x = peep in
  match x with
  | Prim (l, "PUSH", [ Prim (_, "unit", [], _); Prim (_, "Unit", [], _) ], _) ->
    Changed [ Prim (l, "UNIT", [], []) ]
  | _ -> No_change


(* This "optimization" deletes dead code produced by the compiler
   after a FAILWITH, which is illegal in Michelson. This means we are
   thwarting the intent of the Michelson tail fail restriction -- the
   LIGO _user_ might accidentally write dead code immediately after a
   failure, and we will simply erase it. *)
let rec opt_tail_fail : _ michelson -> _ michelson = function
  | Seq (l, args) ->
    let rec aux args =
      match args with
      | [] -> []
      | arg :: args ->
        let arg = opt_tail_fail arg in
        if is_failing arg then [ arg ] else arg :: aux args
    in
    Seq (l, aux args)
  | Prim (l, p, args, annot) -> Prim (l, p, List.map ~f:opt_tail_fail args, annot)
  | x -> x


let%expect_test _ =
  let seq args = Seq (-1, args) in
  let prim p args = Prim (-1, p, args, []) in
  let code =
    seq
      [ prim
          "IF_LEFT"
          [ seq [ prim "FAILWITH" []; prim "DROP" [] ]
          ; seq [ prim "FAILWITH" []; prim "DROP" [] ]
          ]
      ; prim "DROP" []
      ]
  in
  let code = opt_tail_fail code in
  Format.printf "%a" Tezos_utils.Michelson.pp code;
  [%expect {| { IF_LEFT { FAILWITH } { FAILWITH } } |}]

let rec opt_combine_drops (x : 'l michelson) : 'l michelson =
  let rec combine : _ michelson list -> _ michelson list = function
    | [] -> []
    | Prim (l, "DROP", [], []) :: xs ->
      let xs' = combine xs in
      (match xs' with
      | [] -> [ Prim (l, "DROP", [], []) ]
      | Prim (_, "DROP", [], []) :: xs' ->
        Prim (l, "DROP", [ Int (l, Z.of_int 2) ], []) :: xs'
      | Prim (_, "DROP", [ Int (_, n) ], []) :: xs' ->
        Prim (l, "DROP", [ Int (l, Z.of_int (1 + Z.to_int n)) ], []) :: xs'
      | x' :: xs' -> Prim (l, "DROP", [], []) :: x' :: xs')
    | x :: xs -> x :: combine xs
  in
  match x with
  | Seq (l, args) -> Seq (l, combine (List.map ~f:opt_combine_drops args))
  | Prim (l, p, args, annot) -> Prim (l, p, List.map ~f:opt_combine_drops args, annot)
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
    else (
      let bef, aft = acc in
      match aft with
      | [] -> acc
      | x :: aft -> aux (n - 1) (x :: bef, aft))
  in
  let bef, aft = aux n ([], xs) in
  List.rev bef, aft


(* strip annots from type arguments in some instructions *)
let rec opt_strip_annots (x : _ michelson) : _ michelson =
  match x with
  | Seq (l, args) ->
    let args = List.map ~f:opt_strip_annots args in
    Seq (l, args)
  | Prim (l, p, args, annot) ->
    (match prim_type_args p with
    | Some n ->
      let type_args, args = split_at n args in
      (* strip annots from type args *)
      let type_args = List.map ~f:strip_annots type_args in
      (* recur into remaining args *)
      let args = List.map ~f:opt_strip_annots args in
      Prim (l, p, type_args @ args, annot)
    | None ->
      let args = List.map ~f:opt_strip_annots args in
      Prim (l, p, args, annot))
  | x -> x


let optimize
    : type meta.
      Environment.Protocols.t
      -> experimental_disable_optimizations_for_debugging:bool
      -> has_comment:(meta -> bool)
      -> meta michelson
      -> meta michelson
  =
 fun proto ~experimental_disable_optimizations_for_debugging ~has_comment x ->
  ignore proto;
  let x = flatten_seqs ~has_comment x in
  let x = opt_tail_fail x in
  let optimizers =
    [ peephole @@ peep1 opt_drop1
    ; peephole @@ peep2 opt_drop2
    ; peephole @@ peep3 opt_drop3
    ; peephole @@ peep4 opt_drop4
    ; peephole @@ peep3 opt_dip3
    ; peephole @@ peep2 opt_dip2
    ; peephole @@ peep1 opt_dip1
    ; (peephole
      @@
      if experimental_disable_optimizations_for_debugging
      then No_change
      else peep1 opt_cond)
    ; peephole @@ peep2 opt_swap2
    ; peephole @@ peep3 opt_beta3
    ; peephole @@ peep5 opt_beta5
    ; peephole @@ peep1 opt_digdug1
    ; peephole @@ peep2 opt_digdug2
    ; peephole @@ peep2 opt_beta2
    ; peephole @@ peep2 opt_eta2
    ; peephole @@ peep3 opt_dead_unpair
    ; peephole @@ opt_digdug_cycles ()
    ; peephole @@ opt_unpair_car ()
    ; peephole @@ opt_unpair_cdr ()
    ; peephole @@ peep4 opt_unpair_edo
    ; peephole @@ peep1 opt_dup1
    ; peephole @@ peep3 opt_dupn_edo
    ; peephole @@ opt_pair2 ()
    ; peephole @@ opt_unpair2 ()
    ; peephole @@ opt_digdug_drop ()
    ; peephole @@ opt_get ()
    ; peephole @@ opt_push ()
    ; peephole @@ peep2 opt_neg_int
    ; peephole @@ peep3 opt_nat_int_neg
    ]
  in
  let optimizers = List.map ~f:on_seqs optimizers in
  let x = iterate_optimizer (sequence_optimizers optimizers) x in
  let x = opt_tail_fail x in
  (* round two *)
  let optimizers = [ peephole @@ peep1 opt_dup1 ] in
  let optimizers = List.map ~f:on_seqs optimizers in
  let x = iterate_optimizer (sequence_optimizers optimizers) x in
  let x = opt_combine_drops x in
  let x = opt_strip_annots x in
  let x = use_lambda_instr x in
  x


let rec optimize_with_types
    : type l.
      raise:_
      -> typer_oracle:
           (( l
            , Proto_alpha_utils.Memory_proto_alpha.Protocol.Michelson_v1_primitives.prim
            )
            node
            -> Proto_alpha_utils.Memory_proto_alpha.Protocol.Script_tc_errors.type_map
               Lwt.t)
      -> Environment.Protocols.t
      -> experimental_disable_optimizations_for_debugging:bool
      -> has_comment:(l -> bool)
      -> l michelson
      -> l michelson Lwt.t
  =
 fun ~raise
     ~typer_oracle
     proto
     ~experimental_disable_optimizations_for_debugging
     ~has_comment
     contract ->
  let open Lwt.Let_syntax in
  let node_string_of_canonical c =
    let c =
      Proto_alpha_utils.Memory_proto_alpha.Protocol.Michelson_v1_primitives
      .strings_of_prims
        c
    in
    Tezos_micheline.Micheline.inject_locations (fun x -> x) c
  in
  let canonical, locs = Tezos_micheline.Micheline.extract_locations contract in
  let recover_loc l = List.Assoc.find_exn locs ~equal:Int.equal l in
  let canonical =
    Proto_alpha_utils.Trace.trace_alpha_tzresult ~raise (fun _ ->
        failwith "Could not parse primitives from strings")
    @@ Proto_alpha_utils.Memory_proto_alpha.Protocol.Michelson_v1_primitives
       .prims_of_strings
         canonical
  in
  let%bind map =
    typer_oracle @@ Tezos_micheline.Micheline.inject_locations recover_loc canonical
  in
  let type_map =
    List.map
      ~f:(fun (i, (l, _)) -> i, List.map ~f:(fun c -> node_string_of_canonical c) l)
      map
  in
  match Tezos_micheline.Micheline.inject_locations (fun x -> x) canonical with
  | Seq (l, parameter :: storage :: code :: rest) ->
    let pre_type l = List.Assoc.find_exn type_map ~equal:Int.equal l in
    let code =
      Tezos_micheline.Micheline.map_node
        (fun x -> x)
        (fun v ->
          Proto_alpha_utils.Memory_proto_alpha.Protocol.Michelson_v1_primitives
          .string_of_prim
            v)
        code
    in
    let changed, code = on_seqs (peephole (peep1 @@ opt_cond ~pre_type)) code in
    let code =
      Tezos_micheline.Micheline.map_node (fun x -> recover_loc x) (fun x -> x) code
    in
    let code =
      if changed
      then
        optimize proto ~experimental_disable_optimizations_for_debugging ~has_comment code
      else code
    in
    let recover_locs node =
      Tezos_micheline.Micheline.map_node
        recover_loc
        (fun v ->
          Proto_alpha_utils.Memory_proto_alpha.Protocol.Michelson_v1_primitives
          .string_of_prim
            v)
        node
    in
    let contract =
      Seq
        ( recover_loc l
        , [ recover_locs parameter; recover_locs storage; code ]
          @ List.map ~f:recover_locs rest )
    in
    if changed
    then
      optimize_with_types
        ~raise
        ~typer_oracle
        proto
        ~experimental_disable_optimizations_for_debugging
        ~has_comment
        contract
    else Lwt.return contract
  | _ -> Lwt.return contract
