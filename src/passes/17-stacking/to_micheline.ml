module List = Core.List
module Location = Simple_utils.Location
open Tezos_micheline.Micheline
module Compiler = Ligo_coq_ocaml.Compiler
module Datatypes = Ligo_coq_ocaml.Datatypes

open Stage_common.Types

type meta = Mini_c.meta
let null = Mini_c.dummy_meta

let int_to_mich (x : int) : (meta, string) node =
  Int (null, Z.of_int x)

let nat_to_mich : Datatypes.nat -> (meta, string) node =
  let rec aux (x : Z.t) (n : Datatypes.nat) : (meta, string) node =
    match n with
    | O -> Int (null, x)
    | S n -> aux (Z.add Z.one x) n in
  aux Z.zero

let smaller m1 m2 =
  let open Self_michelson in
  let measure mich =
    Bytes.length (Proto_alpha_utils.Memory_proto_alpha.to_bytes mich) in
  let optimize = optimize ~has_comment:(fun _ -> false) Environment.Protocols.current in
  if measure (optimize (Seq (null, m1)))
     <= measure (optimize (Seq (null, m2)))
  then m1
  else m2

(* Should port these to Coq and prove them... and/or just eliminate
   FUNC in the Coq compiler *)
let compile_dups1 (s : bool list) : _ node list =
  let rec aux n s =
    match s with
    | [] -> []
    | false :: s -> aux (n - 1) s
    | true :: s -> [Prim (null, "DIG", [int_to_mich (n - 1)], []);
                    Prim (null, "DUP", [], []);
                    Prim (null, "DUG", [int_to_mich n], [])]
                   @ aux n s in
  aux (List.length s) (List.rev s)

let compile_dups2 (s : bool list) : _ node list =
  let rec aux i n s =
    match s with
    | [] -> []
    | false :: s -> [Prim (null, "DIG", [int_to_mich i], []);
                     Prim (null, "DUG", [int_to_mich (n - 1)], [])]
                    @ aux (i - 1) (n - 1) s
    | true :: s -> [Prim (null, "DIG", [int_to_mich i], []);
                    Prim (null, "DUP", [], []);
                    Prim (null, "DUG", [int_to_mich n], []);
                    Prim (null, "DUG", [int_to_mich i], [])]
                   @ aux (i - 1) n s in
  aux (List.length s - 1) (List.length s) (List.rev s)

let compile_dups (s : bool list) : _ node list =
  smaller (compile_dups1 s) (compile_dups2 s)

let literal_type_prim (l : literal) : string =
  match l with
  | Literal_unit -> "unit"
  | Literal_int _ -> "int"
  | Literal_nat _ -> "nat"
  | Literal_timestamp _ -> "timestamp"
  | Literal_mutez _ -> "mutez"
  | Literal_string _ -> "string"
  | Literal_bytes _ -> "bytes"
  | Literal_address _ -> "address"
  | Literal_signature _ -> "signature"
  | Literal_key _ -> "key"
  | Literal_key_hash _ -> "key_hash"
  | Literal_chain_id _ -> "chain_id"
  | Literal_operation _ -> "operation"
  | Literal_bls12_381_g1 _ -> "bls12_381_g1"
  | Literal_bls12_381_g2 _ -> "bls12_381_g2"
  | Literal_bls12_381_fr _ -> "bls12_381_fr"
  | Literal_chest _ -> "chest"
  | Literal_chest_key _ -> "chest_key"

let literal_type (l : literal) : (meta, string) node =
  Prim (null, literal_type_prim l, [], [])

let literal_value (l : literal) : (meta, string) node =
  match l with
  | Literal_unit -> Prim (null, "Unit", [], [])
  | Literal_int x -> Int (null, x)
  | Literal_nat x -> Int (null, x)
  | Literal_timestamp x -> Int (null, x)
  | Literal_mutez x -> Int (null, x)
  | Literal_string x -> String (null, Simple_utils.Ligo_string.extract x)
  | Literal_bytes x -> Bytes (null, x)
  | Literal_address x -> String (null, x)
  | Literal_signature x -> String (null, x)
  | Literal_key x -> String (null, x)
  | Literal_key_hash x -> String (null, x)
  | Literal_chain_id x -> String (null, x)
  | Literal_operation x -> Bytes (null, x)
  | Literal_bls12_381_g1 x -> Bytes (null, x)
  | Literal_bls12_381_g2 x -> Bytes (null, x)
  | Literal_bls12_381_fr x -> Bytes (null, x)
  | Literal_chest x -> Bytes (null, x)
  | Literal_chest_key x -> Bytes (null, x)

let literal_code (meta : meta) (l : literal) : (meta, string) node list =
  [Prim (meta, "PUSH", [literal_type l; literal_value l], [])]

let global_constant (meta : meta) (hash : string) : (meta, string) node list =
  [Prim (meta, "constant", [String (null, hash)], [])]

let annotate (ann : string option) (x : ('meta, string) node) : ('meta, string) node =
  match ann with
  | None -> x
  | Some ann ->
    match x with
    | Prim (l, p, args, anns) ->
      Prim (l, p, args, ("%"^ann) :: anns)
    | x -> x

let rec translate_type (t : ('l, ('l, 'p) node) Compiler.ty) : ('l, 'p) node =
  match t with
  | T_base (_l, b) ->
    b
  (* func is a fictional version of lambda *)
  | T_func (l, a1, a2) | T_lambda (l, a1, a2) ->
    Prim (l, "lambda", [translate_type a1; translate_type a2], [])
  | T_unit l ->
    Prim (l, "unit", [], [])
  | T_pair (l, n1, n2, a1, a2) ->
    Prim (l, "pair", [annotate n1 (translate_type a1); annotate n2 (translate_type a2)], [])
  | T_or (l, n1, n2, a1, a2) ->
    Prim (l, "or", [annotate n1 (translate_type a1); annotate n2 (translate_type a2)], [])
  | T_option (l, a) ->
    Prim (l, "option", [translate_type a], [])
  | T_list (l, a) ->
    Prim (l, "list", [translate_type a], [])
  | T_set (l, a) ->
    Prim (l, "set", [translate_type a], [])
  | T_map (l, a1, a2) ->
    Prim (l, "map", [translate_type a1; translate_type a2], [])
  | T_big_map (l, a1, a2) ->
    Prim (l, "big_map", [translate_type a1; translate_type a2], [])
  | T_ticket (l, a1) ->
    Prim (l, "ticket", [translate_type a1], [])
  | T_contract (l, a1) ->
    Prim (l, "contract", [translate_type a1], [])
  | T_bool l ->
    Prim (l, "bool", [], [])
  | T_int l ->
    Prim (l, "int", [], [])
  | T_nat l ->
    Prim (l, "nat", [], [])
  | T_mutez l ->
    Prim (l, "mutez", [], [])
  | T_string l ->
    Prim (l, "string", [], [])
  | T_bytes l ->
    Prim (l, "bytes", [], [])

  | T_operation l ->
    Prim (l, "operation", [], [])
  | T_address l ->
    Prim (l, "address", [], [])
  | T_key_hash l ->
    Prim (l, "key_hash", [], [])

let has_field_annot = function
  | Prim (_, _, _, anns) ->
    List.exists anns ~f:(fun ann -> String.length ann > 1 && String.equal "%" (String.sub ann ~pos:0 ~len:1))
  | _ -> false

(* Replace [pair (a %x) (pair (b %y) (c %z))] with [pair (a %x) (b %y) (c %z)]. *)
let rec tuplify_pair_types t =
  match t with
  | Prim (l, "pair", args, ann) ->
    (match List.rev (List.map ~f:tuplify_pair_types args) with
    | [] -> t (* impossible *)
    | last_arg :: rev_args ->
      match last_arg with
      | Prim (_, "pair", last_arg_args, _) when not (has_field_annot last_arg) ->
        let args = List.rev rev_args @ last_arg_args in
        Prim (l, "pair", args, ann)
      | _ ->
        let args = List.rev (last_arg :: rev_args) in
        Prim (l, "pair", args, ann))
  | Prim (l, p, args, ann) ->
    Prim (l, p, List.map ~f:tuplify_pair_types args, ann)
  | Seq (l, args) ->
    Seq (l, List.map ~f:tuplify_pair_types args)
  | t -> t

let translate_type t = tuplify_pair_types (translate_type t)

let translate_tuple = function
  | [] -> Prim (null, "unit", [], [])
  | [a] -> translate_type a
  | az -> Prim (null, "pair", List.map ~f:translate_type az, [])

let unpair_tuple = function
  | [] -> [Prim (null, "DROP", [], [])]
  | [_a] -> []
  | az -> [Prim (null, "UNPAIR", [int_to_mich (List.length az)], [])]

let pair_tuple = function
  | [] -> [Prim (null, "UNIT", [], [])]
  | [_a] -> []
  | az -> [Prim (null, "PAIR", [int_to_mich (List.length az)], [])]

let rec translate_instr (instr : (meta, (meta, string) node, (meta, string) node) Compiler.instr) :
  (meta, string) node list =
  match instr with
  (* TODO... *)
  | I_FUNC (l, cs, a, b, proj1, proj2, body) ->
    let weight p = List.length (List.filter ~f:ident p) in
    let n = List.length cs in
    if n = 0
    then
      [Prim (l, "LAMBDA", [translate_type a;
                           translate_type b;
                           Seq (null, translate_prog body)], [])]
    else
      let capture = translate_tuple cs in
      [Prim (l, "LAMBDA", [Prim (null, "pair", [capture; translate_type a], []);
                           translate_type b;
                           Seq (null, [Prim (null, "UNPAIR", [], [])]
                                      @ unpair_tuple cs
                                      @ [Prim (null, "DIG", [int_to_mich n], [])]
                                      @ translate_prog body
                                      @ (if weight proj2 = 0
                                         then []
                                         else [Prim (null, "DIP", [Seq (null, [Prim (null, "DROP", [int_to_mich (weight proj2)], [])])], [])]))], [])]
      @ compile_dups (false :: proj1)
      @ pair_tuple cs
      @ [Prim (null, "APPLY", [], [])]
  | I_LAMBDA (l, a, b, body) ->
    [Prim (l, "LAMBDA", [translate_type a;
                         translate_type b;
                         Seq (null, translate_prog body)], [])]
  | I_APPLY_LAMBDA (l, _ty) -> [Prim (l, "APPLY", [], [])]
  | I_RAW (_l, _n, raw) -> raw

  | I_SEQ (l, p) -> [Seq (l, translate_prog p)]
  | I_DIP (l, p) -> [Prim (l, "DIP", [Seq (null, translate_prog p)], [])]
  | I_DIG (l, n) -> [Prim (l, "DIG", [nat_to_mich n], [])]
  | I_DUG (l, n) -> [Prim (l, "DUG", [nat_to_mich n], [])]
  | I_DUP (l, n) -> [Prim (l, "DUP", [nat_to_mich n], [])]
  | I_DROP (l, n) -> [Prim (l, "DROP", [nat_to_mich n], [])]
  | I_SWAP l -> [Prim (l, "SWAP", [], [])]
  | I_UNIT l -> [Prim (l, "UNIT", [], [])]
  | I_LEFT (l, a) -> [Prim (l, "LEFT", [translate_type a], [])]
  | I_RIGHT (l, b) -> [Prim (l, "RIGHT", [translate_type b], [])]
  | I_IF_LEFT (l, bt, bf) -> [Prim (l, "IF_LEFT", [Seq (null, translate_prog bt); Seq (null, translate_prog bf)], [])]
  | I_PAIR (l, n) -> [Prim (l, "PAIR", [nat_to_mich n], [])]
  | I_UNPAIR (l, n) -> [Prim (l, "UNPAIR", [nat_to_mich n], [])]
  | I_GET (l, n) -> [Prim (l, "GET", [nat_to_mich n], [])]
  | I_UPDATE (l, n) -> [Prim (l, "UPDATE", [nat_to_mich n], [])]
  | I_CAR l -> [Prim (l, "CAR", [], [])]
  | I_CDR l -> [Prim (l, "CDR", [], [])]
  | I_IF (l, bt, bf) -> [Prim (l, "IF", [Seq (null, translate_prog bt); Seq (null, translate_prog bf)], [])]
  | I_IF_NONE (l, bt, bf) -> [Prim (l, "IF_NONE", [Seq (null, translate_prog bt); Seq (null, translate_prog bf)], [])]
  | I_NIL (l, a) -> [Prim (l, "NIL", [translate_type a], [])]
  | I_CONS l -> [Prim (l, "CONS", [], [])]
  | I_IF_CONS (l, bt, bf) -> [Prim (l, "IF_CONS", [Seq (null, translate_prog bt); Seq (null, translate_prog bf)], [])]
  | I_EXEC l -> [Prim (l, "EXEC", [], [])]
  | I_LOOP (l, body) -> [Prim (l, "LOOP", [Seq (null, translate_prog body)], [])]
  | I_LOOP_LEFT (l, body) -> [Prim (l, "LOOP_LEFT", [Seq (null, translate_prog body)], [])]
  (* | I_PUSH (l, a, x) -> failwith "TODO" *)
  | I_FAILWITH l -> [Prim (l, "FAILWITH", [], [])]
  | I_ITER (l, body) -> [Prim (l, "ITER", [Seq (null, translate_prog body)], [])]
  | I_MAP (l, body) -> [Prim (l, "MAP", [Seq (null, translate_prog body)], [])]
  | I_CREATE_CONTRACT (l, p, s, script) -> [Prim (l, "CREATE_CONTRACT", [Seq (null, [Prim (null, "parameter", [translate_type p], []);
                                                                                     Prim (null, "storage", [translate_type s], []);
                                                                                     Prim (null, "code", [Seq (null, translate_prog script)], [])])], [])]

and translate_prog = List.concat_map ~f:translate_instr

(* strengthening of metadata (select the part of the env info which is
   still relevant after strengthening) *)
let strengthen_meta (r : bool list) (m : meta) =
  { m with env = Ligo_coq_ocaml.Ope.select r m.env }

let strengthen_prog prog embedding =
  let prog1 = prog in
  let (us, prog2) = Compiler.strengthen_prog
    null
    (* hmm, I believe these are not used by strengthen_prog. they can
       probably be removed somehow from the extraction? *)
    (fun _ _ -> failwith "TODO")
    (fun _ _ -> failwith "TODO")
    (fun _ _ -> failwith "TODO")
    strengthen_meta
    prog1
    embedding in
  (us, prog2)
