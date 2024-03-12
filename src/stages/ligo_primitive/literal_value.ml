module Z = Simple_utils.Z

type ligo_string = Simple_utils.Ligo_string.t [@@deriving eq, compare, yojson, hash, sexp]

let bytes_to_yojson b = `String (Bytes.to_string b)
let hash_fold_bytes st b = Hash.fold_string st (Bytes.to_string b)

type t =
  | Literal_unit
  | Literal_int of Z.t
  | Literal_nat of Z.t
  | Literal_timestamp of Z.t
  | Literal_mutez of Z.t
  | Literal_string of ligo_string
  | Literal_bytes of bytes
  | Literal_address of string
  | Literal_signature of string
  | Literal_key of string
  | Literal_key_hash of string
  | Literal_chain_id of string
  | Literal_operation of bytes
  | Literal_bls12_381_g1 of bytes
  | Literal_bls12_381_g2 of bytes
  | Literal_bls12_381_fr of bytes
  | Literal_chest of bytes
  | Literal_chest_key of bytes
[@@deriving eq, compare, yojson, hash, sexp]

let pp_operation ppf (o : bytes) : unit = Format.fprintf ppf "%a" Hex.pp (Hex.of_bytes o)

let pp ppf (l : t) =
  let open Format in
  match l with
  | Literal_unit -> fprintf ppf "unit"
  | Literal_int z -> fprintf ppf "%a" Z.pp_print z
  | Literal_nat z -> fprintf ppf "+%a" Z.pp_print z
  | Literal_timestamp z -> fprintf ppf "+%a" Z.pp_print z
  | Literal_mutez z -> fprintf ppf "%amutez" Z.pp_print z
  | Literal_string s -> fprintf ppf "%a" Simple_utils.Ligo_string.pp s
  | Literal_bytes b -> fprintf ppf "0x%a" Hex.pp (Hex.of_bytes b)
  | Literal_address s -> fprintf ppf "@%S" s
  | Literal_operation o -> fprintf ppf "Operation(%a)" pp_operation o
  | Literal_key s -> fprintf ppf "key %s" s
  | Literal_key_hash s -> fprintf ppf "key_hash %s" s
  | Literal_signature s -> fprintf ppf "Signature %s" s
  | Literal_chain_id s -> fprintf ppf "Chain_id %s" s
  | Literal_bls12_381_g1 b -> fprintf ppf "bls12_381_g1 0x%a" Hex.pp (Hex.of_bytes b)
  | Literal_bls12_381_g2 b -> fprintf ppf "bls12_381_g2 0x%a" Hex.pp (Hex.of_bytes b)
  | Literal_bls12_381_fr b -> fprintf ppf "bls12_381_fr 0x%a" Hex.pp (Hex.of_bytes b)
  | Literal_chest b -> fprintf ppf "chest 0x%a" Hex.pp (Hex.of_bytes b)
  | Literal_chest_key b -> fprintf ppf "chest_key 0x%a" Hex.pp (Hex.of_bytes b)


let assert_eq (a, b) = if equal a b then Some () else None

let typeof lit =
  match lit with
  | Literal_unit -> Literal_types.unit
  | Literal_string _ -> Literal_types.string
  | Literal_key _ -> Literal_types.key
  | Literal_key_hash _ -> Literal_types.key_hash
  | Literal_chain_id _ -> Literal_types.chain_id
  | Literal_signature _ -> Literal_types.signature
  | Literal_bytes _ -> Literal_types.bytes
  | Literal_int _ -> Literal_types.int
  | Literal_nat _ -> Literal_types.nat
  | Literal_timestamp _ -> Literal_types.timestamp
  | Literal_mutez _ -> Literal_types.tez
  | Literal_address _ -> Literal_types.address
  | Literal_operation _ -> Literal_types.operation
  | Literal_bls12_381_g1 _ -> Literal_types.bls12_381_g1
  | Literal_bls12_381_g2 _ -> Literal_types.bls12_381_g2
  | Literal_bls12_381_fr _ -> Literal_types.bls12_381_fr
  | Literal_chest _ -> Literal_types.chest
  | Literal_chest_key _ -> Literal_types.chest_key
