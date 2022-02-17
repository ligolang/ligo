open Format
open Types

let operation ppf (o: bytes) : unit =
  fprintf ppf "%a" Hex.pp (Hex.of_bytes o)

let constant' ppf : constant' -> unit = pp_constant' ppf

let literal ppf (l : literal) =
  match l with
  | Literal_unit -> fprintf ppf "unit"
  | Literal_int z -> fprintf ppf "%a" Z.pp_print z
  | Literal_nat z -> fprintf ppf "+%a" Z.pp_print z
  | Literal_timestamp z -> fprintf ppf "+%a" Z.pp_print z
  | Literal_mutez z -> fprintf ppf "%amutez" Z.pp_print z
  | Literal_string s -> fprintf ppf "%a" Simple_utils.Ligo_string.pp s
  | Literal_bytes b -> fprintf ppf "0x%a" Hex.pp (Hex.of_bytes b)
  | Literal_address s -> fprintf ppf "@%S" s
  | Literal_operation o -> fprintf ppf "Operation(%a)" operation o
  | Literal_key s -> fprintf ppf "key %s" s
  | Literal_key_hash s -> fprintf ppf "key_hash %s" s
  | Literal_signature s -> fprintf ppf "Signature %s" s
  | Literal_chain_id s -> fprintf ppf "Chain_id %s" s
  | Literal_bls12_381_g1 b -> fprintf ppf "bls12_381_g1 0x%a" Hex.pp (Hex.of_bytes b)
  | Literal_bls12_381_g2 b -> fprintf ppf "bls12_381_g2 0x%a" Hex.pp (Hex.of_bytes b)
  | Literal_bls12_381_fr b -> fprintf ppf "bls12_381_fr 0x%a" Hex.pp (Hex.of_bytes b)
  | Literal_chest b -> fprintf ppf "chest 0x%a" Hex.pp (Hex.of_bytes b)
  | Literal_chest_key b -> fprintf ppf "chest_key 0x%a" Hex.pp (Hex.of_bytes b)
