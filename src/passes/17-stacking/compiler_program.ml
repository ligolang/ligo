open Stage_common.Types

open Tezos_micheline.Micheline
module Location = Simple_utils.Location

type compiled_expression = {
  expr_ty : (Location.t, string) node ;
  expr : (Location.t, string) node ;
}

include Ligo_coq_ocaml.Compiler
open Ligo_coq_ocaml.Ligo

let wipe_locations l e =
  inject_locations (fun _ -> l) (strip_locations e)

let generated = Location.generated

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

let literal_type (l : literal) : (Location.t, string) node =
  Prim (generated, literal_type_prim l, [], [])

let literal_value (l : literal) : (Location.t, string) node =
  match l with
  | Literal_unit -> Prim (generated, "Unit", [], [])
  | Literal_int x -> Int (generated, x)
  | Literal_nat x -> Int (generated, x)
  | Literal_timestamp x -> Int (generated, x)
  | Literal_mutez x -> Int (generated, x)
  | Literal_string x -> String (generated, Simple_utils.Ligo_string.extract x)
  | Literal_bytes x -> Bytes (generated, x)
  | Literal_address x -> String (generated, x)
  | Literal_signature x -> String (generated, x)
  | Literal_key x -> String (generated, x)
  | Literal_key_hash x -> String (generated, x)
  | Literal_chain_id x -> String (generated, x)
  | Literal_operation x -> Bytes (generated, x)
  | Literal_bls12_381_g1 x -> Bytes (generated, x)
  | Literal_bls12_381_g2 x -> Bytes (generated, x)
  | Literal_bls12_381_fr x -> Bytes (generated, x)

let compile_binds' = compile_binds
let compile_expr' = compile_expr

open Ligo_coq_ocaml.Micheline_wrapper

let rec compile_binds ~raise protocol_version env outer proj binds =
  List.map ~f:forward
    (compile_binds'
       generated
       (fun x y z ->
          List.map ~f:backward
            (compile_operator ~raise protocol_version x y z))
       (fun x -> backward (literal_type x))
       (fun x -> backward (literal_value x))
       env outer proj binds)

and compile_expr ~raise protocol_version env outer expr =
  List.map ~f:forward
    (compile_expr'
       generated
       (fun x y z ->
          List.map ~f:backward
            (compile_operator ~raise protocol_version x y z))
       (fun x -> backward (literal_type x))
       (fun x -> backward (literal_value x))
       (List.map ~f:backward env)
       outer
       expr)
       
and apply_static_args ~raise : Environment.Protocols.t -> string -> (_, constant', literal) static_args -> _ node =
  fun protocol_version prim args ->
  match args with
  | Type_args (annot, types) ->
    Prim (generated, prim, List.map ~f:forward types, Option.to_list annot)
  | Script_arg (Script (p, s, e)) ->
    (* prim will always be CREATE_CONTRACT, recursively compile the
       contract here *)
    let e = compile_binds ~raise protocol_version [] [] [] e in
    let parameter = Prim (generated, "parameter", [forward p], []) in
    let storage = Prim (generated, "storage", [forward s], []) in
    let code = Prim (generated, "code", [Seq (generated, e)], []) in
    Prim (generated, prim, [Seq (generated, [parameter; storage; code])], [])

and compile_operator ~raise : Environment.Protocols.t -> Location.t -> constant' -> (_, constant', literal) static_args -> (Location.t, string) node list =
  fun protocol_version loc c args ->
  match Predefined.Stacking.get_operators protocol_version c with
  | Some x -> [(* Handle predefined (and possibly special)
                  operators, applying any type/annot/script args
                  using apply_static_args. *)
               (Predefined.Stacking.unpredicate
                  loc
                  (fun prim -> wipe_locations () (apply_static_args ~raise protocol_version prim args))
                  x)]
  | None ->
    let open Simple_utils.Trace in
    (raise.raise) (Errors.unsupported_primitive c protocol_version)

let compile_expr ~raise protocol_version env outer e =
  Seq (generated, compile_expr ~raise protocol_version env outer e)

let compile_function_body ~raise protocol_version e =
  Seq (generated, compile_binds ~raise protocol_version [] [] [] e)
