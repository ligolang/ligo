open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location
open Ligo_prim.Literal_types
module Ligo_string = Simple_utils.Ligo_string
open Tezos_crypto
(* TODO: for decompilation, might be good to build a map Timestamp <-> (fun _ -> e_timestamp _) *)

let is_ty_var ty str =
  match get_t_var ty with
  | None -> false
  | Some v -> Ty_variable.is_name v str


let str_to_byte_opt x =
  try Some (Hex.to_bytes (`Hex x)) with
  | _ -> None


let compile ~raise =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_annot (ae, ty) as e ->
      (match get_e ae with
      | E_literal (Literal_string lit)
        when is_ty_var ty (to_string Key_hash)
             && Caml.(Ligo_string.get_type lit = `Standard) ->
        let s = Ligo_string.extract lit in
        let _ =
          trace_option
            ~raise
            (bad_format_literal ae s)
            (Signature.Public_key_hash.of_b58check_opt s)
        in
        e_key_hash ~loc s
      | E_literal (Literal_string lit)
        when is_ty_var ty (to_string Signature)
             && Caml.(Ligo_string.get_type lit = `Standard) ->
        let s = Ligo_string.extract lit in
        let _ =
          trace_option ~raise (bad_format_literal ae s) (Signature.of_b58check_opt s)
        in
        e_signature ~loc (Ligo_string.extract lit)
      | E_literal (Literal_string lit)
        when is_ty_var ty (to_string Key) && Caml.(Ligo_string.get_type lit = `Standard)
        ->
        let s = Ligo_string.extract lit in
        let _ =
          trace_option
            ~raise
            (bad_format_literal ae s)
            (Signature.Public_key.of_b58check_opt s)
        in
        e_key ~loc s
      | E_literal (Literal_string lit)
        when is_ty_var ty (to_string Timestamp)
             && Caml.(Ligo_string.get_type lit = `Standard) ->
        let open Tezos_base.TzPervasives.Time.Protocol in
        let str = Ligo_string.extract lit in
        let time = trace_option ~raise (bad_timestamp str ae) @@ of_notation str in
        let itime = Z.of_int64 @@ to_seconds time in
        e_timestamp_z ~loc itime
      | E_literal (Literal_int lit) when is_ty_var ty (to_string Timestamp) ->
        e_timestamp_z ~loc lit
      | E_literal (Literal_string lit)
        when is_ty_var ty (to_string Chain_id)
             && Caml.(Ligo_string.get_type lit = `Standard) ->
        e_chain_id ~loc (Ligo_string.extract lit)
      | E_literal (Literal_string lit)
        when is_ty_var ty (to_string Address)
             && Caml.(Ligo_string.get_type lit = `Standard) ->
        e_address ~loc (Ligo_string.extract lit)
      | E_literal (Literal_bytes lit) when is_ty_var ty (to_string Bls12_381_g1) ->
        e_bls12_381_g1 ~loc lit
      | E_literal (Literal_bytes lit) when is_ty_var ty (to_string Bls12_381_g2) ->
        e_bls12_381_g2 ~loc lit
      | E_literal (Literal_bytes lit) when is_ty_var ty (to_string Bls12_381_fr) ->
        e_bls12_381_fr ~loc lit
      | E_literal (Literal_string lit)
        when is_ty_var ty (to_string Bytes) && Caml.(Ligo_string.get_type lit = `Standard)
        ->
        let str = Ligo_string.extract lit in
        let b = trace_option ~raise (bad_conversion_bytes ae) (str_to_byte_opt str) in
        e_bytes ~loc b
      | _ -> make_e ~loc e)
    | E_raw_code { language; code } as e when String.equal language "bytes" ->
      (match get_e code with
      | E_literal (Literal_string lit) ->
        let str = Ligo_string.extract lit in
        e_bytes_string ~loc str
      | E_annot (ae, ty) ->
        (match get_e ae with
        | E_literal (Literal_string lit) ->
          let str = Ligo_string.extract lit in
          e_annot ~loc (e_bytes_string ~loc:(get_e_loc ae) str, ty)
        | _ -> make_e ~loc e)
      | _ -> make_e ~loc e)
    | e -> make_e ~loc e
  in
  `Cata { idle_cata_pass with expr }


let pass ~raise =
  morph
    ~name:__MODULE__
    ~compile:(compile ~raise)
    ~decompile:`None
    ~reduction_check:Iter.defaults
