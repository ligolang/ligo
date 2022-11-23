module Var = Simple_utils.Var
module Ligo_string = Simple_utils.Ligo_string
open Errors
open Ast_imperative
open Simple_utils.Trace
open Ligo_prim
open Literal_types

let peephole_expression ~raise : expression -> expression =
 fun e ->
  let return expression_content = { e with expression_content } in
  match e.expression_content with
  | E_ascription { anno_expr = e'; type_annotation = t } as ec ->
    (match e'.expression_content, t.type_content with
    | E_literal (Literal_string s), T_variable tv when Type_var.equal tv v_key_hash ->
      return @@ E_literal (Literal_key_hash (Ligo_string.extract s))
    | E_literal (Literal_string s), T_variable tv when Type_var.equal tv v_signature ->
      return @@ E_literal (Literal_signature (Ligo_string.extract s))
    | E_literal (Literal_string s), T_variable tv when Type_var.equal tv v_key ->
      return @@ E_literal (Literal_key (Ligo_string.extract s))
    | E_literal (Literal_int i), T_variable tv when Type_var.equal tv v_timestamp ->
      return @@ E_literal (Literal_timestamp i)
    | E_literal (Literal_string str), T_variable tv when Type_var.equal tv v_timestamp ->
      let open Tezos_base.TzPervasives.Time.Protocol in
      let str = Ligo_string.extract str in
      let time = trace_option ~raise (bad_timestamp str e') @@ of_notation str in
      let itime = Z.of_int64 @@ to_seconds time in
      return @@ E_literal (Literal_timestamp itime)
    | E_literal (Literal_string str), T_variable tv when Type_var.equal tv v_chain_id ->
      return @@ E_literal (Literal_chain_id (Ligo_string.extract str))
    | E_literal (Literal_string str), T_variable tv when Type_var.equal tv v_address ->
      return @@ E_literal (Literal_address (Ligo_string.extract str))
    | E_literal (Literal_bytes b), T_variable tv when Type_var.equal tv v_bls12_381_g1 ->
      return @@ E_literal (Literal_bls12_381_g1 b)
    | E_literal (Literal_bytes b), T_variable tv when Type_var.equal tv v_bls12_381_g2 ->
      return @@ E_literal (Literal_bls12_381_g2 b)
    | E_literal (Literal_bytes b), T_variable tv when Type_var.equal tv v_bls12_381_fr ->
      return @@ E_literal (Literal_bls12_381_fr b)
    | E_literal (Literal_string str), T_variable tv when Type_var.equal tv v_bytes ->
      let str = Ligo_string.extract str in
      let e' = trace_option ~raise (bad_conversion_bytes e) @@ e'_bytes str in
      return e'
    | _ -> return ec)
  | E_raw_code { language; code = { expression_content = E_literal (Literal_string code); _ } }
    when String.equal language "bytes" ->
    let str = Simple_utils.Ligo_string.extract code in
    let e' = e_bytes_string str in
    return e'.expression_content
  | E_raw_code
      { language
      ; code =
          { expression_content =
              E_ascription
                { anno_expr = { expression_content = E_literal (Literal_string code); location = lcode }
                ; type_annotation
                }
          ; _
          }
      }
    when String.equal language "bytes" ->
    let str = Simple_utils.Ligo_string.extract code in
    e_ascription ~loc:e.location { anno_expr = e_bytes_string ~loc:lcode str; type_annotation } ()
  | e -> return e
