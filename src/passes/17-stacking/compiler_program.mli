open Stage_common.Types

open Tezos_micheline.Micheline

type meta = Mini_c.meta

type compiled_expression = {
  expr_ty : (meta, string) node ;
  expr : (meta, string) node ;
}

open Ligo_coq_ocaml
open Co_de_bruijn
open Ligo

val compile_expr: raise:Errors.stacking_error Simple_utils.Trace.raise -> Environment.Protocols.t -> (meta, string) node list -> splitting -> (meta, constant', literal) expr -> (meta, string) node

val compile_function_body : raise:Errors.stacking_error Simple_utils.Trace.raise -> Environment.Protocols.t -> (meta, constant', literal) binds -> (meta, string) node
