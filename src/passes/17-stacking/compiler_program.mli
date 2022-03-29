open Stage_common.Types

open Tezos_micheline.Micheline

type meta = Mini_c.meta

type compiled_expression = {
  expr_ty : (meta, string) node ;
  expr : (meta, string) node ;
}

open Ligo_coq_ocaml.Compiler

(* TODO ugh *)
(* type ('meta, 'base_type, 'op, 'lit, 'static_args, 'micheline) expr *)

val compile_expr: (meta, (meta, string) Tezos_micheline.Micheline.node) ty list -> (meta, (meta, string) node, literal, (meta, string) node) expr -> (meta, string) node

val compile_function_body : (meta, (meta, string) node, literal, (meta, string) node) binds -> (meta, string) node
