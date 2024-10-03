open Tezos_micheline.Micheline

type meta = Mini_c.meta

type compiled_expression =
  { expr_ty : (meta, string) node
  ; expr : (meta, string) node
  }
