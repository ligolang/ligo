open Tezos_micheline.Micheline
module Location = Simple_utils.Location

type meta = Mini_c.meta

type compiled_expression =
  { expr_ty : (meta, string) node
  ; expr : (meta, string) node
  }
