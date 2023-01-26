module PP_helpers = Simple_utils.PP_helpers
open Var

type 'expr t =
  { contract : Contract_var.t
  ; address : 'expr
  ; method_ : Value_var.t
  ; params : 'expr list
  ; on_none : 'expr option
  }
[@@deriving equal, compare, yojson, hash, fold, map]

let fold_map f acc { contract; address; method_; params; on_none } =
  let acc, address = f acc address in
  let acc, params = List.fold_map params ~init:acc ~f in
  let acc, on_none =
    match on_none with
    | None -> acc, None
    | Some on_none ->
      let acc, on_none = f acc on_none in
      acc, Some on_none
  in
  acc, { contract; address; method_; params; on_none }


let pp pp_expr ppf { contract; address; method_; params; on_none } =
  match on_none with
  | None ->
    Format.fprintf
      ppf
      "@[(contract ( %a : %a )).%a %a@]"
      pp_expr
      address
      Contract_var.pp
      contract
      Value_var.pp
      method_
      PP_helpers.(list_sep pp_expr (tag "@,"))
      params
  | Some on_none ->
    Format.fprintf
      ppf
      "@[<v>try (contract ( %a : %a )).%a %a@;with Failure -> %a@]"
      pp_expr
      address
      Contract_var.pp
      contract
      Value_var.pp
      method_
      PP_helpers.(list_sep pp_expr (tag "@,"))
      params
      pp_expr
      on_none
