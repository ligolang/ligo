module With_custom_condition = struct
  type ('cond, 'expr) t =
    { condition : 'cond
    ; then_body : 'expr
    ; else_body : 'expr
    }
  [@@deriving eq, compare, yojson, hash, fold, map]

  let filter_map_condition f t =
    let open Option.Let_syntax in
    let%bind condition = f t.condition in
    return { t with condition }


  let pp f_cond f_expr ppf { condition; then_body; else_body } =
    Format.fprintf
      ppf
      "if %a then %a else %a"
      f_cond
      condition
      f_expr
      then_body
      f_expr
      else_body
end

type 'expr t = ('expr, 'expr) With_custom_condition.t
[@@deriving eq, compare, yojson, hash, fold, map]

let pp f_expr = With_custom_condition.pp f_expr f_expr
