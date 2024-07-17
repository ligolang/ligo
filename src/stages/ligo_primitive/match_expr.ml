module Make (Pattern : Pattern.S) = struct
  type ('e, 't) match_case =
    { pattern : 't Pattern.t
    ; body : 'e
    }
  [@@deriving compare, eq, fold, hash, iter, map, sexp, yojson]

  type ('e, 't) t =
    { matchee : 'e
    ; cases : ('e, 't) match_case list
    }
  [@@deriving compare, eq, fold, hash, iter, map, sexp, yojson]

  let fold_map_match_case f g acc { pattern; body } =
    let acc, pattern = Pattern.fold_map g acc pattern in
    let acc, body = f acc body in
    acc, { pattern; body }


  let fold_map f g acc { matchee; cases } =
    let acc, matchee = f acc matchee in
    let acc, cases = List.fold_map ~init:acc ~f:(fold_map_match_case f g) cases in
    acc, { matchee; cases }


  let pp_match_case f g ppf { pattern; body } =
    Format.fprintf ppf "@[| %a -> %a@]" (Pattern.pp g) pattern f body


  let pp f g ppf { matchee; cases } =
    Format.fprintf
      ppf
      "@[<v 2> match %a with@,%a@]"
      f
      matchee
      Simple_utils.PP_helpers.(list_sep (pp_match_case f g) (tag "@ "))
      cases
end
