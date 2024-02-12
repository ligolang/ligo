module Make (Pattern : Pattern.S) = struct
  type ('e, 't) match_case =
    { pattern : 't Pattern.t
    ; body : 'e
    }
  [@@deriving eq, compare, yojson, hash, fold, map, iter]

  type ('e, 't) t =
    { matchee : 'e
    ; disc_label : Label.t option
    ; cases : ('e, 't) match_case list
    }
  [@@deriving eq, compare, yojson, hash, fold, map]

  let pp_match_case f g ppf { pattern; body } =
    Format.fprintf ppf "@[| %a -> %a@]" (Pattern.pp g) pattern f body


  let pp f g ppf { matchee; disc_label; cases } =
    let pp_disc_label ppf = function
      | None -> Format.fprintf ppf ""
      | Some disc_label -> Label.pp ppf disc_label
    in
    Format.fprintf
      ppf
      "@[<v 2> match %a%a with@,%a@]"
      f
      matchee
      pp_disc_label
      disc_label
      Simple_utils.PP_helpers.(list_sep (pp_match_case f g) (tag "@ "))
      cases
end
