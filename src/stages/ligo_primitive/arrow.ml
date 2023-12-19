type 'a t =
  { type1 : 'a
  ; type2 : 'a
  ; param_names : string list
  }
[@@deriving eq, compare, yojson, hash, fold, map, sexp]

let pp g ppf { type1; type2; param_names } =
  let pp_param_names ppf param_names =
    if not @@ List.is_empty param_names
    then
      Format.fprintf
        ppf
        "[%a]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf _ -> Format.fprintf ppf ", ")
           Format.pp_print_string)
        param_names
  in
  Format.fprintf ppf "%a%a -> %a" pp_param_names param_names g type1 g type2
