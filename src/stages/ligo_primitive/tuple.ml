type 'a t = 'a list

let pp f ppf t =
  let open Simple_utils.PP_helpers in
  Format.fprintf ppf "@[<hv 2>( %a )@]" (list_sep f (tag " *@ ")) t
