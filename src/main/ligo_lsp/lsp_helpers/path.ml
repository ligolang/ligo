let normalise_backslashes p = p |> Str.global_replace (Str.regexp "[\\|/]+") "/"
let normalise_case = Caml.String.lowercase_ascii
let normalise p = p |> normalise_backslashes |> normalise_case
