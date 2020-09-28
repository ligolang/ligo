let yojson_opt f opt = match opt with
  | None -> `Null
  | Some v -> f v