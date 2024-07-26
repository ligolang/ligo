type complex_t = { a : int list option ; b : int list }

let complex = fun (x:complex_t) (y:complex_t) ->
  match (x,y) with
  | {a=None; b=_}, {a = _; b = _} -> -1
  | {a=_; b=_}, {a = Some ([]); b = (hd::tl)} -> hd
  | {a=_; b=_}, {a = Some (hd::tl); b = []} -> hd
  | {a=Some a; b=_}, _ -> int (List.length a)