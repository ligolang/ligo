let bts : bytes = Bytes.pack 42

let int_set : int set = Set.empty

let int_string_map : (int, string) map = Map.empty

let main (_, s : unit * int) : operation list * int =
  let s1 = Bytes.length bts in
  let s2 = Set.cardinal int_set in
  let s3 = Map.size int_string_map in
  (([] : operation list), s + s1 + s2 + s3)
