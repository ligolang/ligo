type storage_ = ((int, int) big_map * unit)

let set_ (n : int) (m : storage_) : storage_ =
    (Map.update 23 (Some(n)) m.(0), ())

let rm (m : storage_) : storage_ =
    (Map.remove 42 m.(0), ())

let gf (m : storage_) : int = Map.find 23 m.(0)

let get (m: storage_): int option =
    Map.find_opt 42 m.(0)