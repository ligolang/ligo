type storage =
  { a : int
  ; b : string
  ; c : bool
  }

let defStorage : storage = { a = 42 ; b = "str" ; c = False }

[@entry]
let main () (s : storage) : operation list * storage =
  let newStorage =
    match Some () with
    | Some _ -> { s with c = True }
    | None -> s
  in
  (([] : operation list), newStorage)
