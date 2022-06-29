type storage =
  { a: int
  ; b: nat
  ; c: string
  }

let defStorage(s: string): storage =
  { a = 0
  ; b = 0n
  ; c = String.concat s "!"
  }

let defEmptyStorage = defStorage ""

let main(_, s: unit * storage) =
    (([] : operation list), s)
