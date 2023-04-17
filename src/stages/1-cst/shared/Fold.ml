open Simple_utils.Function

type 'a fold_control = Stop | Skip | Continue of 'a | Last of 'a

type 'a monoid =
  { empty : 'a
  ; append : 'a -> 'a -> 'a
  }

let endo_monoid : ('a -> 'a) monoid =
  { empty = Fun.id
  ; append = (<@)
  }

let list_monoid : 'a list monoid =
  { empty = []
  ; append = (@)
  }

let int_monoid : int monoid =
  { empty = 0
  ; append = (+)
  }

let string_monoid : string monoid =
  { empty = ""
  ; append = (^)
  }
