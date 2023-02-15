type sum =
  | ChoiceA
  | ChoiceB

let fold_test (input : sum) : bool =
  match input with
  | ChoiceA -> false
  | ChoiceB -> true

type record =
  { fieldA : int
  ; fieldB : unit
  }

let value : record =
  { fieldA = 0
  ; fieldB = ()
  }
