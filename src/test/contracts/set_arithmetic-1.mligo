// Test set iteration

let aggregate (i : int) (j : int) : int = i + j

let fold_op (s : int set) : int =
  Set.fold s 15 aggregate
