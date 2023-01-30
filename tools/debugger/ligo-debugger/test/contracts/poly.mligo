module TestId =
  struct
    module One =
      struct
        let id (type a) (x : a) : a = x
      end
  end

let main (_, s : unit * int) : operation list * int =
  let poly_troll42_ = 15 in
  let foo (type a) : a list = ([] : a list) in
  let int_list = 42 :: (foo : int list) in
  let add (a, b : int * int) = a + b in
  let sum_list = List.fold_left add 0 int_list in
  ((foo : operation list), s + sum_list + (TestId.One.id 42 : int) + poly_troll42_)
