type comb_two =
  [@layout tree]
  {
   [@annot anfoo] foo : int;
   [@annot anbar] bar : string
  }

type comb_three =
  [@layout tree]
  {
   [@annot ana] a : int;
   [@annot anb] b : string;
   [@annot anc] c : nat
  }

type comb_five =
  [@layout tree]
  {
   [@annot an_One] one : int;
   [@annot an_Two] two : string;
   [@annot an_Three] three : bool;
   [@annot an_Four] four : nat;
   [@annot an_Five] five : int
  }

type parameter = unit

type op_list = operation list

module Main_comb_two = struct
  [@entry]
  let main () (store : comb_two) : op_list * comb_two =
    let o = store.foo in
    let oo = {store with foo = o} in
    ([] : operation list), oo

end

module Main_comb_three = struct
  [@entry]
  let main () (_ : comb_three) : op_list * comb_three =
    ([] : operation list),
    {
     a = 1;
     b = "";
     c = 1n
    }

end

module Main_comb_five = struct
  [@entry]
  let main () (store : comb_five) : op_list * comb_five =
    ([] : operation list), store

end
