type comb_two =
  [@layout comb]
  {
   [@annot anbfoo] foo : int;
   [@annot anabar] bar : string
  }

type comb_three =
  [@layout comb]
  {
   [@annot ana] a : int;
   [@annot anb] b : string;
   [@annot anc] c : nat
  }

type comb_five =
  [@layout comb]
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
  let main (_ : parameter) (store : comb_two) : op_list * comb_two =
    let o = store.foo in
    let oo = {store with foo = o} in
    ([] : operation list), oo

end

module Main_comb_three = struct
  [@entry]
  let main (_ : parameter) (_ : comb_three) : op_list * comb_three =
    ([] : operation list),
    {
     a = 1;
     b = "";
     c = 1n
    }

end

module Main_comb_five = struct
  [@entry]
  let main (_ : parameter) (store : comb_five) : op_list * comb_five =
    ([] : operation list), store

end

let r : comb_five =
  {
   one = 1;
   two = "";
   three = true;
   four = 1n;
   five = 2
  }

let accesses = r.one, r.two, r.three, r.four, r.five
