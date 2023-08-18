type comb_two =
  [@layout tree]
  | [@annot anbfoo] Foo of int
  | [@annot anabar] Bar of string

type comb_three =
  [@layout tree]
  | [@annot ana] A of int
  | [@annot anb] B of string
  | [@annot anc] C of nat

type comb_five =
  [@layout tree]
  | [@annot an_One] One of int
  | [@annot an_Two] Two of string
  | [@annot an_Three] Three of bool
  | [@annot an_Four] Four of nat
  | [@annot an_Five] Five of int

type parameter = unit

type op_list = operation list

module Main_comb_two = struct
  let main () (store : comb_two) : op_list * comb_two =
    let o =
      match store with
        Foo _ -> Bar "foo"
      | Bar _ -> Foo 1 in
    ([] : operation list), o

end

module Main_comb_three = struct
  let main () (_ : comb_three) : op_list * comb_three =
    let o = (C 1n) in
    ([] : operation list), o

end

module Main_comb_five = struct
  let main () (store : comb_five) : op_list * comb_five =
    let o =
      match store with
        One _ -> Five (1)
      | Two _ -> Four (2n)
      | Three _ -> Three (true)
      | Four _ -> Two ("lol")
      | Five _ -> One 1 in
    ([] : operation list), o

end
