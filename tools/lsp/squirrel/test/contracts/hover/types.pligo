type test is
  | Alt1 of int -> string
  | Alt2
  | Alt3 of
      record [
        field1 : int;
        field2 : int * int -> (Join of int | Empty)
      ]

function main (const p : unit; const _ : unit) is
  ((list [] : list (operation)), p)
