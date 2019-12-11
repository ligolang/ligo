type root =
| A of a
| B of int
| C of string

and a = {
  a1 : ta1 ;
  a2 : ta2 ;
}

and ta1 =
| X of root
| Y of ta2

and ta2 =
| Z of ta2
