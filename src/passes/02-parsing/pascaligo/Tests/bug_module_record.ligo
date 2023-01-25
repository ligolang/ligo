type t1 is record [ a : int ; b : string ]
type t2 is record [ x : t1]

const v = record [ x = record [ a = 1 ; b = "b"] ]

module A is {
  const y = v ;
}

const tm = A.y.x
