type foo is record [ a :int ; b : nat ]
type bar is record [ c : int * nat ; d : foo ]
type baz is OneCase of int

const t1 : int = {
  var (a,b,c,d) := (1,2,3,4) ;
} with a

const t2 : string = {
  var (a,(b,c),(d,(e,f))) := (1,(2,3),(4,(5,"7"))) ;
} with f


const t3 = {
  var (a,(b,(c,(d,e,f)))) := (1,(2,(3n,(4,5,"7")))) ;
} with (a+b,c,f)

const t4 = {
  var (a,(b,(c,(d,(e,f))),g)) := (1,(1n,(1,(1n,(1,1n))),1)) ;
} with (a+c+e+g , b + d + f)

const t5 : nat = {
  var record [ a = a ; b = b ] := record [ a = 1 ; b = 1n ] ;
} with b

const t6 = {
  var x : foo :=  record [ a = 1 ; b = 2n ] ;
  var (record [ a = a ; b = b ],(c,d)) := (x,(1,1)) ;
} with (a + c + d, b)

const t7 = {
  var (record [ c = (a,b) ; d = record [ a = c ; b = d ] ]) := record [ c = ( 1 , 2n) ; d = record [ a = 1 ; b = 1n ] ] ;
} with ( a + c , b + d )

const t8 = {
  var (x, (y, record [ a = a ; b = b ] )) := (1, (1n, record [a = 1 ; b = 1n] )) ;
} with (x + a , y + b)

const t9 = {
  var (OneCase (av), record [ a ; b = _ ]) := (OneCase (1), record [ a = 1 ; b = 1n ]) ;
} with (av + a)
