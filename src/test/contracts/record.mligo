type foobar = {
 foo : int ;
 bar : int ;
}

let fb : foobar = {
  foo = 0 ;
  bar = 0 ;
}

type abc = {
  a : int ;
  b : int ;
  c : int
}

let abc : abc = {
  a = 42 ;
  b = 142 ;
  c = 242
}

let a : int = abc.a
let b : int = abc.b
let c : int = abc.c

let projection (r : foobar) : int = r.foo + r.bar

let modify (r : foobar) : foobar = {foo = 256; bar = r.bar}

let modify_abc (r : abc) : abc = {r with {b = 2048}}

type big_record = {
     a : int ;
     b : int ;
     c : int ;
     d : int ;
     e : int ;
}

let br : big_record = {
  a = 23 ;
  b = 23 ;
  c = 23 ;
  d = 23 ;
  e = 23 ;
}
