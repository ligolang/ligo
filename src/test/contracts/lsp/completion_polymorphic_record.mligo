type ('a, 'b, 'c, 'd) r1 = { x : 'c
                           ; z: int
                           ; y : 'a
                           ; c: ('a, 'b) map
                           ; d: 'd
                           }

let m1 (type a) : (int, string, int, a) r1 = { x = 1; z = 1; y = 1; c = Map.empty; d = failwith "" }

let h1 = m1.

type 'a r2 = { f1: 'a; f2: 'a }

let m2 (type a) : a r2 = { f1 = failwith ""; f2 = failwith "" }

let h2 = m2.

let m3: int r2 = { f1 = 1; f2 = 2 }

let h3 = m3.

type intr2 = int r2

let m4: intr2 = { f1 = 1; f2 = 2 }

let h4 = m4.
