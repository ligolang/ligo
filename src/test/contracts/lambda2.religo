type storage = unit;

/* Not supported yet:
   let main (p:unit) storage = (fun x -> ()) ()
   */

let main2 = (z: unit, s: storage) =>
  ((f: (unit => unit)) => f())((z: unit) => unit);

let main = (x: (unit, storage)) => main2(x[0],x[1]);
