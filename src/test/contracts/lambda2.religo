type storage = unit;

/* Not supported yet:
   let main (p:unit) storage = (fun x -> ()) ()
   */

let main = (z: unit, storage) =>
  ((f: (unit => unit)) => f())((z: unit) => unit);
