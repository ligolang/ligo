type storage = unit;

/* Not supported yet:
   let main (p:unit) storage = (fun x -> ()) ()
   */

let main = ((a, s) : (unit, storage)) : unit =>
  ((f : (unit => unit)) => f ()) ((useless : unit) => unit);
