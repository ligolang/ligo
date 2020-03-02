type storage = unit;

/* Not supported yet:
   let main (p:unit) storage = (fun x -> ()) ()
   */

let main = ((a, s) : (unit, storage)) : (operation (list), storage) =>
  ((f : (unit => unit)) => f ()) ((useless : unit) => unit);
