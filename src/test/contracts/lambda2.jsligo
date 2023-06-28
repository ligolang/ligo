type storage = unit;

/* Not supported yet:
   let main (p:unit) storage = (fun x -> ()) ()
   */

let check = (_unit : unit, _storage : storage) : unit =>
  ((f : ((a: unit) => unit)) => f (unit)) ((_useless : unit) => unit);
