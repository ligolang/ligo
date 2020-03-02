type storage = unit;

/* not supported yet
   let%entry main (p:unit) storage =
     (fun x -> ()) ()
   */

let main = ((p,s) : (unit, storage)) => (((useless : unit)) => ()) ();
