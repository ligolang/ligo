type storage = unit;

/* not supported yet
   let%entry main (p:unit) storage =
     (fun x -> ()) ()
   */

let main2 = ((p: unit), s: storage) => (((xxx: unit)) => ())();

let main = (x: (unit, storage)) => main2(x[0], x[1]);
