type storage = unit;

let main = (p: unit, storage) =>
  if (true) {
    failwith("This contract always fails");
  } else {
    ();
  };
