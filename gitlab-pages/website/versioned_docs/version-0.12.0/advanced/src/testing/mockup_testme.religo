// This is mockup_testme.religo
type storage = string;

type parameter =
  Append (string)

type return = (list (operation), storage);

let main = ((action, store) : (parameter, storage)) : return => {
 (([] : list (operation)),    // No operations
 (switch (action) {
  | Append (s) => store ++ s
  }))
};
