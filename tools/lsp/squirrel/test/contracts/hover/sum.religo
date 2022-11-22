type storage = int;

type parameter =
  Increment (int)
| Decrement (int)
| Reset;

type return = (list (operation), storage);

let main = ((action, store) : (parameter, storage)) : return => {
 (
    ([] : list (operation)),
    switch (action) {
      | Increment (n) => store + n
      | Decrement (n) => store + n
      | Reset         => 0
    }
  )
};
