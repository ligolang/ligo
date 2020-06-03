type storage = int;

type parameter = Add(int) | Sub(int);

type return = (list(operation), storage);

let main = 
  ((action, store): (parameter, storage)) => 
    {
      let store = 
        store
        + (switch(action) {
           | Addn => n
           | Subn => -n
           });
      (([] : list(operation)), store)
    };
