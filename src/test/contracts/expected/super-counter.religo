type parameter = Increment(int) | Decrement(int);

type storage = int;

type return = (list(operation), storage);

let main = 
  ((action, store): (parameter, storage)): return => 
    {
      let store = 
        switch(action) {
        | Incrementn => store + n
        | Decrementn => store - n
        };
      ([] : list(operation), store)
    };
