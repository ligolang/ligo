type storage = int;

type parameter = Increment(int) | Decrement(int);

let add = ((a, b): (int, int)): int => a + b;

let sub = ((a, b): (int, int)): int => a - b;

let main = 
  ((p, storage): (parameter, storage)) => 
    {
      let storage = 
        switch(p) {
        | Incrementn => add((storage, n))
        | Decrementn => sub((storage, n))
        };
      ([] : list(operation), storage)
    };
