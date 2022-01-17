type storage = int;
type action = Increment(nat) | Decrement(nat);

let main = ((p,s): (action, storage)) =>
    let stor =
        switch(p) { 
        | Increment(n) => s + 1
        | Decrement    => s - 1
        };
    ([] : list(operation), stor)