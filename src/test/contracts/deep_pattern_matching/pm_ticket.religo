type tr = { myt : ticket(int) , mynat : nat };
type parameter = ( tr , option(nat) );
type storage = nat;

let main = ((p,_s): (parameter, storage)) =>
  switch(p) {
  | { myt : _myt , mynat : mynat } , None     => ([]: list(operation), mynat)
  | { myt : _myt , mynat : mynat } , Some (x) => ([]: list(operation), x    )
  }