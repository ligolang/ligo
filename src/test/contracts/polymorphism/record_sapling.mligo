type action = SetName of string

type state = (int * 8 sapling_state)

type storage =
  {
   state : state;
   name : string
  }

let set_name (name : string) (storage : storage) : operation list * storage =
  let storage : storage =
    {
     state = storage.state;
     name = name
    } in
  ([] : operation list), storage

[@entry]
let main (action : action) (storage : storage) : operation list * storage =
  match action with
  SetName name -> set_name name storage
