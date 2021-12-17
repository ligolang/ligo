type action = SetName of string

type state = (int * 8 sapling_state)

type storage = {
  state: state;
  name: string;
}

let set_name (name, storage : string * storage) : operation list * storage = 
    let storage : storage = { state = storage.state; name = name } in
    ([] : operation list), storage

let main (action, storage: action * storage) : operation list * storage =
    match action with
    | SetName name -> set_name (name, storage)