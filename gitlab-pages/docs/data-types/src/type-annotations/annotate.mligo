let myInteger : int = 5
let myString : string = "Hello"
let myList : int list = [1; 2; 3]
let myMap : (string, int) map =
  Map.literal [
    ("one", 1);
    ("two", 2);
  ]
module Counter = struct
  type storage_type = int
  type return_type = operation list * storage_type

  [@entry]
  let add (value : int) (storage: storage_type) : return_type =
    [], storage + value

  [@entry]
  let sub (value : int) (storage: storage_type) : return_type =
    [], storage - value

end