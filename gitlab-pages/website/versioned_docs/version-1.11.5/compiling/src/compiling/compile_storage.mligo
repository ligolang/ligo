module ComplexStorage = struct
  type my_record = {
    x: int;
    y: int;
    z: int;
  }
  type my_labels = {
    a: string;
    b: string;
    c: string;
  }
  type storage = (my_record * my_labels * address)
  type return_type = operation list * storage

  [@entry]
  let noop (_u : unit) (storage : storage) : return_type = [], storage

end