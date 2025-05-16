module Counter = struct
  type storage_type = int
  type return_type = operation list * storage_type

  [@entry]
  (* The type of the value parameter is assumed to be an int *)
  let add (value) (storage: storage_type) : return_type =
    [], storage + value

  [@entry]
  (* The type of the value parameter is assumed to be an int *)
  let sub (value) (storage: storage_type) : return_type =
    [], storage - value

end