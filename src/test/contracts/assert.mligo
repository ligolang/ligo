let main (p : bool) (s : unit) =
  let () : unit = Assert.assert p
  in ([] : operation list), s

let with_error (p, s: bool * unit) : operation list * unit =
  let () = Assert.Error.assert p "my custom error"
  in [], s

let some (o : unit option) = Assert.some o

let some_with_error (o : unit option) =
  Assert.Error.some o "my custom error"

let none (o : unit option) = Assert.none o

let none_with_error (o : unit option) =
  Assert.Error.none o "my custom error"
