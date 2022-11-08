type foo = { useless : unit; useless_2 : unit }

let do_nothing _ = ()

let main (_, _ : unit * unit) : operation list * unit =
  let useless = () in
  let foo = { useless; useless_2 = useless |> do_nothing } in
  [], foo.useless |> do_nothing |> do_nothing
