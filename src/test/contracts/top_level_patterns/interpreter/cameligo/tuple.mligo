let () = Test.set_print_values ()

let f () = 
  let () = Test.log "Once" in
  (1n, 1, "Hello")
let (a, b, c) = f ()

let (a1, b1, c1) = (1n, 1, "Hello")

let test =
  begin
    assert (a = a1);
    assert (b = b1);
    assert (c = c1)
  end