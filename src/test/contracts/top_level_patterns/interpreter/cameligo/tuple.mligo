let () = Test.IO.set_test_print ()

let f () =
  let () = Test.IO.log "Once" in
  (1n, 1, "Hello")
let (a, b, c) = f ()

let (a1, b1, c1) = (1n, 1, "Hello")

let test =
  begin
    Assert.assert (a = a1);
    Assert.assert (b = b1);
    Assert.assert (c = c1)
  end
