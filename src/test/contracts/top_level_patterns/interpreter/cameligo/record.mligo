let () = Test.IO.set_test_print ()

type r = { a : nat ; b : int ; c : string }

let f () =
  let () = Test.IO.log "Once" in
  { a = 1n ; b = 1 ; c = "Hello" }
let { a ; b ; c } = f ()
let { a = a1 ; b = b1 ; c = c1 } = { a = 1n ; b = 1 ; c = "Hello" }

let test =
  begin
    Assert.assert (a = a1);
    Assert.assert (b = b1);
    Assert.assert (c = c1)
  end
