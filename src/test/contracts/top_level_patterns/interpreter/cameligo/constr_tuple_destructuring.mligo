module Test = Test.Next

type t = Foo of int

let f () =
  let () = Test.IO.log "Once" in
  1, (Foo 2, "hey")

let (a, ((Foo x), c)) = f ()

let test =
  Assert.assert ((a + x + String.length c) = 6)
