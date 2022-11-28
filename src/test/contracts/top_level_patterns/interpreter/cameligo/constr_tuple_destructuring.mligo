type t = Foo of int

let f () = 
  let () = Test.log "Once" in
  1, (Foo 2, "hey")

let (a, ((Foo x), c)) = f ()

let test = 
  assert ((a + x + String.length c) = 6)
