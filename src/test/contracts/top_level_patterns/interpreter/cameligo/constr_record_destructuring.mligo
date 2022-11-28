type t = Foo of int
type r = { a : int ; b : t ; c : string }

let f () =
  let () = Test.log "Once" in
  { a = 1 ; b = Foo 2 ; c = "hey" }

let { a ; b = (Foo x) ; c} = f ()

let test = 
  assert ((a + x + String.length c) = 6)
