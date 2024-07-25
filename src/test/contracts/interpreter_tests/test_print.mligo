module Test = Test.Next

let test =
  let () = Test.IO.print "Hello " in
  let () = Test.IO.println "world" in
  let () =
    Test.IO.print (Option.value_with_error "option is None" (Test.String.chr 64n)) in
  let () = Test.IO.print (Test.String.show 42) in
  let () = Test.IO.print Test.String.nl in
  Test.String.show (true, 42n)
