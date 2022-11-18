let test_sub =
 let subtractthis : int = 2*86400 in
 let the_time : timestamp = ("1970-01-04t00:00:00Z" : timestamp) in
 let new_time_mich : timestamp = Test.decompile (Test.run (fun () -> the_time - subtractthis) ()) in
 let new_time : timestamp = the_time - subtractthis in
 assert (new_time = new_time_mich)

let test_get_time =
  assert (Test.get_time () = ("1970-01-01t00:00:00Z" : timestamp))
