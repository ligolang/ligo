type parameter = One | Two

type some_record = { a : int ; b : nat ; c : string }

let log =
  let v1 = { a= 1 ; b = 2n ; c = "aaa" } in
  let v2 = One in
  let () = Test.log v1 in
  let () = Test.log v2 in
  true
