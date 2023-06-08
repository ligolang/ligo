type parameter = One | Two

type some_record = { a : int ; b : nat ; c : string }

let () =
  let v1 = { a= 1 ; b = 2n ; c = "aaa" } in
  let v2 = One in
  begin
    Test.log v1 ;
    Test.log v2
  end
