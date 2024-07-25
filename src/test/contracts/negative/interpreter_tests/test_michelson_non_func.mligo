module Test = Test.Next
let assert = Assert.assert

let test =
  let x : int = [%Michelson ({|{ PUSH int 1 }|} : int)] in
  begin
    Test.IO.log x;
    assert (x = x);
    assert (x = 1)
  end
