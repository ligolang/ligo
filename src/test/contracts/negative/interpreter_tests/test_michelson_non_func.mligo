module Test = Test.Next

let test =
  let x : int = [%Michelson ({|{ PUSH int 1 }|} : int)] in
  begin
    Test.IO.log x;
    Assert.assert (x = x);
    Assert.assert (x = 1)
  end
