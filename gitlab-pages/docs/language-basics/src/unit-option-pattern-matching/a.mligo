let n : unit = ()
let m (x : int) =
  begin
    Assert.assert (x > 0);
    Assert.assert (x < 10);
    x
  end