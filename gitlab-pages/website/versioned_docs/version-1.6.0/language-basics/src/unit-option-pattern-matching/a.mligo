let n : unit = ()
let m (x : int) =
  begin
    assert (x > 0);
    assert (x < 10);
    x
  end