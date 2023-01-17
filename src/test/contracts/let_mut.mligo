let swap (a, b) = 
  let mut ma = a in
  let mut mb = b in
  begin
    ma := b;
    mb := a;
    ma, mb
  end

