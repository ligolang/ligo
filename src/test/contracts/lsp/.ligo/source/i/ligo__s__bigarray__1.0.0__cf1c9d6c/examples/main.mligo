#import "bigarray-cameligo/lib/bigarray.mligo" "Bigarray"

let test_fill =
  begin
    assert (Bigarray.fill 4 10 = [10;10;10;10]);
    assert (Bigarray.fill 4 "foo" = ["foo"; "foo"; "foo"; "foo"])
  end
