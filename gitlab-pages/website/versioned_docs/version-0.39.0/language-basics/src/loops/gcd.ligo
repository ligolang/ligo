function gcd (var x : nat; var y : nat) : nat is {
  if x < y then {
    const z : nat = x;
    x := y; y := z
  };
  var r : nat := 0n;
  while y =/= 0n {
    r := x mod y;
    x := y;
    y := r
  }
} with x
