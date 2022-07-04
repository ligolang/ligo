// Test while loops in PascaLIGO

function for_sum (var n : nat) : int is
  {
    var acc : int := 0;
    for i := 1 to int (n)
      {
        acc := acc + i
      }
  } with acc
