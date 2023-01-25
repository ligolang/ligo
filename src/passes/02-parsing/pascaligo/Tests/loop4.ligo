// Test while loops in PascaLIGO

function while_sum (var n : nat) : nat is
  {
    var i : nat := 0n;
    var r : nat := 0n;
    while i < n {
      i := i + 1n;
      r := r + i
    };
    var _ := i;
  } with r


function while_record (var n: nat) : nat is
  {
    var c := 0n;
    var tmp := record [ y = n; prev_y = 0n; ];
    while abs (tmp.y - tmp.prev_y) > 1n
      {
        tmp.prev_y := tmp.y ;
        tmp.y := (tmp.y + c) / 2n
      }
  } with tmp.y
