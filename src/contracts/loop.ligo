// Test while loops in PascaLIGO

function counter (var n : nat) : nat is block {
  var i : nat := 0n ;
  while (i < n) block {
    i := i + 1n ;
  }
} with i

function while_sum (var n : nat) : nat is block {
  var i : nat := 0n ;
  var r : nat := 0n ;
  while (i < n) block {
    i := i + 1n ;
    r := r + i ;
  }
} with r

function for_sum (var n : nat) : nat is block {
  for i := 1 to 100
    begin 
      n := n + 1;
    end }
  with n

function dummy (const n : nat) : nat is block {
  while (False) block { skip }
} with n
