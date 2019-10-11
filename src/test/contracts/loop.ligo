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

function for_sum_up (var n : nat) : int is block {
  var acc : int := 0 ;
  for i := 1 to int(n) step 1
    begin 
      acc := acc + i ;
    end
} with acc 

function for_sum_down (var n : nat) : int is block {
  var acc : int := 0 ;
  for i := int(n) down to 1 step 1
    begin 
      acc := acc + i ;
    end
} with acc 

function for_sum_step (var n : nat) : int is block {
  var acc : int := 0 ;
  var mystep : int := 2 ;
  for i := 1 to int(n) step mystep
    begin 
      acc := acc + i ;
    end;
  for i := 0 to int(n) step mystep
    begin 
      acc := acc + i ;
    end;
} with acc 

function dummy (const n : nat) : nat is block {
  while (False) block { skip }
} with n
