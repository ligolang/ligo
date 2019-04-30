function plus_op (const n : int) : int is
  begin skip end with n + 42

function minus_op (const n : int) : int is
  begin skip end with n - 42

function times_op (const n : int) : int is
  begin skip end with n * 42

function div_op (const n : int) : int is
  begin skip end with n / 2

function int_op (const n : nat) : int is
  block { skip } with int(n)
