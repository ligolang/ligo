// Test PascaLIGO bitwise operators

function or_op (const n : nat) : nat is
  bitwise_or(n , 4n)

function and_op (const n : nat) : nat is
  bitwise_and(n , 7n)

function xor_op (const n : nat) : nat is
  bitwise_xor(n , 7n)

function lsl_op (const n : nat) : nat is
  bitwise_lsl(n , 7n)

function lsr_op (const n : nat) : nat is
  bitwise_lsr(n , 7n)