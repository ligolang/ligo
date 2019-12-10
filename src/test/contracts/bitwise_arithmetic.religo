/* Test ReasonLigo bitwise operators */

let or_op = (n: nat): nat => Bitwise.lor(n, 4n);
let and_op = (n: nat): nat => Bitwise.land(n, 7n);
let xor_op = (n: nat): nat => Bitwise.lxor(n, 7n);
