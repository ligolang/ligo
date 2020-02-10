// Test a PascaLIGO function with more complex logic than function.ligo

function main (const i : int) : int is
  block {
    var j : int := 0;
    var k : int := 1;
    j := k + i;
    k := i + j
  } with k + j
