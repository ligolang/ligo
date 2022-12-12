const Unit = Test.set_print_values ()

function f (const _ : unit) is { 
  Test.log ("Once");
} with (1n, 1, "Hello")

const (a, b, c) = f (Unit)

const (a1, b1, c1) = (1n, 1, "Hello")

const test = {
  assert (a = a1);
  assert (b = b1);
  assert (c = c1);
} with Unit