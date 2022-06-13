function foo(const _ : unit) is {
  var x := 42;
  function bar(const _ : unit) is {
    const y = 0;
    x := 6;
  } with unit;
  bar(unit);
} with (x);
