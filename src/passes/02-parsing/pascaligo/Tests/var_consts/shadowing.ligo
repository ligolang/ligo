function foo(const toto : int) is
  {
    var toto := 2;
    toto := 3;
  } with toto

function bar(var _u : unit) is
  {
    const toto = 1;
    var toto := 2;
    toto := 3;
  } with toto
