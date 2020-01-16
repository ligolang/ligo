const x: int = 1; attributes ["inline"];

function foo (const a : int) : int is
  block {
    const test: int = 2 + a; attributes ["inline"];
  } with test;
attributes ["inline"];
  
const y: int = 1; attributes ["inline"; "other"];

function bar (const b : int) : int is
  block {
    function test (const z : int) : int is begin
      const r : int = 2 + b + z
    end with r;
    attributes ["inline"; "foo"; "bar"];
  } with test(b);
