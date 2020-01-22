const x : int = 1; attributes ["inline"]

function foo (const a : int) : int is
  begin
    const test : int = 2 + a;
    attributes ["inline"];
  end with test;
attributes ["inline"];

const y : int = 1; attributes ["inline"; "other"]

function bar (const b : int) : int is
  begin
    function test (const z : int) : int is
      begin
        const r : int = 2 + b + z
      end with r;
    attributes ["inline"; "foo"; "bar"]
  end with test(b)
