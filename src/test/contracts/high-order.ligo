// Test a PascaLIGO function which takes another PascaLIGO function as an argument
function foobar (const i : int) : int is
  function foo (const i : int) : int is
    block { skip } with i ;
  function bar (const f : int -> int) : int is
    block { skip } with f ( i ) ;
  block { skip } with bar (foo) ;

// higher order function with more than one argument
function higher2(const i: int; const f: int -> int): int is
  block { 
      const ii: int = f(i)
   } with ii

function foobar2 (const i : int) : int is
  function foo2 (const i : int) : int is
    block { skip } with i;
  block { skip } with higher2(i,foo2)

const a : int = 0;
function foobar3 (const i : int) : int is
  function foo2 (const i : int) : int is
    block { skip } with (a+i);
  block { skip } with higher2(i,foo2)

function f (const i : int) : int is
  block { skip }
  with i

function g (const i : int) : int is
  block { skip }
  with f(i)

function foobar4 (const i : int) : int is
  block { skip }
  with g(g(i))

function higher3(const i: int; const f: int -> int; const g: int -> int): int is
  block { 
      const ii: int = f(g(i));
   } with ii

function foobar5 (const i : int) : int is
  const a : int = 0;
  function foo (const i : int) : int is
    block { skip } with (a+i);
  function goo (const i : int) : int is
    block { skip } with foo(i);
  block { skip } with higher3(i,foo,goo)
