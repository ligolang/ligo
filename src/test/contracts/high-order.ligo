// Test a PascaLIGO function which takes another PascaLIGO function as an argument
function foobar (const i : int) : int is
  block {
    function foo (const i : int) : int is
      i ;
    function bar (const f : int -> int) : int is
      f ( i ) ;
 } with bar (foo) ;

// higher order function with more than one argument
function higher2(const i: int; const f: int -> int): int is
  block {
      const ii: int = f(i)
   } with ii

function foobar2 (const i : int) : int is
  block {
    function foo2 (const i : int) : int is
      i;
  } with higher2(i,foo2)

const a : int = 0;
function foobar3 (const i : int) : int is
  block {
    function foo2 (const i : int) : int is
      (a+i);
  } with higher2(i,foo2)

function f (const i : int) : int is
  i

function g (const i : int) : int is
  f(i)

function foobar4 (const i : int) : int is
  g(g(i))

function higher3(const i: int; const f: int -> int; const g: int -> int): int is
  block {
      const ii: int = f(g(i));
   } with ii

function foobar5 (const i : int) : int is
  block {
    const a : int = 0;
    function foo (const i : int) : int is
      (a+i);
    function goo (const i : int) : int is
      foo(i);
  } with higher3(i,foo,goo)
