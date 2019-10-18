// Test if conditional in PascaLIGO

function main (const i : int) : int is
  var result : int := 23 ;
  begin
    if i = 2 then
      result := 42
    else
      result := 0
  end with result

function foo (const b : bool) : int is
  var x : int := 41 ;
  begin
    x := 1 + (if b then x else main(x)) ;
  end with x