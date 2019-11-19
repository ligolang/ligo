// Test if conditional in PascaLIGO

function main (const i : int) : int is
  begin
    var result : int := 23 ;
    if i = 2 then
      result := 42
    else
      result := 0
  end with result

function foo (const b : bool) : int is
  begin
    var x : int := 41 ;
    x := 1 + (if b then x else main(x)) ;
  end with x
