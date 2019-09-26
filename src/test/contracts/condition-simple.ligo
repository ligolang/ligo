// Test if conditional with trivial conditions in PascaLIGO

function main (const i : int) : int is
  begin
    if 1 = 1 then
      i := 42
    else
      i := 0
  end with i

