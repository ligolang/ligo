// Test a trivial PascaLIGO procedure

procedure sub (const j: int) is 
  begin
    i := i + 1
  end

function main (const i: int) : int is 
  begin 
    sub(i)
  end with i 
