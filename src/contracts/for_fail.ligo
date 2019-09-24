// This was meant to test the for loop in PascaLIGO
// But for whatever reason, the LIGO compiler currently thinks this is a 'complex loop'
// even though it isn't. 
// See this error:
// $ ligo dry-run for.ligo main 0 0
// bounded iterators: only simple for loops are supported yet 
// {"loop_loc":"in file \"for.ligo\", line 4, characters 10-42"}


function main (const a: int) : int is
  block { for i := 0 to 100 block { skip } } with i;
