// Test while loops in PascaLIGO

recursive function fibo (const n : int; const acc: int) : int is
  if n<1 then acc else fibo(n-1,acc+n)
