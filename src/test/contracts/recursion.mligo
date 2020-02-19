// Test while loops in PascaLIGO

let fibo (n : int) (acc: int) : int =
    if (n < 1) then acc
    else fibo (n-1) (acc+n)
