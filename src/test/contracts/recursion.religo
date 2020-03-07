// Test while loops in PascaLIGO

let rec fibo = ((n, acc) : (int,int)): int =>
    if (n < 1) {acc;}
    else {fibo ((n-1,acc+n));};
