type parameter = int * int

let rec fibo ((n,acc):int * int) : int =
    if (n < 1) then acc
    else fibo (n-1, acc+n)

let main ((p,s):parameter*int) = 
    let s = fibo (p) in
    ([] : operation list),s

