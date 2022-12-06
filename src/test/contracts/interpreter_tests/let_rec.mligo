let rec fibo : int -> int -> int = 
  fun n acc ->
    if n = 0 then acc
    else fibo (n - 1) (acc * n)

let fibo_no_rec_kwd : int -> int = 
  fun n ->
    let rec fibo : int * int -> int =
      fun (n, acc) ->
        if n = 0 then acc
        else fibo (n - 1, acc * n)
    in
    fibo (n, 1)

let test = fibo 100 1 = fibo_no_rec_kwd 100