(** This function is the integer square root function. *)
let isqrt (y: nat) =
    if y > 3n then
        let z = y in
        let x = y / 2n + 1n in
        let rec iter (x, y, z: nat * nat * nat): nat =
            if x < z then
                iter ((y / x + x) / 2n, y, x)
            else
                z
        in
        iter (x, y, z)
    else if y <> 0n then
        1n
    else
        0n

let rec fast_power : nat -> nat -> nat -> nat = fun a b result ->
    if b = 0n then result
    else 
        let result = if (b land 1n) = 1n then result * a else result in
        let b = b lsr 1n in
        let a = a * a in
        fast_power a b result

(** return x ^ y *)
let power (x, y : nat * nat) : nat = fast_power x y 1n

(** It computes the factorial n! for a given nat n. (i.e. n*(n-1)*(n-2)*...*1n ) *)
let factorial (n : nat) : nat = 
    let rec fact(acc, i : nat * nat) : nat = 
        if (i < 2n) then acc else fact(acc * i, abs(i - 1n)) in
    fact(1n, n)

let min (a, b: nat * nat) : nat =
    if (a < b) then a else b

let max (a, b: nat * nat) : nat =
    if (a > b) then a else b

// TODO : optimize log algorithm !
let log_10 (x : nat) : nat =
    let rec check_power(x, i : nat * nat) : nat =
        if (x mod power(10n, i) > 0n) then
            abs(i - 1n)
        else
            check_power(x, i + 1n)
    in 
    check_power(x, 1n)
