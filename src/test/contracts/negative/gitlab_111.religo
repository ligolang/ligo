type fraction = 
{ 
    num : int,
    den : nat
};

type storage = fraction;
type parameter = unit;


let twopow_aux = (base : nat, accu : nat, n : nat)  => 
    if (n > 0n) {
        if (n mod 2n == 1n) {
            continue((base * base, accu * base, n / 2n));
        } else {
            continue((base * base, accu, n / 2n));
        };
    } else {
        stop((base, accu, n));
    };

let twopow = (n : nat) : nat => {    
    let (_, x, _) =  Loop.fold_while(twopow_aux, (2n, 1n, n));
    x
};

let tp = 
let exp = (x : fraction) : fraction => {
    let r : int =  1649 * x.num;
    let s : nat =  1143n * x.den;
    let ab = {
        num: r / int(s), den: r mod s
        };

    let (i, f) = 
    if (2n * ab.den <= s) {
        (ab.num, {num: ab.den, den: 1649n * x.den});
    }
    else {
        (ab.num + 1, {num: ab.den - s, den: 1649n * x.den});
    };    
    let pow : nat = twopow(abs(i));
    let tp = if (i > 0) {{num:int(pow), den:1n};} else {{num:1, den:pow};};
    let num : int =  (f.num * f.num + 6 * f.num * int(f.den) + int(12n * f.den * f.den)) * tp.num;
    let den : int =  (f.num * f.num - 6 * f.num * int(f.den) + int(12n * f.den * f.den)) * int(tp.den);
    if (den < 0)
    {
        {num:-num, den:abs(den)};
    } else {
        {num:num, den:den};
    };
};

let main = (p : parameter, s : storage) =>
{
    let r = exp(s);
    ([]: list(operation), r)
};
