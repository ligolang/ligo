#import "../core/math.mligo" "Math"

// n = a * 10^b
type t = { value : int; pow: int }

[@inline]
let new (value : int) (pow: int) : t = 
    { value=value; pow=pow }

[@inline]
let inverse (a : t) : t = 
    { value= 1n * Math.power(10n, 18n) / a.value; pow=(a.pow * -1) - 18n }

[@inline]
let add (a : t) (b : t) : t = 
    if (a.pow < b.pow) then
        { value= b.value * Math.power(10n, abs(b.pow - a.pow)) + a.value; pow=a.pow }
    else
        { value= a.value * Math.power(10n, abs(a.pow - b.pow)) + b.value; pow=b.pow }

[@inline]
let sub (a : t) (b : t) : t =
    if (a.pow < b.pow) then
        { value= a.value - b.value * Math.power(10n, abs(b.pow - a.pow)); pow=a.pow }
    else
        { value= a.value * Math.power(10n, abs(a.pow - b.pow)) - b.value; pow=b.pow }

[@inline]
let lt (a : t) (b : t) : bool = 
    if (a.value < 0) && (b.value > 0) then
        true
    else if (a.value > 0) && (b.value < 0) then
        false
    else 
        let diff = sub a b in
        diff.value < 0  

[@inline]
let lte (a : t) (b : t) : bool = 
        if (a.value < 0) && (b.value > 0) then
        true
    else if (a.value > 0) && (b.value < 0) then
        false
    else 
        let diff = sub a b in
        diff.value <= 0  

[@inline]
let gte (a : t) (b : t) : bool = 
    if (a.value >= 0) && (b.value < 0) then
        true
    else if (a.value <= 0) && (b.value > 0) then
        false
    else 
        let diff = sub a b in
        diff.value >= 0  

[@inline]
let gt (a : t) (b : t) : bool = 
    if (a.value > 0) && (b.value < 0) then
        true
    else if (a.value < 0) && (b.value > 0) then
        false
    else 
        let diff = sub a b in
        diff.value > 0  

[@inline]
let mul (a : t) (b : t) : t =
    { value= a.value * b.value ; pow=a.pow + b.pow }

[@inline]
let div (a : t) (b : t) : t =
    { value= a.value * Math.power(10n, 18n) / b.value ; pow=a.pow - b.pow - 18n }

[@inline]
let modulo (a : t) (b : t) : t =
    let rec compute (a, b : t * t) : t =
        if (lt a b) then 
            a 
        else
            compute((sub a b), b)
    in
    compute(a, b)

[@inline] [@private]
let resolve (a: t) (prec: nat) : int =
    let resolve_positif (a: t) (prec: nat) : int =
        if (a.pow > 0) then
            a.value * Math.power(10n, abs(a.pow)) * Math.power(10n, prec) 
        else
            a.value * Math.power(10n, prec) / Math.power(10n, abs(-a.pow)) 
    in
    if (a.value < 0) then
        -1 * (resolve_positif (new (int(abs(a.value))) a.pow) prec)
    else
        resolve_positif a prec