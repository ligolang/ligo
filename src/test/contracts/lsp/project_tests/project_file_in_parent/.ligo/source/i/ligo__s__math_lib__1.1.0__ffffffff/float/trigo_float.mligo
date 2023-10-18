#import "./float.mligo" "Float"

type chebychev_coef = Float.t list 

let zero : Float.t = {value=0 ; pow=0}
let one : Float.t = {value=1; pow=0}
let minus_one : Float.t = {value=-1; pow=0}
let two : Float.t = {value=2; pow=0}
//PI = 1.414213562373095048801688724209698078569671875376948073176679737990732478462107038850387534327641572735013846230912297024924836055850737212644121497099935831413222665927505592755799950501152782060571
let pi : Float.t = {value=31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679 ; pow=-100}
let two_pi : Float.t = Float.mul pi (Float.new 2 0)
let pi_half : Float.t = Float.div pi (Float.new 2 0)
let pi_quarter : Float.t = Float.div pi (Float.new 4 0)
let three_pi_half : Float.t = Float.mul pi_half (Float.new 3 0)
let three_pi_quarter : Float.t = Float.mul pi_quarter (Float.new 3 0)
let five_pi_quarter : Float.t = Float.mul pi_quarter (Float.new 5 0)
let seven_pi_quarter : Float.t = Float.mul pi_quarter (Float.new 7 0)
let pi_third : Float.t = Float.div pi (Float.new 3 0)
let pi_sixth : Float.t = Float.div pi (Float.new 6 0)

let sqrt_2 : Float.t = {value=1414213562373095048801688724209; pow=-30}
let sqrt_3 : Float.t = {value=17320508075688772935; pow=-19}

(* computes sinus for an ange between zero and half_pi *)
let sin(a, n : Float.t * nat) : Float.t = 
    let _check_angle_positive = assert_with_error (Float.gte a zero) "[Trigo_float.sinus] given angle is out of bound" in
    let _check_angle_pi_half = assert_with_error (Float.lte a pi_half) "[Trigo_float.sinus] given angle is out of bound" in
    let one = Float.new 1 0 in
    let two = Float.new 2 0 in
    //let u = (x - (a+b)/2.0 )/ (b-a)/2.0
    let u = (Float.div
        (Float.sub a (Float.div (Float.add zero pi_half) (two)))
        (Float.div (Float.sub pi_half zero) (two))
    ) in
    let coef : chebychev_coef = [ 
            {value=6021947012555463;pow=-16};
            {value=513625166679107;pow=-15};
            {value=-10354634426296383;pow=-17};
            {value=-13732034234358675;pow=-18};
            {value=13586698380902013;pow=-19};
            {value=10726309440570181;pow=-20};
            {value=-7046296793891682;pow=-21};
            {value=-3963902510811801;pow=-22};
            {value=194995972671759;pow=-22};
            {value=8522923894416223;pow=-25};
            {value=-3351717514643582;pow=-26};
            {value=-11987008607938776;pow=-28};
            {value=3835820550079916;pow=-29};
        ]
    in

    let coef_0 = Option.unopt (List.head_opt coef) in
    let coef_1 = Option.unopt (List.head_opt (Option.unopt (List.tail_opt coef))) in
    let y0 = Float.add coef_0 (Float.mul coef_1 u) in
    let t0 : Float.t = one in
    let t1 : Float.t = u in
    let coef_from_2 = Option.unopt (List.tail_opt (Option.unopt (List.tail_opt coef))) in
    let rec compute (i, acc, t_prev, t_prev_prev, n, coef : nat * Float.t * Float.t * Float.t * nat * chebychev_coef) : Float.t =
        if (i <= n) then
            let t_next_u = Float.sub (Float.mul (Float.mul two u) t_prev) t_prev_prev in
            let current_coef = Option.unopt (List.head_opt coef) in
            let rest_coef = Option.unopt (List.tail_opt coef) in
            let new_acc = Float.add acc (Float.mul t_next_u current_coef) in
            compute(i + 1n, new_acc, t_next_u, t_prev, n, rest_coef)
        else
            acc
    in
    compute(2n, y0, t1, t0, n, coef_from_2)

[@private]
let rec sinus_symetry(sign, a, n : Float.t * Float.t * nat) : Float.t =
    // sin(-a) = - sin(a)
    if (Float.lt a zero) then
        sinus_symetry(Float.mul sign (Float.new (-1) 0), Float.mul a (Float.new (-1) 0), n)
    else
        let a_mod_two_pi = Float.modulo a two_pi in 
        //[0, pi_half]
        if (Float.lte a_mod_two_pi pi_half) then
            Float.mul sign (sin(a_mod_two_pi, n))
        //[pi_half, pi] -> sin(Pi/2 + a) = sin(Pi/2 - a)
        else if (Float.lte a_mod_two_pi pi) then
            let theta = Float.sub a_mod_two_pi pi_half in
            let half_pi_minus_a = Float.sub pi_half theta in
            Float.mul sign (sin(half_pi_minus_a, n))
        //[pi, two_pi] -> sin(Pi + a) = - sin(a)
        else if (Float.lt pi a_mod_two_pi) then
            let minus_pi = Float.sub a_mod_two_pi pi in 
            sinus_symetry(Float.mul sign (Float.new (-1) 0), minus_pi, n)
        else
            (failwith("[Trigo_float.sinus] ERROR out of bound angle") : Float.t)

let sinus(a, n : Float.t * nat) : Float.t =
    sinus_symetry(Float.new (1) 0, a, n)

let cosinus(a, n : Float.t * nat) : Float.t = 
    let plus_half_PI = Float.add a pi_half in 
    sinus_symetry(Float.new (1) 0, plus_half_PI, n)
