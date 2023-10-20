#import "../../core/math.mligo" "Math"
#import "../float.mligo" "Float"
#import "../trigo_float.mligo" "Trigo"

let test =

    let _test_trigo_sinus = 
        // GIVEN
        let error_threshold = (Float.inverse (Float.new 1 12)) in
        let precision : nat = 11n in

        let test_sinus (angle, expected : Float.t * Float.t) : bool = 
            let diff = Float.sub (Trigo.sinus(angle, precision)) expected in
            Float.lt diff error_threshold
        in
        // ASSERT
        let () = Test.log("sin(0)") in
        let () = assert(test_sinus(Trigo.zero, Trigo.zero)) in
        let () = Test.log("sin(Pi/2)") in
        let () = assert(test_sinus(Trigo.pi_half, Trigo.one)) in
        let () = Test.log("sin(Pi)") in
        let () = assert(test_sinus(Trigo.pi, Trigo.zero)) in
        let () = Test.log("sin(3*Pi/2)") in
        let () = assert(test_sinus(Trigo.three_pi_half, Trigo.minus_one)) in
        let () = Test.log("sin(2*Pi)") in
        let () = assert(test_sinus(Trigo.two_pi, Trigo.zero)) in
        let () = Test.log("sin(Pi/4)") in
        let expected = (Float.div Trigo.sqrt_2 Trigo.two) in
        let () = assert(test_sinus(Trigo.pi_quarter, expected)) in
        let () = Test.log("sin(Pi/6)") in
        let expected = (Float.inverse Trigo.two) in
        let () = assert(test_sinus(Trigo.pi_sixth, expected)) in
        let () = Test.log("sin(Pi/3)") in
        let expected = (Float.div Trigo.sqrt_3 Trigo.two) in
        let () = assert(test_sinus(Trigo.pi_third, expected)) in
        let () = Test.log("sin(-Pi/2)") in
        let angle = Float.sub Trigo.zero Trigo.pi_half in
        let () = assert(test_sinus(angle, Trigo.minus_one)) in
        let () = Test.log("sin(-3*Pi/2)") in
        let angle = Float.sub Trigo.pi_half Trigo.two_pi in
        let () = assert(test_sinus(angle, Trigo.one)) in

        Test.log("Test 'trigo sinus (with float)' finished")
    in
    let _test_trigo_cosinus = 
        // GIVEN
        let error_threshold = (Float.inverse (Float.new 1 12)) in
        let precision : nat = 11n in

        let test_cosinus (angle, expected : Float.t * Float.t) : bool = 
            let diff = Float.sub (Trigo.cosinus(angle, precision)) expected in
            Float.lt diff error_threshold
        in

        // ASSERT
        let () = Test.log("cos(0)") in
        let () = assert(test_cosinus(Trigo.zero, Trigo.one)) in
        let () = Test.log("cos(Pi/2)") in
        let () = assert(test_cosinus(Trigo.pi_half, Trigo.zero)) in
        let () = Test.log("cos(Pi)") in
        let () = assert(test_cosinus(Trigo.pi, Trigo.minus_one)) in
        let () = Test.log("cos(3*Pi/2)") in
        let () = assert(test_cosinus(Trigo.three_pi_half, Trigo.zero)) in
        let () = Test.log("cos(2*Pi)") in
        let () = assert(test_cosinus(Trigo.two_pi, Trigo.one)) in
        let () = Test.log("cos(Pi/4)") in
        let expected = (Float.div Trigo.sqrt_2 Trigo.two) in
        let () = assert(test_cosinus(Trigo.pi_quarter, expected)) in
        let () = Test.log("cos(Pi/6)") in
        let expected = (Float.div Trigo.sqrt_3 Trigo.two) in
        let () = assert(test_cosinus(Trigo.pi_sixth, expected)) in
        let () = Test.log("cos(Pi/3)") in
        let expected = (Float.inverse (Float.new 2 0)) in
        let () = assert(test_cosinus(Trigo.pi_third, expected)) in
        let () = Test.log("cos(-Pi/2)") in
        let angle = Float.sub (Float.new 0 0) Trigo.pi_half in
        let () = assert(test_cosinus(angle, Trigo.zero)) in
        let () = Test.log("cos(-3*Pi/2)") in
        let angle = Float.sub Trigo.pi_half Trigo.two_pi in
        let () = assert(test_cosinus(angle, Trigo.zero)) in

        Test.log("Test 'trigo cosinus (with float)' finished")
    in
    let _test_trigo = 
        let error_threshold = (Float.inverse (Float.new 1 12)) in
        let precision : nat = 11n in
        // cos²(a) + sin²(a) = 1
        let () = Test.log("cos^2(a) + sin^2(a) = 1") in
        let angle = Trigo.pi_half in
        let expected = (Float.new 1  0) in
        let cos_a = Trigo.cosinus(angle, precision) in
        let sin_a = Trigo.sinus(angle, precision) in
        let res = Float.add (Float.mul cos_a cos_a) (Float.mul sin_a sin_a) in
        let diff = Float.sub (res) expected in
        // let error = Float.resolve diff 12n in
        let () = assert(Float.lt diff error_threshold ) in

        Test.log("Test 'trigo (with float)' finished")
    in
    ()

