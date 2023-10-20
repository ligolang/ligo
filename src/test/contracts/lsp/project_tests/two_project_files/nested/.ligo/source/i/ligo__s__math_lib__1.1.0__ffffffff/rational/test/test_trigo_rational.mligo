#import "../../core/math.mligo" "Math"
#import "../rational.mligo" "Rational"
#import "../trigo_rational.mligo" "Trigo"


let test =

    let _test_trigo_sinus = 
        let error_threshold = (Rational.inverse (Rational.new (int(Math.power(10n, 12n))))) in
        let precision : nat = 11n in
        // sin(0)
        let () = Test.log("sin(0)") in
        let angle = Trigo.zero in
        let expected = (Rational.new 0) in
        let diff = Rational.sub (Trigo.sinus(angle, precision)) expected in
        //let () = Test.log(error) in
        let () = assert(Rational.lt diff error_threshold ) in

        // sin(PI/2)
        let () = Test.log("sin(Pi/2)") in
        let angle = Trigo.pi_half in
        let expected = (Rational.new 1) in
        let diff = Rational.sub (Trigo.sinus(angle, precision)) expected in
        //let () = Test.log(error) in
        let () = assert(Rational.lt diff error_threshold ) in

        // sin(PI)
        let () = Test.log("sin(Pi)") in
        let angle = Trigo.pi in
        let expected = (Rational.new 0) in
        let diff = Rational.sub (Trigo.sinus(angle, precision)) expected in
        //let () = Test.log(error) in
        let () = assert(Rational.lt diff error_threshold ) in

        // sin(3*PI/2)
        let () = Test.log("sin(3*Pi/2)") in
        let angle = Trigo.three_pi_half in
        let expected = (Rational.new (-1)) in
        let diff = Rational.sub (Trigo.sinus(angle, precision)) expected in
        //let () = Test.log(error) in
        let () = assert(Rational.lt diff error_threshold ) in
        
        // sin(2*PI)
        let () = Test.log("sin(2*Pi)") in
        let angle = Trigo.two_pi in
        let expected = (Rational.new 0) in
        let diff = Rational.sub (Trigo.sinus(angle, precision)) expected in
        //let () = Test.log(error) in
        let () = assert(Rational.lt diff error_threshold ) in

        // sin(PI/4)
        let () = Test.log("sin(Pi/4)") in
        let angle = Trigo.pi_quarter in
        let expected = (Rational.div Trigo.sqrt_2 (Rational.new 2)) in
        let diff = Rational.sub (Trigo.sinus(angle, precision)) expected in
        //let () = Test.log(error) in
        let () = assert(Rational.lt diff error_threshold ) in
        
        // sin(PI/6)
        let () = Test.log("sin(Pi/6)") in
        let angle = Trigo.pi_sixth in
        let expected = (Rational.inverse (Rational.new 2)) in
        //let expected = (Rational.div Trigo.sqrt_3 (Rational.new 2)) in
        let diff = Rational.sub (Trigo.sinus(angle, precision)) expected in
        //let () = Test.log(error) in
        let () = assert(Rational.lt diff error_threshold ) in

        // sin(PI/3)
        let () = Test.log("sin(Pi/3)") in
        let angle = Trigo.pi_third in
        let expected = (Rational.div Trigo.sqrt_3 (Rational.new 2)) in
        let diff = Rational.sub (Trigo.sinus(angle, precision)) expected in
        //let () = Test.log(error) in
        let () = assert(Rational.lt diff error_threshold ) in

        // sin(-PI/2)
        let () = Test.log("sin(-Pi/2)") in
        let angle = Rational.sub (Rational.new 0) Trigo.pi_half in
        let expected = (Rational.new (-1)) in
        let diff = Rational.sub (Trigo.sinus(angle, precision)) expected in
        //let () = Test.log(error) in
        let () = assert(Rational.lt diff error_threshold ) in

        // sin(-3*PI/2)
        let () = Test.log("sin(-3*Pi/2)") in
        //let angle = Rational.sub Trigo.zero Trigo.three_pi_half in
        let angle = Rational.sub Trigo.pi_half Trigo.two_pi in
        let expected = (Rational.new (1)) in
        let diff = Rational.sub (Trigo.sinus(angle, precision)) expected in
        //let () = Test.log(error) in
        let () = assert(Rational.lt diff error_threshold ) in

        Test.log("Test 'trigo sinus (with rational)' finished")
    in
    let _test_trigo_cosinus = 
        let error_threshold = (Rational.inverse (Rational.new (int(Math.power(10n, 12n))))) in
        let precision : nat = 11n in
        // cos(0)
        let () = Test.log("cos(0)") in
        let angle = Trigo.zero in
        let expected = (Rational.new 1) in
        let diff = Rational.sub (Trigo.cosinus(angle, precision)) expected in
        //let () = Test.log(error) in
        let () = assert(Rational.lt diff error_threshold ) in

        // cos(PI/2)
        let () = Test.log("cos(Pi/2)") in
        let angle = Trigo.pi_half in
        let expected = (Rational.new 0) in
        let diff = Rational.sub (Trigo.cosinus(angle, precision)) expected in
        //let () = Test.log(error) in
        let () = assert(Rational.lt diff error_threshold ) in

        // cos(PI)
        let () = Test.log("cos(Pi)") in
        let angle = Trigo.pi in
        let expected = (Rational.new (-1)) in
        let diff = Rational.sub (Trigo.cosinus(angle, precision)) expected in
        //let () = Test.log(error) in
        let () = assert(Rational.lt diff error_threshold ) in

        // cos(3*PI/2)
        let () = Test.log("cos(3*Pi/2)") in
        let angle = Trigo.three_pi_half in
        let expected = (Rational.new 0) in
        let diff = Rational.sub (Trigo.cosinus(angle, precision)) expected in
        //let () = Test.log(error) in
        let () = assert(Rational.lt diff error_threshold ) in
        
        // cos(2*PI)
        let () = Test.log("cos(2*Pi)") in
        let angle = Trigo.two_pi in
        let expected = (Rational.new 1) in
        let diff = Rational.sub (Trigo.cosinus(angle, precision)) expected in
        //let () = Test.log(error) in
        let () = assert(Rational.lt diff error_threshold ) in

        // cos(PI/4)
        let () = Test.log("cos(Pi/4)") in
        let angle = Trigo.pi_quarter in
        let expected = (Rational.div Trigo.sqrt_2 (Rational.new 2)) in
        let diff = Rational.sub (Trigo.cosinus(angle, precision)) expected in
        //let () = Test.log(error) in
        let () = assert(Rational.lt diff error_threshold ) in
        
        // cos(PI/6)
        let () = Test.log("cos(Pi/6)") in
        let angle = Trigo.pi_sixth in
        let expected = (Rational.div Trigo.sqrt_3 (Rational.new 2)) in
        //let expected = (Rational.div Trigo.sqrt_3 (Rational.new 2)) in
        let diff = Rational.sub (Trigo.cosinus(angle, precision)) expected in
        //let () = Test.log(error) in
        let () = assert(Rational.lt diff error_threshold ) in

        // cos(PI/3)
        let () = Test.log("cos(Pi/3)") in
        let angle = Trigo.pi_third in
        let expected = (Rational.inverse (Rational.new 2)) in
        let diff = Rational.sub (Trigo.cosinus(angle, precision)) expected in
        //let () = Test.log(error) in
        let () = assert(Rational.lt diff error_threshold ) in

        // cos(-PI/2)
        let () = Test.log("cos(-Pi/2)") in
        let angle = Rational.sub (Rational.new 0) Trigo.pi_half in
        let expected = (Rational.new 0) in
        let diff = Rational.sub (Trigo.cosinus(angle, precision)) expected in
        //let () = Test.log(error) in
        let () = assert(Rational.lt diff error_threshold ) in

        // cos(-3*PI/2)
        let () = Test.log("cos(-3*Pi/2)") in
        //let angle = Rational.sub Trigo.zero Trigo.three_pi_half in
        let angle = Rational.sub Trigo.pi_half Trigo.two_pi in
        let expected = (Rational.new 0) in
        let diff = Rational.sub (Trigo.cosinus(angle, precision)) expected in
        //let () = Test.log(error) in
        let () = assert(Rational.lt diff error_threshold ) in

        Test.log("Test 'trigo cosinus (with rational)' finished")
    in
    let _test_trigo = 
        let error_threshold = (Rational.inverse (Rational.new (int(Math.power(10n, 12n))))) in
        let precision : nat = 11n in
        // cos²(a) + sin²(a) = 1
        let () = Test.log("cos^2(a) + sin^2(a) = 1") in
        let angle = Trigo.pi_half in
        let expected = (Rational.new 1) in
        let cos_a = Trigo.cosinus(angle, precision) in
        let sin_a = Trigo.sinus(angle, precision) in
        let res = Rational.add (Rational.mul cos_a cos_a) (Rational.mul sin_a sin_a) in
        let diff = Rational.sub (res) expected in
        let () = assert(Rational.lt diff error_threshold ) in

        Test.log("Test 'trigo (with rational)' finished")
    in
    ()

