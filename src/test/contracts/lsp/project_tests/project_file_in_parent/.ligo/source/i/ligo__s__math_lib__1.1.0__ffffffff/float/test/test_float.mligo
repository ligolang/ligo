#import "../float.mligo" "Float"

let test =

    let _test_scientific = 

        let a : Float.t = Float.inverse (Float.new 6 0) in
        let value_resolved : int = Float.resolve a 3n in
        let () = assert(value_resolved = 166) in

        let a : Float.t = Float.inverse (Float.new (6) 0) in
        let a : Float.t = Float.mul (Float.new (-1) 0) a in
        let value_resolved : int = Float.resolve a 3n in
        let () = assert(value_resolved = -166) in

        let a : Float.t = Float.inverse (Float.new 3 0) in
        let b : Float.t = Float.inverse (Float.new 2 0) in
        let value : Float.t = Float.add a b in
        //let () = assert(value = { p=5; q=6 }) in
        let value_resolved : int = Float.resolve value 3n in
        let () = assert(value_resolved = 833) in

        let a : Float.t = Float.inverse (Float.new 3 0) in
        let b : Float.t = Float.inverse (Float.new 2 0) in
        let value : Float.t = Float.sub a b in
        //let () = assert(value = { p=-1; q=6 }) in
        let value_resolved : int = Float.resolve value 3n in
        let () = assert(value_resolved = -166) in

        let a : Float.t = Float.inverse (Float.new 3 0) in
        let b : Float.t = Float.inverse (Float.new 2 0) in
        let value : Float.t = Float.mul a b in
        //let () = assert(value = { p=1; q=6 }) in
        let value_resolved : int = Float.resolve value 3n in
        let () = assert(value_resolved = 166) in

        let a : Float.t = Float.inverse (Float.new 3 0) in
        let b : Float.t = Float.inverse (Float.new 2 0) in
        let value : Float.t = Float.div a b in
        //let () = assert(value = { p=2; q=3 }) in
        let value_resolved : int = Float.resolve value 3n in
        let () = assert(value_resolved = 666) in

        // 1/2 % 1/3
        let a : Float.t = Float.inverse (Float.new 2 0) in
        let b : Float.t = Float.inverse (Float.new 3 0) in
        let value : Float.t = Float.modulo a b in
        //let () = assert(value = { p=2; q=3 }) in
        let value_resolved : int = Float.resolve value 3n in
        let () = assert(value_resolved = 166) in

        Test.log("Test finished")
    in
    ()

