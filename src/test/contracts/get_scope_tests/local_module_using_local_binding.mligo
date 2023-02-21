let a = 1

let b = 
    let a = 2 in
    module A = struct 
        let x = a
    end in
    A.x

let c = 4

let d =
    let c = 5 in
    module C = struct
        let c = 6
        let x = c
    end in
    C.x