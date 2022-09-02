module A = struct 
    let x = 1
end 

let x =
    module B = struct 
        let y = 2
        let z = A.x
        module C = struct
            let a1 = y
            let a2 = z
            let a3 = 3
        end
    end in

    module D = B in
    
    let b1 = B.y in
    let b2 = B.z in
    let b3 = B.C.a1 in
    let b4 = B.C.a2 in
    let b5 = B.C.a3 in
    
    let c1 = D.y in
    let c2 = D.z in
    let c3 = D.C.a1 in
    let c4 = D.C.a2 in
    let c5 = D.C.a3 in
    
    42