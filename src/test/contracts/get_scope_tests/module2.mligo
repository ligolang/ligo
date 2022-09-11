module A = struct
    let x = 1
    module B = struct
        let y = 2
    end
    module E = B
end

module C = A.B

module D = A

module F = A.E

let a1 = A.B.y
let a2 = A.x
let a3 = C.y
let a4 = D.x
let a5 = F.y