module A = struct
    module B = struct
        module C = struct
            let x = 1
        end
        module F = C
    end
    module E = B
end

module D = A

let x1 = A.B.C.x
let x2 = A.B.F.x
let x3 = A.E.C.x
let x4 = A.E.F.x
let x5 = D.B.C.x
let x6 = D.B.F.x
let x7 = D.E.C.x
let x8 = D.E.F.x