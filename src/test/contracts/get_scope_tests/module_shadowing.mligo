module A = struct
    let x = 1
end

module D = A

let y = 
    let x = 1 in
    module A = struct
        let x = A.x
    end in
    module C = A in
    x + A.x