module A = struct
    let x = 1
end

let y = 
    let x = 1 in
    module A = struct
        let x = A.x
    end in
    x + A.x