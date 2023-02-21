
module A = struct
    module B = struct
        let toto = 1
    end
end

module C = struct
    module D = A.B
    let tata = D.toto
end