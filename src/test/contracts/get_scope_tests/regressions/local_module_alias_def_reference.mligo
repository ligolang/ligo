module A = struct
    type titi = int
    module C = struct
        let toto: titi = 42
    end
end

module D = A

let toto : D.titi =
    module E = A.C in
    E.toto

