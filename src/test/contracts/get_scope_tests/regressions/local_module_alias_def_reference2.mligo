let toto : int =
    module A = struct 
        module C = struct
            let toto = 1
        end
    end in
    module E = struct 
        module F = A.C
        let toto = F.toto
    end in
    E.toto