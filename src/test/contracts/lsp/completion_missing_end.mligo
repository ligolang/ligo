module ABA = struct let foo = 1 end

module CCC = struct
    let bar = 2
    module ABA = struct let faa = 2 end
    let x = A
