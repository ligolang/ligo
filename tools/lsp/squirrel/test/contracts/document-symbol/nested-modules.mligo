module A = struct
  let a : string = "A"
  module B = struct
    module C = struct
      let c : string = "c"
    end
    let b : string = "b"
  end
end
