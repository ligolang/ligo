module A =
  struct
    let bar = 0
    module B =
      struct
        let baz = 1
        module C =
          struct
            let bax = 2
          end
      end
  end

let complete_me_1 = A.B.ba
let complete_me_2 = A.B.C.ba
