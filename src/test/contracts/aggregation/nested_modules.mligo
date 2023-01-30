module A = struct
  module B = struct
    let nested : int = 0
  end
  module Bx = B
  let toto = Bx.nested
end


module A2 = struct
  module B = struct
    module C = struct
      let toto = 0
    end
  end
  module Cx = B.C
end

module A3 = struct
  let toto =
    module By = A2.B in
    By.C.toto
end

module A4 = struct
  let toto =
    module Cy = A2.Cx in
    Cy.toto
end

(* double jump *)
module Top = struct
  let toto = 0
end

module Middle = struct
  module Top = Top
end

module End = struct
  module Top = Middle.Top
end

module Mangled = struct
	module Ledger = struct
		let toto = 1
	end
	module Storage = struct
		let toto = Ledger.toto
	end

	let titi = Storage.toto
end

module M = Mangled



let main (action : int) (store : int) : operation list * int =
  [], A.Bx.nested + A.toto + A2.Cx.toto + A3.toto + A4.toto + End.Top.toto + M.titi
