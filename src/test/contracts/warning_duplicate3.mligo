module Foo = struct
    type t = {
        ck : nat ;
        c  : nat;
    }
end

let main (f : Foo.t) (_ : int) : (operation list * int) = 
    let _ = f.ck in
    let _ = f.c in
    ([] : operation list), 1