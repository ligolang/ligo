module Foo = struct
    type t = {
        ck : chest_key ;
        c  : chest     ;
    }
end

let main ((f,_) : (Foo.t * int)) : (operation list * int) = 
    let _ = f.ck in
    let _ = f.c in
    ([] : operation list), 1