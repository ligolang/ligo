type foofoo = {foo : int; bar : int}

let x = { foo = 1 ; bar = 2 }

let y = 
    let { foo ; foo ; bar } = x in
    foo + bar