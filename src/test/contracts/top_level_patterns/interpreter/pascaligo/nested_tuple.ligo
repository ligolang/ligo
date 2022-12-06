const Unit = Test.set_print_values ()

function f (const _ : unit) is { 
    Test.log ("Once");
} with ((1n, 1, "H"), (2n, 2, "E"), (3n, 3, "L"))

const ((a1, a2, a3), (b1, b2, b3), (c1, c2, c3)) = f (Unit)

const ((a4, a5, a6), (b4, b5, b6), (c4, c5, c6)) 
    = ((1n, 1, "H"), (2n, 2, "E"), (3n, 3, "L"))

const test = {
    assert ((a1 + b1 + c1) = (a4 + b4 + c4));
    assert ((a2 + b2 + c2) = (a5 + b5 + c5));
    assert ((a3 ^ b3 ^ c3) = (a6 ^ b6 ^ c6));
} with Unit