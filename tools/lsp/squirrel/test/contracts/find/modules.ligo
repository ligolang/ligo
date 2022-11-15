module B is {
    type titi is int
}

module A is {
    type titi is B.titi
    module C is {
        const toto : titi = 42
    }
    function add (const a : titi; const b : titi) : titi is a + b
}

module D is A

const toto : D.titi = {
    module E is D.C;
} with E.toto

function add (const a : A.titi; const b : D.titi) : A.titi is A.add (a,b)
