module A = struct
    type t = int
    type 'a x = { foo : 'a } 

    let a : t = 1
    let b : string x = { foo = "bar" }

    let exp1 =
        type foo = string in
        type 'a bar = { bar : 'a } in
        // Module type in
        let g : foo = "Hello" in
        // Module parametric type in
        let h : string bar = { bar = "World" } in
        ()
end

module B = A

// Module alias type
let c : B.t = 2
// Module alias parametric type
type hmm = B.x
let d : nat hmm = { foo = 1n }

// Module type
let c : A.t = 3
// Module parametric type
type idk = A.x
let d : bool idk = { foo = true }

type s = nat
type 'a q = Baz of 'a

// toplevel type
let e : s = 4n
// toplevel parametric type
let f : bool q = Baz false

let exp2 = 
    type qux = bool in
    // type in
    let i : qux = false in
    type 'a boo = 'a option in
    // paramertic type in
    let j : nat boo = Some 1n in
    ()