module A = struct
type str = string
let s : str = "s"
end

let t = assert (A.s = "s" : bool)
let q : A.str = A.s

type astr = A.str
let p : astr = q
