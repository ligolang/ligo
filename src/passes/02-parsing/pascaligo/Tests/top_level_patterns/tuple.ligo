const r = (1n, 1, "Hello")
const (a, b, c) = r
const (a1, b1, c1) = (2n, 2, "World")

type storage is nat * int * string

function main (const _ : unit; const _ : storage) : list(operation) * storage
  is (nil, (a + a1, b + b1, c ^ c1))