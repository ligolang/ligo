# 1 "examples/multiple-entrypoints.ligo"
# 1 "<built-in>"
# 1 "<command-line>"
# 31 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































# 32 "<command-line>" 2
# 1 "examples/multiple-entrypoints.ligo"
type action is
| Increment of int
| Decrement of int

function add (const a : int ; const b : int) : int is
    block { skip } with a + b

function subtract (const a : int ; const b : int) : int is
    block { skip } with a - b

function main (const p : action ; const s : int) : (list(operation) * int) is
  block {skip} with ((nil : list(operation)),
    case p of
    | Increment n -> add(s, n)
    | Decrement n -> subtract(s, n)
    end)
