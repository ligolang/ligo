# 1 "examples/functions.ligo"
# 1 "<built-in>"
# 1 "<command-line>"
# 31 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































# 32 "<command-line>" 2
# 1 "examples/functions.ligo"
function multiply (const a : int ; const b : int) : int is
    begin
        const result : int = a * b ;
    end with result

function add (const a : int ; const b : int) : int is
    block { skip } with a + b

function main (const p : unit ; const s : unit) : (list(operation) * unit) is
  block {skip} with ((nil : list(operation)), s)
