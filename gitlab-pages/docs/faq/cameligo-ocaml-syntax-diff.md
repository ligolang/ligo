---
id: cameligo-ocaml-syntax-diff
title: What are the differences between syntaxes of CameLIGO and OCaml ?
---

Most of the CameLIGO syntax follows the OCaml syntax, however, there are a few syntactic shortcuts available in one but not the other.

### Consecutive '-' operators

In OCaml, you can do :

```ocaml
let y = - -1 (* In OCaml *)
```
But this has been forbidden in CameLIGO, you have to add parentheses instead:

```cameligo
let y = -(-1) // In CameLIGO
```

### Unary '+' operator

This is possible in OCaml but not CameLIGO :

```ocaml
let x = +1 (* In OCaml *)
```

### 'type in' statements

In CameLIGO, you can declare types locally to an expression.
For example, here is a function returning a list of integers :

```cameligo
// In CameLIGO
let res = 
  type t = int list in
  let x : t = [42] in
  43 :: x
```

### Semicolons in `begin ... end` sequences

In OCaml, the last instruction of a `begin ... end` sequence can be terminated by a semicolon `;`, but not in CameLIGO.

```cameligo
// In CameLIGO
type param   = int
type storage = int

let main (_p, s : param * storage) : operation list * storage =
  let tests =
    begin
      assert (1 = 1);
      assert (2 = 2) // no semicolon here
    end
  in
  [], s
```

### Name punning

Name punning permits record assignments without repeating the right-hand side if it is the same as the record field name.
Although possible in OCaml, this is not yet avaiable in CameLIGO.

```ocaml
(* In OCaml *)
type point = {
  x : int;
  y : int;
}

let x = 24
let y = 42
  
let p_assign_without_punning : point = {x = x; y = y}
let p_assign_with_punning    : point = {x; y}  (* Unavailable in CameLIGO *)
```
