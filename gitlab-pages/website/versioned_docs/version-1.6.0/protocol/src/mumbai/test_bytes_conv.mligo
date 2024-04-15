(* bytes -> nat *)
let test_bytes_nat = nat 0x1234 (* 1234n *)

(* nat -> bytes *)
let test_nat_bytes = bytes 4660n (* 0x1234 *)

(* bytes -> int *)
let test_bytes_int = int 0x1234 (* 4660 *)

(* int -> bytes *)
let test_int_bytes = bytes 4660 (* 0x1234 *)