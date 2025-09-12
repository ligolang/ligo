(* Bitwise and *)
let b_and         = 0x0005 land 0x0106 (* 0x0004 *)

(* Bitwise or *)
let b_or          = 0x0005 lor  0x0106 (* 0x0107 *)

(* Bitwise xor *)
let b_xor         = 0x0005 lxor 0x0106 (* 0x0103 *)

(* Bitwise shift left *)
let b_shift_left  = 0x06   lsl  8n     (* 0x0600 *)

(* Bitwise shift right *)
let b_shift_right = 0x0006 lsr  1n     (* 0x0003 *)