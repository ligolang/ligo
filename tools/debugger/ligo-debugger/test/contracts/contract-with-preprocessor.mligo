let foo (a, b : int * int) : int =
#if SOME_DEFINE
  42
#else
  a + b
#endif

let main (_, s : unit * int) : operation list * int =
  (([] : operation list), foo(s, s + 42))
