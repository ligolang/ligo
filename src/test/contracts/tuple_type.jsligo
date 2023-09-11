/*
  The difference between tuples and arguments is subtle in JsLIGO.

   `f(a, b);`
   f is called with two arguments

   `f((a, b));`
   f is called with a tuple.

*/

type fun_type = (a: int, b: int) => int

const arguments = (b: int, c: int) : int => b + c

const arguments_type_def = (b: fun_type) : int => b (5, 3)

const arguments_test = (_: int) : int => arguments_type_def (arguments)

type tuple_type = (a: [int, int]) => int

const tuple = ([a, b]: [int, int]) : int => a + b

const tuple_type_def = (b: tuple_type) : int => b ([5, 3])

const tuple_test = (_: int):int => tuple_type_def (tuple)

/* inline */

const arguments_inline = (b: int, c: int) : int => b + c

const arguments_type_def_inline = (b: (a: int, b: int) => int) : int => b(5, 3)

const arguments_test_inline = (_: int) : int =>
   arguments_type_def_inline (arguments_inline);

function tuple_inline ([a, b]: [int, int]) : int { return a + b }

const tuple_type_def_inline = (b: (a: [int, int]) => int) : int => b ([5, 3])

const tuple_test_inline = (_: int) : int =>
  tuple_type_def_inline(tuple_inline)
