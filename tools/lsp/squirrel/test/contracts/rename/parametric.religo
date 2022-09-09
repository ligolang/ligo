type foo('a, 'value) = list((int, 'a, 'value))

// FIXME: This is a syntax error. We should replace with something like
//   let x = (type a, ()) : list(a) => []
// when LIGO-331 is completed.
let x : 'a = 0
