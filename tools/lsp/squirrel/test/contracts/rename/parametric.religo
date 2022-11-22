type foo('a, 'value) = list((int, 'a, 'value))

let x = (type a, ()) : list(a, int) => []
