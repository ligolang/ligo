let f = (a: int): int => {
    let x = a + 100;
    let x2 = x + 100;
    x2
}
let g = (): int =>
    let y = 10 + 10;
    y

let main = ((_, s) : (unit, int)) : (list(operation), int) =>
  let s2 = s + 1;
  let s3 = g() + f(s2);
  (([] : list(operation)), s3)
