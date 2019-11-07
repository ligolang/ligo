const a: int = 5 - 10;
// substraction of two nats, yields an int
const b: int = 5n - 2n;
// won't compile, result is an int, not a nat
// const c: nat = 5n - 2n;
const d: tez = 5mutez - 1mutez;