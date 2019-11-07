// int + int produces int
const a: int = 5 + 10;
// nat + int produces int
const b: int = 5n + 10;
// tez + tez produces tez
const c: tez = 5mutez + 10mutez;
// you can't add tez + int or tez + nat, this won't compile
// const d: tez = 5mutez + 10n;
// two nats produce a nat
const e: nat = 5n + 10n;
// nat + int produces an int, this won't compile
// const f: nat = 5n + 10;
const g: int = 1_000_000;