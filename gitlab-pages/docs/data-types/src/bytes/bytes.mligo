let a : bytes = 0x70FF
let zero : bytes = 0x
let zero_too = 0x00
let b : bytes = bytes 123n   // 7B in hexadecimal
let c : bytes = bytes 123
let d : bytes = bytes (-123) // Two's complement

let n : nat = nat 0x7B // n = 123n
let i : int = int 0x7B // i = 123
let from_ascii : bytes = [%bytes "foo"]
// raw = from_ascii
let raw : bytes = ("666f6f" : bytes)