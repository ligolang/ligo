// Bitwise "and"
let and : bytes = 0x0005 land 0x0106 // 0x0004

// Bitwise "or"
let @or : bytes = 0x0005 lor 0x0106 // 0x0107

// Bitwise "xor"
let xor : bytes = 0x0005 lxor 0x0106 // 0x0103

// Bitwise "shift left"
let shift_left : bytes = 0x06 lsl 8n // 0x0600

// Bitwise "shift right"
let shift_right : bytes = 0x0006 lsr 1n // 0x0003