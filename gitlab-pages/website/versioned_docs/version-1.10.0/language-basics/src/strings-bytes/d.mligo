let white : bytes = 0xffff
let black : bytes = 0x0000
let pixels : bytes = Bytes.concat white black (* 0xffff0000 *)