let b1 = ([] : int list) && "soo"

let b2 = (Set.literal [1]) || b1

let bnat = not 0n && 1n

let bint = not 0 && 1 && (0 - 1)

let btez = not 0tez && 1tez

let bstring = not "" && "foo"

let bbytes = not (Bytes.sub 0n 0n (0x00 : bytes) : bool) && (0x00 : bytes)

let blist = not ([] : int list) && [1]

let bset = not (Set.empty : int set) && (Set.literal [1])

let bmap = not (Map.empty : (int, int) map) && (Map.literal [(1, 1)])

let ball = bnat && bint && btez && bstring && bbytes && blist && bset && bmap
