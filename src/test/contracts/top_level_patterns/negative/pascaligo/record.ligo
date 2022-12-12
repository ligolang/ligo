type r is record[ a : nat ; b : int ; c : string ]

const r = record[ a = 1n ; b = 1 ; c = "Hello" ]
const record[ a ; b = a ; c ] = r