type t is (nat * nat)
type s is map(t)      

function main (const u : unit; const s : s) : (list(operation) * s) is ((nil : list(operation)), s)
