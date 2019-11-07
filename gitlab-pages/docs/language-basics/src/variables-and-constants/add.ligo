// won't work, use const for global values instead
// var four: int = 4;

function add(const a: int; const b: int) : int is
    block { 
        var c : int := a + b;
     } with c