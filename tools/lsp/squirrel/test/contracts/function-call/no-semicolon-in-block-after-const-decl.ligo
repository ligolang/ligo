type heap is map (nat, heap_elt)

function bar (const h : heap) : heap_elt is get_force (1n, h)

function pop (const h : heap) : heap * heap_elt * nat is
  block {
    const result : heap_elt = bar ()
    var c : nat := 0n;
  } with (h, result, c)
