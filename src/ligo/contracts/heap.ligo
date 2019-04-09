type heap_element is int * string ;

type heap is map(int, heap_element) ;

function is_empty (const h : heap) : bool is
  block {skip} with size(h) = 0n
