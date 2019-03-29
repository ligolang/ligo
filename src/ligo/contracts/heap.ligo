type heap_element is int * string

type heap is record
  heap_content : map(int, heap_element) ;
  heap_size : nat ;
end

function is_empty (const h : heap) : bool is
  block {skip} with h.heap_size = 0
