type heap_element is int * string

function heap_element_lt(const x : heap_element ; const y : heap_element) : bool is
  block { skip } with x.0 < y.0

#include "heap.ligo"
