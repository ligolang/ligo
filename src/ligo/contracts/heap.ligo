type heap is map(nat, heap_element) ;

function is_empty (const h : heap) : bool is
  block {skip} with size(h) = 0n

function get_top (const h : heap) : heap_element is
  block {skip} with get_force(1n, h)

function pop_switch (const h : heap) : heap is
  block {
   const result : heap_element = get_top (h) ;
   const s : nat = size(h) ;
   const last : heap_element = get_force(s, h) ;
   remove 1n from map h ;
   h[1n] := last ;
  } with h

// function largest_child (const h : heap) : nat is
//   block {
//    const result : heap_element = get_top (h) ;
//    const s : nat = size(h) ;
//    var current : heap_element := get_force(s, h) ;
//    const i : nat = 1n ;
//    const left : nat = 2n * i ;
//    const right : nat = left + 1n ;
//    remove 1n from map h ;
//    h[1n] := current ;
//    var largest : nat := i ;
//    if (left <= s and heap_element_lt(get_force(s , h) , get_force(left , h))) then
//      largest := left
//    else if (right <= s and heap_element_lt(get_force(s , h) , get_force(right , h))) then
//      largest := right
//    else skip
//   } with largest
