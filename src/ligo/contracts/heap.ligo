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
  } with h ;


// function pop (const h : heap) : heap is
//   block {
//    const result : heap_element = get_top (h) ;
//    const s : nat = size(h) ;
//    const last : heap_element = get_force(int(s), h) ;
//    remove 1 from map h ;
//    h[1] := last ;
//   } with h ;
