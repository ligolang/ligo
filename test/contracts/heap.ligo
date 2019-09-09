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

function pop_ (const h : heap) : nat is
  begin
   const result : heap_element = get_top (h) ;
   const s : nat = size(h) ;
   var current : heap_element := get_force(s, h) ;
   const i : nat = 1n ;
   const left : nat = 2n * i ;
   const right : nat = left + 1n ;
   remove 1n from map h ;
   h[1n] := current ;
   var largest : nat := i ;
   if (left <= s and heap_element_lt(get_force(s , h) , get_force(left , h))) then
     largest := left
   else if (right <= s and heap_element_lt(get_force(s , h) , get_force(right , h))) then
     largest := right
   else skip
  end with largest

function insert (const h : heap ; const e : heap_element) : heap is
  begin
    var i : nat := size(h) + 1n ;
    h[i] := e ;
    var largest : nat := i ;
    var parent : nat := 0n ;
    while (largest =/= i) block {
      parent := i / 2n ;
      largest := i ;
      if (parent >= 1n) then block {
        if (heap_element_lt(get_force(parent , h) , get_force(i , h))) then block {
          largest := parent ;
          const tmp : heap_element = get_force(i , h) ;
          h[i] := get_force(parent , h) ;
          h[parent] := tmp ;
        } else skip
      } else skip
    }
  end with h

function pop (const h : heap) : (heap * heap_element * nat) is
  begin
   const result : heap_element = get_top (h) ;
   var s : nat := size(h) ;
   const last : heap_element = get_force(s, h) ;
   remove s from map h ;
   h[1n] := last ;
   s := size(h) ;
   var i : nat := 0n ;
   var largest : nat := 1n ;
   var left : nat := 0n ;
   var right : nat := 0n ;
   var c : nat := 0n ;
   while (largest =/= i) block {
     c := c + 1n ;
     i := largest ;
     left := 2n * i ;
     right := left + 1n ;
     if (left <= s) then begin
       if (heap_element_lt(get_force(left , h) , get_force(i , h))) then begin
        largest := left ;
        const tmp : heap_element = get_force(i , h) ;
        h[i] := get_force(left , h) ;
        h[left] := tmp ;
       end else skip ;
     end else if (right <= s) then begin
       if (heap_element_lt(get_force(right , h) , get_force(i , h))) then begin
         largest := right ;
         const tmp : heap_element = get_force(i , h) ;
         h[i] := get_force(right , h) ;
         h[left] := tmp ;
       end else skip ;
     end else skip ;
  }
  end with (h , result , c)
