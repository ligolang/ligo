type foobar is list(int)

const fb : foobar = list
  23 ;
  42 ;
end

function size_ (const m : foobar) : nat is
  block {skip} with (size(m))

// function hdf (const m : foobar) : int is begin skip end with hd(m)

const bl : foobar = list
  144 ;
  51 ;
  42 ;
  120 ;
  421 ;
end

function iter_op (const s : list(int)) : int is
  var r : int := 0 ;
  function aggregate (const i : int) : unit is
  begin
    r := r + i ;
  end with unit
  begin
    list_iter(s , aggregate) ;
  end with r
