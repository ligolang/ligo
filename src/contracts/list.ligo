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
