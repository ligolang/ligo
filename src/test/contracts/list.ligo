// Test list type and related built-in functions in PascaLIGO

type foobar is list(int)

const fb : foobar = list
  23 ;
  42 ;
end

const fb2 : foobar = 144 # fb

const fb3 : foobar = cons(688 , fb2)

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
  begin
    var r : int := 0 ;
    function aggregate (const i : int) : unit is
      begin
        r := r + i ;
      end with unit ;
    list_iter(s , aggregate) ;
  end with r

function map_op (const s : list(int)) : list(int) is
  block {
    function increment (const i : int) : int is block { skip } with i + 1
  } with list_map(s , increment)
