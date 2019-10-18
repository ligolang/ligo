type storage_ is big_map(int, int) * unit

function main(const p : unit; const s : storage_) : list(operation) * storage_ is
  var toto : option (int) := Some(0);
  block {
    toto := s.0[23];
    s.0[2] := 444;
  }
  with ((nil: list(operation)), s)

function set_ (var n : int ; var m : storage_) : storage_ is block {
  m.0[23] := n ;
} with m

function rm (var m : storage_) : storage_ is block {
  remove 42 from map m.0;
} with m

function gf (const m : storage_) : int is begin skip end with get_force(23, m.0)

function get (const m : storage_) : option(int) is begin skip end with m.0[42]

function mutimaps (const m : storage_; const n : storage_) : storage_ is block
{
  var foo : big_map(int,int) := m.0 ;
  foo[42] := 0 ;
  n.0[42] := get_force(42, foo) ; 
} with n

const empty_big_map : big_map(int,int) = big_map end

const map1 : big_map(int,int) = big_map
  23 -> 0 ;
  42 -> 0 ;
end