type storage_ is big_map(int, int) * unit
type foo is big_map(int, int)

function main(const p : unit; const s : storage_) : list(operation) * storage_ is
  block {
    var toto : option (int) := Some(0);
    toto := s.0[23];
    s.0[2] := 444;
  }
  with ((nil: list(operation)), s)

function set_ (var n : int ; var m : foo) : foo is block {
  m[23] := n ;
} with m

function add (var n : int ; var m : foo) : foo is set_(n,m)

function rm (var m : foo) : foo is block {
  remove 42 from map m;
} with m

function gf (const m : foo) : int is begin skip end with get_force(23, m)

function get (const m : foo) : option(int) is begin skip end with m[42]

const empty_big_map : big_map(int,int) = big_map end

const big_map1 : big_map(int,int) = big_map
  23 -> 0 ;
  42 -> 0 ;
end

function mutimaps (const m : foo ; const n : foo) : foo is block
{
  var bar : foo := m ;
  bar[42] := 0 ;
  n[42] := get_force(42, bar) ;
} with n
