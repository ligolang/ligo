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