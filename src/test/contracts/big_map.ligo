type storage_ is big_map(int, int) * unit

function main(const p : unit; const s : storage_) : list(operation) * storage_ is
  var r : big_map(int, int) := s.0 ;
  var toto : option (int) := Some(0);
  block {
    toto := r[23];
    r[2] := 444;
    s.0 := r;
  }
  with ((nil: list(operation)), s)

function set_ (var n : int ; var m : storage_) : storage_ is block {
  var tmp : big_map(int,int) := m.0 ;
  tmp[23] := n ;
  m.0 := tmp ;
} with m

function rm (var m : storage_) : storage_ is block {
  var tmp : big_map(int,int) := m.0 ;
  remove 42 from map tmp;
  m.0 := tmp;
} with m

function gf (const m : storage_) : int is begin skip end with get_force(23, m.0)

function get (const m : storage_) : option(int) is
  begin
    skip
  end with m.0[42]