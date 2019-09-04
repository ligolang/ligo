// type storage_ is big_map(int, int)
type storage_ is big_map(int, int) * unit

function main(const p : unit; const s : storage_) : list(operation) * storage_ is
  var r : big_map(int, int) := s.0 ;
  var toto : option (int) := Some(0);
  block {
    toto := r[23];
    s.0 := r;
  }
  with ((nil: list(operation)), s)