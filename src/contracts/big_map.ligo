// type storage_ is big_map(int, int) * unit
type storage_ is big_map(int, int)

// function main(const p : unit; const s : storage_) : list(operation) * storage_ is
//   block { skip }
//   with ((nil : list(operation)), s)

function main(const p : unit; const s : storage_) : list(operation) * storage_ is
  // var r : big_map(int, int) := s.0 ;
  var r : big_map(int,int) := s ;
  var toto : option (int) := Some(0);
  block {
    // r[23] := 2;
    toto := r[23];
    s := r;
    // skip
  }
  with ((nil: list(operation)), s)