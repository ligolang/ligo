function main (const _ : unit; const s : int) : list(operation) * int is block {
  var s2 : int := 0;
  if (s > 10) then {
    s2 := s * 2;
  } else if (s > 20) then {
    s2 := s * 4;
  } else {
    s2 := s / 2;
  }
} with ((list [] : list(operation)), s2)
