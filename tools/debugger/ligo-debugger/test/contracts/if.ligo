function main (const _ : unit; const s :int) : list(operation) * int is block {
  var s2 := 0;
  if (s > 10) then {
    s2 := s * 2;
  } else {
    s2 := s / 2;
  }
} with ((list [] : list(operation)), s2)
