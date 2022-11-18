type storage_t is record [
    some_map : map (int,nat)
]
type return_t is (list (operation) * storage_t)
type params_t is nat

function main(
  const params        : list(params_t);
  var s               : storage_t)
                      : return_t is
  block {
    var operations := nil;
    for param in list params {
      s.some_map[0] := param
    };
    const other_map : map(int,nat) = Map.empty;
    for key -> val in map other_map {
      (* param was accidentally still in the typing context after this point *)
      s.some_map[0] := param;
    };
  } with (operations, s)