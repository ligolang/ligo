include PolySet

let alias ~demoted_repr ~new_repr s =
  if PolySet.mem demoted_repr s then
    if PolySet.mem new_repr s then
      PolySet.remove demoted_repr s
    else
      PolySet.add new_repr (PolySet.remove demoted_repr s)
  else
    s
