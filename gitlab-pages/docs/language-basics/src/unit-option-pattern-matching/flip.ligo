type coin is Head | Tail

function flip (const c : coin) : coin is
  case c of
    Head -> Tail (Unit) // Unit needed because of a bug
  | Tail -> Head (Unit) // Unit needed because of a bug
  end
