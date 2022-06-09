// Test the pattern matching functionality of PascaLIGO

function match_bool (const i : int) : int is
  {
    var result : int := 23;
    case i = 2 of [
      True  -> result := 42
    | False -> result := 0
    ]
  } with result

function match_bool_old (const i : int) : int is
  {
    var result : int := 23;
    case i = 2 of [
      True  -> result := 42
    | False -> result := 0
    ]
  } with result

function match_option (const o : option (int)) : int is
  {
    var result : int := 23;
    case o of [
      None -> skip
    | Some (s) -> result := s
    ]
  } with result

function match_expr_bool (const i : int) : int is
  case i = 2 of [
    True -> 42
  | False -> 0
  ]

function match_expr_option (const o : option (int)) : int is
  case o of [
    None -> 42
  | Some (s) -> s
  ]

function match_expr_list (const l : list (int)) : int is
  case l of [
    nil -> -1
  | hd # _tl -> hd
  ]
