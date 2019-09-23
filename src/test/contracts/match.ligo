// Test the pattern matching functionality of PascaLIGO

function match_bool (const i : int) : int is
  var result : int := 23 ;
  begin
  case i = 2 of
  | True -> result := 42
  | False -> result := 0
  end
  end with result

function match_option (const o : option(int)) : int is
  var result : int := 23 ;
  begin
  case o of
  | None -> skip
  | Some (s) -> result := s
  end
  end with result

function match_expr_bool (const i : int) : int is
  begin skip end with
  case i = 2 of
  | True -> 42
  | False -> 0
  end

function match_expr_option (const o : option(int)) : int is
  begin skip end with
  case o of
  | None -> 42
  | Some (s) -> s
  end

function match_expr_list (const l : list(int)) : int is
  begin skip end with
  case l of
  | nil -> -1
  | hd # tl -> hd
  end
