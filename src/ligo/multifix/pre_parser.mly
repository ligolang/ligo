%{
    open Ast
%}

%start <Ast.entry_point> entry_point

%%

naked_list(X):
  | { [] }
  | x = X xs = naked_list(X) { x :: xs }

naked_list_ne(X):
  | x = X { [ x ] }
  | x = X xs = naked_list_ne(X) { x :: xs }

trail_list(separator, X):
  | { [] }
  | trail_list_content(separator, X) { $1 }

trail_list_content(separator, X):
  | x = trail_list_last(separator, X) { x }
  | x = X separator xs = trail_list_content(separator, X) { x :: xs }

trail_list_last(separator, X):
  | x = X option(separator) { [ x ] }

trail_force_list(separator, X):
  | { [] }
  | x = X separator xs = trail_force_list(separator, X) { x :: xs }

trail_force_list_ne(separator, X):
  | x = X separator { [ x ] }
  | x = X separator xs = trail_force_list_ne(separator, X) { x :: xs }

trail_option_list(separator, X):
  | { [] }
  | trail_option_list_content(separator, X) { $1 }

trail_option_list_content(separator, X):
  | x = trail_option_list_last(separator, X) { x }
  | x = X option(separator) xs = trail_option_list_content(separator, X) { x :: xs }

trail_option_list_last(separator, X):
  | x = X option(separator) { [ x ] }

lead_list_ne(separator, X):
  | separator x = X { [x] }
  | separator x = X xs = lead_list_ne(separator, X) { x :: xs }

lead_list(separator, X):
  | { [] }
  | lead_list_content(separator, X) { $1 }

lead_list_content(separator, X):
  | x = lead_list_first(separator, X) { x }
  | xs = lead_list_content(separator, X) separator x = X { xs @ [ x ] }

lead_list_first (separator, X):
  | option(separator) x = X { [ x ] }

%inline wrap(X):
  | x = X { let loc = Location.make $startpos $endpos in Location.wrap ~loc x }
