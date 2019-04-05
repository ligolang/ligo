%{
    open Ast
%}

%start <Ast.entry_point Location.wrap> entry_point

%%

trail_list(separator, X):
  | { [] }
  | trail_list_content(separator, X) { $1 }

trail_list_content(separator, X):
  | x = trail_list_last(separator, X) { x }
  | x = X separator xs = trail_list_content(separator, X) { x :: xs }

trail_list_last(separator, X):
  | x = X option(separator) { [ x ] }

lead_list(separator, X):
  | { [] }
  | lead_list_content(separator, X) { $1 }

lead_list_content(separator, X):
  | x = lead_list_first(separator, X) { x }
  | xs = lead_list_content(separator, X) separator x = X { xs @ [ x ] }

lead_list_first (separator, X):
  | option(separator) x = X { [ x ] }
