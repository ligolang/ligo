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
(* Full Grammar *)
(* Generated Language *)

entry_point : arith EOF { $1 }

(* Singletons *)

variable : NAME
  {
    let loc = Location.make $startpos $endpos in
    Location.wrap ~loc $1
  }
  

(* Hierarchies *)

(* Top-level for arith *)
%inline arith : arith_0 { $1 }

arith_4 :
  (* Arith_variable *)
  | a = variable
    {
      let loc = Location.make $startpos $endpos in
      Location.wrap ~loc @@ Arith_variable (a)
    }  
arith_3 :
  (* List *)
  | LIST LSQUARE a = lead_list(SEMICOLON, arith_3) RSQUARE
    {
      let loc = Location.make $startpos $endpos in
      Location.wrap ~loc @@ List (a)
    }
  | arith_4 { $1 }  
arith_2 :
  (* Multiplication *)
  | a = arith_2 TIMES b = arith_3
    {
      let loc = Location.make $startpos $endpos in
      Location.wrap ~loc @@ Multiplication (a , b)
    }
  (* Division *)
  | a = arith_2 DIV b = arith_3
    {
      let loc = Location.make $startpos $endpos in
      Location.wrap ~loc @@ Division (a , b)
    }
  | arith_3 { $1 }  
arith_1 :
  (* Addition *)
  | a = arith_1 PLUS b = arith_2
    {
      let loc = Location.make $startpos $endpos in
      Location.wrap ~loc @@ Addition (a , b)
    }
  (* Substraction *)
  | a = arith_1 MINUS b = arith_2
    {
      let loc = Location.make $startpos $endpos in
      Location.wrap ~loc @@ Substraction (a , b)
    }
  | arith_2 { $1 }  
arith_0 :
  (* Let_in *)
  | LET a = variable EQUAL b = arith_0 IN c = arith_0
    {
      let loc = Location.make $startpos $endpos in
      Location.wrap ~loc @@ Let_in (a , b , c)
    }
  | arith_1 { $1 }
