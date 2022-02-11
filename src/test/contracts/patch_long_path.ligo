(* testing patches of nested structures involving record ; maps and sets *)

const w1 : record [ x : record [ a : int ; b : int] ; y : string ] = {
  var loc_v := record [ x = record [ a = -1 ; b = -2 ] ; y = "y"] ;
  patch loc_v.x with record [ a = 1 ; b = 2 ] ;
} with loc_v

const w2 : map (string, record [ x : record [ a : int ; b : string] ]) = {
  const v = record [ x = record [ a = 1 ; b = "one"] ] ;
  var loc_v := map [ "foo" -> v ; "bar" -> v] ;
  patch (loc_v["foo"]).x with record [ a = 12 ; b = "douze"] ;
} with loc_v

const w3 : record [ x : map(string,int) ] = {
  var loc_v := record [ x = map ["one" -> 1]] ;
  patch loc_v.x with map [ "two" -> 2 ; ] 
} with loc_v

const w4 : map (string, map (string, int)) = {
  var loc_v := map [ "x" -> map ["one" -> 1 ; "two" -> 2] ; "y" -> map [ "four" -> 4 ; "five" -> 5] ] ;
  patch loc_v["x"] with map [ "three" -> 3 ]
} with loc_v

const w5 : map (string, map(string, record [ x : record [ a : string ; b :string ] ])) = {
  var loc_v := map [ "foo" -> map [ "bar" -> record [ x = record [ a = "X" ; b = "b"] ] ] ] ;
  patch (loc_v["foo"]["bar"]).x with record [ a = "a" ] ;
} with loc_v

const w6 : record [ a : int ; b : string] = {
  var loc_v := record [ a = 2 ; b = "b" ] ;
  patch loc_v with record [ a = 1 ] ;
} with loc_v

const w7 : map (string, int) = {
  var loc_v := map [ "a" -> -1 ; "b" -> 2] ;
  patch loc_v with map [ "c" -> 3 ; "a" -> 1 ] ;
} with loc_v

const w8 : map (string , set (string)) = {
  var loc_v := map [ "foo" -> set ["a"] ;  "bar" -> set [ "c" ; "d" ] ] ;
  patch loc_v["foo"] with set [ "b" ] ;
} with loc_v