(* testing assignments of nested structures involving record and maps *)

const a1 : map (string, record [ x : record [ a : int ; b : int] ; y : string ]) = {
  var loc_v := map [ "foo" -> record [ x = record [ a = -1 ; b = -2 ] ; y = "y"]] ;
  (loc_v["foo"]).x := record [ a = 1 ; b = 2] ;
} with loc_v

const a2 : map (string, map(string, record [ x : record [ a : string ; b : string] ; y : int ]))= {
  var loc_v := map [ "foo" -> map [ "bar" -> record [ x = record [ a = "" ; b = ""] ; y = 2] ] ] ;
  (loc_v["foo"]["bar"]).x := record [ a = "a" ; b = "b" ] ;
} with loc_v

const a3 : record [ x : record [ a : map (string,record[a:int]) ; b : int ] ; y : int ]= {
  var loc_v := record [ x = record [ a = (map [] : map(string,record [ a : int ])) ; b = 2 ] ; y = 3 ] ;
  (loc_v.x.a)["foo"] := record [ a = 1] ;
} with loc_v

const a4 : map (string, record [ x : map (string,int) ; y : int ])= {
  var loc_v := map [ "foo" -> record [ x = map [ "foo" -> 1 ] ; y = 3 ] ] ;
  ((loc_v["foo"]).x)["bar"] := 2 ;
} with loc_v

(* those silently "fail": the assigned structure isn't modified *)

const af1 : map (string, record [ x : record [ a : int ; b : int] ; y : string ]) = {
  var loc_v := map [ "foo" -> record [ x = record [ a = -1 ; b = -2 ] ; y = "y"]] ;
  (loc_v["fooz"]).x := record [ a = 1 ; b = 2] ;
} with loc_v

const af2 : map (string, map(string, record [ x : record [ a : string ; b : string] ; y : int ]))= {
  var loc_v := map [ "foo" -> map [ "bar" -> record [ x = record [ a = "" ; b = ""] ; y = 2] ] ] ;
  (loc_v["fooz"]["barz"]).x := record [ a = "a" ; b = "b" ] ;
} with loc_v

(* the last accessor do not have the same behavior as the previous one (if it do not exist, we add to it) *)

const af3 : record [ x : record [ a : map (string,record[a:int]) ; b : int ] ; y : int ]= {
  var loc_v := record [ x = record [ a = (map [] : map(string,record [ a : int ])) ; b = 2 ] ; y = 3 ] ;
  (loc_v.x.a)["fooz"] := record [ a = 1] ;
} with loc_v

const af4 : map (string, record [ x : map (string,int) ; y : int ])= {
  var loc_v := map [ "fooz" -> record [ x = map [ "foo" -> 1 ] ; y = 3 ] ] ;
  ((loc_v["fooz"]).x)["barz"] := 2 ;
} with loc_v