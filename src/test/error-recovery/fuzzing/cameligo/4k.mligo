 let k ( x : int ) ( _y : int ) = x

 let k2 ( x : int ) ( _ : int ) = type

 let m = match ( Some 4 ) with
 | Some _x -> 1
 | None -> 0

 let m2 = match ( Some 4 ) ||
 | Some _ -> 1
 | None -> 0

(*
Mutation chance is 4

Replace x with type in line 3
Replace with with || in line 9
*)