type optioni = int option
type myti = Nili | Consi of optioni

let fl = fun (x:myti) -> 1
let fo = fun (x:optioni) -> 2

let t10 = fun (x: myti) (y: myti) ->
  match x,y with
  | Nili , ys  -> fl ys
  | xs  , Nili -> fl xs
  | Consi (None) , Consi (Some b) ->
    let b = 1 in b
  | Consi (a) , Consi (b) ->
    (* testing that subtitution is stoping on resursive definitions *)
    let rec a (b : int) : int =let x = fo a in b + 1 in
    (a 1) + (fo b)