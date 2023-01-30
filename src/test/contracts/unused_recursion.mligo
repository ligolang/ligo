type t = { foo : int ; bar : int }

let coucou = fun (storage : t) : t ->
  let number = 2 in
  
  let id (x : int) : int = x in
  
  (* parameter shadows fun_name: simple *)
  let rec toto : int -> int = fun (toto:int) : int -> let number = toto in number + 1 in

  (* parameter shadows fun_name: complex *)
  let rec foo : (int -> int) -> int = fun (foo : (int -> int)) -> let foo = foo 0 in foo in
  
  (* fun_name shadowed in body *)
  let rec bar : int -> t = fun (x : int) ->
    let bar = x in
    { storage with bar = bar }
  in
  
  let n = toto (number)  + foo (id) in
  
  bar n 

let main (_ : unit) (storage : t) : (operation list * t) =
 ([] : operation list), coucou storage