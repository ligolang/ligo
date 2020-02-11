type root =
| A of rootA
| B of rootB
| C of string

and a = {
  a1 : ta1 ;
  a2 : ta2 ;
}

and ta1 =
| X of root
| Y of ta2

and ta2 =
| Z of ta2
| W of unit

and rootA =
  a list

and rootB =
  int list

let fold_list v state continue =
  let aux = fun (lst', state) elt ->
    let (elt', state) = continue elt state in
    (elt' :: lst' , state) in
  List.fold_left aux ([], state) v

let fold_option v state continue =
  match v with
    Some x -> continue x state
  | None -> None
