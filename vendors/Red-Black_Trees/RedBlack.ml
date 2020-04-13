(* Red-black trees according to the following classic paper:

   Chris Okasaki, Red-Black Trees in a Functional
   Setting. J. Funct. Program. 9(4): 471-477 (1999)
*)

type colour = Red | Black

type 'a t =
  Ext
| Int of colour * 'a t * 'a * 'a t

let empty = Ext

let is_empty m = (m = empty)

let blacken = function
                         Ext -> Ext
| Int (_, left, root, right) -> Int (Black, left, root, right)

let balance colour left root right =
  match colour, left, root, right with
    Black, Int (Red, Int (Red, a, x, b), y, c), z, d
  | Black, Int (Red, a, x, Int (Red, b, y, c)), z, d
  | Black, a, x, Int (Red, Int (Red, b, y, c), z, d)
  | Black, a, x, Int (Red, b, y, Int (Red, c, z, d)) ->
      Int (Red, Int (Black, a, x, b), y, Int (Black, c, z, d))
  | _ -> Int (colour, left, root, right)

type choice = Old | New

let choose ~old ~new' = function
  Old -> old
| New -> new'

exception Physical_equality

let add ~cmp choice elt tree =
  let rec insert = function
    Ext -> Int (Red, Ext, elt, Ext)  (* A leaf *)
  | Int (colour, left, root, right) ->
      let diff = cmp elt root in
      if diff = 0 then
        let root' = choose ~new':elt ~old:root choice
        in if root == root' then raise Physical_equality
           else Int (colour, left, root', right)
      else if diff < 0 then
             balance colour (insert left) root right
           else balance colour left root (insert right)
  in try blacken (insert tree) with
       Physical_equality -> tree

exception Not_found

let rec find ~cmp elt = function
  Ext -> raise Not_found
| Int (_, left, root, right) ->
    let diff = cmp elt root in
    if diff = 0 then root
    else if diff < 0 then find ~cmp elt left
         else find ~cmp elt right

let find_opt ~cmp elt tree =
  try Some (find ~cmp elt tree) with Not_found -> None

(* Inorder iterators *)

let rec iter f = function
                         Ext -> ()
| Int (_, left, root, right) -> iter f left; f root; iter f right

let rec inorder acc = function
                         Ext -> acc
| Int (_, left, root, right) -> inorder (root :: inorder acc right) left

let elements t = inorder [] t

let rec fold_inc f ~init = function
                         Ext -> init
| Int (_, left, root, right) ->
    fold_inc f ~init:(f ~elt:root ~acc:(fold_inc f ~init left)) right

let rec fold_dec f ~init = function
                         Ext -> init
| Int (_, left, root, right) ->
    fold_dec f ~init:(f ~elt:root ~acc:(fold_dec f ~init right)) left
