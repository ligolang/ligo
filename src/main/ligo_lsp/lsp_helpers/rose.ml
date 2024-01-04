type 'a tree = Tree of 'a * 'a forest
and 'a forest = 'a tree list

let rec pp_tree : 'a Fmt.t -> 'a tree Fmt.t =
 fun pp_elt ppf (Tree (parent, children)) ->
  Format.fprintf ppf "Tree (%a, %a)" pp_elt parent (pp_forest pp_elt) children


and pp_forest : 'a Fmt.t -> 'a forest Fmt.t =
 fun pp_elt ppf forest -> Fmt.Dump.(list (pp_tree pp_elt)) ppf forest


let rec map_tree ~(f : 'a -> 'b) (Tree (parent, children) : 'a tree) : 'b tree =
  Tree (f parent, map_forest ~f children)


and map_forest ~(f : 'a -> 'b) : 'a forest -> 'b forest = List.map ~f:(map_tree ~f)

let forest_of_list ~(compare : 'a -> 'a -> int) ~(intersects : 'a -> 'a -> bool)
    : 'a list -> 'a forest
  =
  let ( <@ ) = Simple_utils.Function.( <@ ) in
  let rec group_by_head = function
    | [] -> []
    | hd :: tl ->
      let inside, outside = List.split_while ~f:(intersects hd) tl in
      (hd, inside) :: group_by_head outside
  in
  let rec go elts =
    List.map (group_by_head elts) ~f:(fun (max, inner) -> Tree (max, go inner))
  in
  go <@ List.sort ~compare
