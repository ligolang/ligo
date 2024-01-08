open Simple_utils

(** A rose tree is a tree where each node has an arbitrary amount of children (zero or
    more). This implementation is a bit unconventional in the sense that each node may
    hold one or more elements. Those are the elements that compared as equal by
    [forest_of_list], which will be grouped together. *)
type 'a tree = Tree of 'a List.Ne.t * 'a forest

(** A rose forest is nothing more than a list of rose trees. *)
and 'a forest = 'a tree list

(** Pretty print a rose tree for debugging. *)
let rec pp_tree : 'a Fmt.t -> 'a tree Fmt.t =
 fun pp_elt ppf (Tree (parent, children)) ->
  Format.fprintf
    ppf
    "Tree (%a, %a)"
    Fmt.Dump.(list pp_elt)
    (List.Ne.to_list parent)
    (pp_forest pp_elt)
    children


(** Pretty print a rose forest for debugging. *)
and pp_forest : 'a Fmt.t -> 'a forest Fmt.t =
 fun pp_elt ppf forest -> Fmt.Dump.(list (pp_tree pp_elt)) ppf forest


(** Apply [f] to every element of the tree. *)
let rec map_tree ~(f : 'a -> 'b) (Tree (parent, children) : 'a tree) : 'b tree =
  Tree (List.Ne.map f parent, map_forest ~f children)


(** Apply [f] to every element of the forest. *)
and map_forest ~(f : 'a -> 'b) : 'a forest -> 'b forest = List.map ~f:(map_tree ~f)

(** Create an ordered rose forest from the given list.
      [compare] is similar to any ordinary comparison function but with a caveat that must
    be satisfied: the elements should be ordered lexicographically, but ranged elements
    whose start points compare as equal should be ordered such that the largest one
    compares with [-1] (or [0] if they are the same size), and the smallest with [1].
      [intersects] is a function that should return [true] if any of the two ranges are
    inside the other, or [false] otherwise. The first range is guaranteed to either start
    before the second, or start together with the second but be greater than the second,
    as a consequence of [compare].
      The resulting rose forest will be one such that every range will be nested
    accordingly, and equal elements will be grouped together.
      This function assumes that, for any pair of ranges, either one of them will be
    completely inside the other, or completely outside the other. In other words, one
    range may not be partially contained in another. *)
let forest_of_list ~(compare : 'a -> 'a -> int) ~(intersects : 'a -> 'a -> bool)
    : 'a list -> 'a forest
  =
  let ( <@ ) = Simple_utils.Function.( <@ ) in
  let rec group_by_head : 'a list -> ('a List.Ne.t * 'a list) list = function
    | [] -> []
    | hd :: tl ->
      let inside, outside = List.split_while ~f:(intersects hd) tl in
      let equal, not_equal = List.split_while ~f:(Int.equal 0 <@ compare hd) inside in
      ((hd, equal), not_equal) :: group_by_head outside
  in
  let rec go elts =
    List.map (group_by_head elts) ~f:(fun (max, inner) -> Tree (max, go inner))
  in
  go <@ List.sort ~compare


let%expect_test "Creates a rose forest out of ranges" =
  (*      0-10
         / |  \
      1-4 6-7  8-10
     /   \      |
  1-2     3-4  8-10
                   \
                    9-10 *)
  let input = [ 8, 10; 3, 4; 1, 4; 9, 10; 8, 10; 1, 2; 0, 10; 6, 7 ] in
  let forest =
    forest_of_list
      ~compare:(fun (x1, y1) (x2, y2) ->
        (* We need to define that [a < b] if [a] contains [b]. Since e.g. interval [1, 4]
           contains [1, 2] and [3, 4], then [(1, 4) < (1, 2)]. *)
        let ord = compare x1 x2 in
        if ord = 0 then compare y2 y1 else ord)
      ~intersects:(fun (_x1, y1) (x2, _y2) ->
        (* The first point comes before the second, or they start at the same place but
           the first is bigger, so we can just check that the first's end is after the
           second's start. *)
        compare y1 x2 > 0)
      input
  in
  Format.printf
    "%a\n"
    (pp_forest (fun ppf (x, y) -> Format.fprintf ppf "%d, %d" x y))
    forest;
  [%expect
    {|
    [Tree ([0, 10], [Tree ([1, 4], [Tree ([1, 2], []); Tree ([3, 4], [])]);
                     Tree ([6, 7], []);
                     Tree ([8, 10; 8, 10], [Tree ([9, 10], [])])])] |}]
