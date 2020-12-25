(* Rope tracking some information (total string length, …) *)
module Rope = struct
  type ('a, 'info) rope = Empty | Leaf of 'info * 'a | Pair of ('info * ('a, 'info) rope * ('a, 'info) rope)
  let empty = Empty
  let info ~default = function
    | Empty               -> default
    | Leaf (info, _)      -> info
    | Pair (info, _x, _y) -> info
  let rope ~info x = Leaf (info x, x)
  let pair ~merge ~default x y =
    let merged_infos = merge (info ~default x) (info ~default y) in
    Pair (merged_infos, x, y)
  let rec list_of_rope : 'a 'info . 'a list -> ('a, 'info) rope -> 'a list = fun acc -> function
      Empty -> acc
    | Leaf (_info, x) -> x :: acc
    | Pair (_info1, Empty, x) -> list_of_rope acc x
    | Pair (_info1, Leaf (_info2, x), y) -> list_of_rope (x :: acc) y
    | Pair ( info1, Pair (_info2, x, y), z) ->
      (* info1 is just a dummy value for the recursive call, it is not
         used and there is no need to recompute the info for the
         remaining of the rope. *)
      list_of_rope acc (Pair (info1, x, Pair (info1, y, z)))
  let list_of_rope r = List.rev @@ list_of_rope [] r
  let rope_of_list ~merge ~default ~info l =
    List.fold_left
      (fun r x -> pair ~merge ~default r (rope ~info x))
      empty
      l
end

module SimpleRope = struct
  open Rope
  let empty = empty
  let rope x = rope ~info:(fun _ -> ()) x
  let pair x y = pair ~merge:(fun () () -> ()) ~default:() x y
  let list_of_rope = list_of_rope
  let rope_of_list l = rope_of_list ~merge:(fun () () -> ()) ~default:() ~info:(fun _ -> ()) l
end
