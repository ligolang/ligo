(* Persistent implementation of Union/Find with height-balanced
   forests and without path compression: O(n*log(n)).

   In the definition of type [t], the height component is that of the
   source, that is, if [ItemMap.find i m = (j,h)], then [h] is the
   height of [i] (_not_ [j]).
*)

module Make (Item: Partition.Item) =
  struct

    type item = Item.t
    type repr = item   (** Class representatives *)

    let equal i j = Item.compare i j = 0

    module ItemMap = Map.Make (Item)

    type height = int

    type partition = (item * height) ItemMap.t
    type t = partition

    let empty = ItemMap.empty

    let rec seek (i: item) (p: partition) : repr * height =
      let j, _ as i' = ItemMap.find i p in
      if equal i j then i' else seek j p

    let repr item partition = fst (seek item partition)

    let is_equiv (i: item) (j: item) (p: partition) =
      equal (repr i p) (repr j p)

    let get_or_set (i: item) (p: partition) =
      try seek i p, p with
        Not_found -> let i' = i,0 in (i', ItemMap.add i i' p)

    let equiv (i: item) (j: item) (p: partition) : partition =
      let (ri,hi), p = get_or_set i p in
      let (rj,hj), p = get_or_set j p in
      let add = ItemMap.add in
      if   equal ri rj
      then p
      else if   hi > hj
           then add rj (ri,hj) p
           else add ri (rj,hi) (if hi < hj then p else add rj (rj,hj+1) p)

    let alias (i: item) (j: item) (p: partition) : partition =
      let (ri,hi), p = get_or_set i p in
      let (rj,hj), p = get_or_set j p in
      let add = ItemMap.add in
      if   equal ri rj
      then p
      else if   hi = hj || equal ri i
           then add ri (rj,hi) @@ add rj (rj, max hj (hi+1)) p
           else if hi < hj then add ri (rj,hi) p
                           else add rj (ri,hj) p

    (* Printing *)

    let print (p: partition) =
      let print i (j,hi) =
        let _,hj = ItemMap.find j p in
        Printf.printf "%s,%d -> %s,%d\n"
          (Item.to_string i) hi (Item.to_string j) hj
      in ItemMap.iter print p

  end
