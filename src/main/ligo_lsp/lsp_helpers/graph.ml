open Simple_utils.Utils

module type S = sig
  (** The type of nodes in the graph. *)
  type vertex

  (** The abstract graph type. *)
  type t

  (** A graph with no nodes. *)
  val empty : t

  (** Check if the graph contains the given vertex. *)
  val mem : vertex -> t -> bool

  (** Add a node to the graph. *)
  val vertex : vertex -> t -> t

  (** Add various nodes to the graph. *)
  val vertices : vertex list -> t -> t

  (** Add an edge going from the first to the second node to the graph. *)
  val edge : vertex -> vertex -> t -> t

  (** Add various edges each going from the first to the second node to the graph. *)
  val from_assoc : (vertex * vertex) list -> t -> t

  (** Add various edges each going from the first to every list of the second nodes to the
      graph. *)
  val from_assocs : (vertex * vertex list) list -> t -> t

  (** Convert the graph to a list of nodes, each going from the first node to the list of
      second nodes. *)
  val to_assocs : t -> (vertex * vertex list) list

  (** Extract the list of nodes of the graph. *)
  val to_vertices : t -> vertex list

  (** Extract every incident child of a node, if found in the graph. *)
  val post : vertex -> t -> vertex list option

  (** Extract the subgraph of every reachable node, if found in the graph. *)
  val reachable : vertex -> t -> t option

  (** Reverse all the edges of the input graph. *)
  val transpose : t -> t

  (** Overlay a graph on top of another, performing their union. *)
  val overlay : t -> t -> t

  (** Create a double-edged graph from the input. *)
  val to_undirected : t -> t

  (** Get the Weakly Connected Components of the input graph. *)
  val wcc : t -> t list

  (** Pretty-print the graph, given a way to pretty-print a node. *)
  val pp : vertex Fmt.t -> t Fmt.t
end

module Make (Ord : Caml.Map.OrderedType) : S with type vertex = Ord.t = struct
  module Node_map = Caml.Map.Make (Ord)
  module Node_set = Caml.Set.Make (Ord)

  let set_to_list = Caml.List.of_seq <@ Node_set.to_seq

  type vertex = Ord.t
  type t = Node_set.t Node_map.t

  let empty = Node_map.empty
  let vertex node = Node_map.update node Option.(some <@ value ~default:Node_set.empty)
  let vertices vs g = List.fold vs ~init:g ~f:(Fn.flip vertex)
  let mem = Node_map.mem

  let edge from to' g =
    Node_map.update
      from
      Option.(some <@ value_map ~default:(Node_set.singleton to') ~f:(Node_set.add to'))
      (vertex to' g)


  let from_assoc vs g = List.fold vs ~init:g ~f:(fun g (v1, v2) -> edge v1 v2 g)

  let from_assocs edges g =
    List.fold edges ~init:g ~f:(fun g (node, nodes) ->
        List.fold nodes ~init:(vertex node g) ~f:(fun g v -> edge node v g))


  let to_assocs =
    Caml.List.of_seq <@ Seq.map (Tuple2.map_snd ~f:set_to_list) <@ Node_map.to_seq


  let to_vertices = List.map ~f:fst <@ to_assocs
  let post node g = Option.map (Node_map.find_opt node g) ~f:set_to_list

  (** Internal: exposed implementation of [reachable] that performs no check that [node]
      is in [g], and returns the set of visited vertices, taking the initial set of
      [vis]ited nodes and [acc]umulated reachability graph. *)
  let rec reachable_impl (vis : Node_set.t) (acc : t) (node : vertex) (g : t)
      : Node_set.t * t
    =
    if Node_set.mem node vis
    then vis, acc
    else
      Node_set.fold
        (fun child (vis, acc) -> reachable_impl vis (edge node child acc) child g)
        (Node_map.find node g)
        (Node_set.add node vis, acc)


  let reachable node g =
    if mem node g then Some (snd @@ reachable_impl Node_set.empty empty node g) else None


  let transpose g =
    Node_map.fold
      (fun parent -> Node_set.fold (fun child acc -> edge child parent acc))
      g
      empty


  let overlay =
    Node_map.merge (fun _parent children1_opt children2_opt ->
        Some
          (Node_set.union
             (Option.value ~default:Node_set.empty children1_opt)
             (Option.value ~default:Node_set.empty children2_opt)))


  let to_undirected g = overlay g (transpose g)

  let wcc g =
    let double_edge_g = to_undirected g in
    snd
    @@ Node_map.fold
         (fun parent _children (vis, acc) ->
           if Node_set.mem parent vis
           then vis, acc
           else
             Tuple2.map_snd (reachable_impl vis empty parent double_edge_g) ~f:(fun g' ->
                 Node_map.mapi Fn.(const <@ flip Node_map.find g) g' :: acc))
         g
         (Node_set.empty, [])


  let pp pp_node ppf =
    Node_map.iter (fun parent children ->
        Format.fprintf
          ppf
          "* %a -> %a\n"
          pp_node
          parent
          Fmt.Dump.(list pp_node)
          (set_to_list children))
end
