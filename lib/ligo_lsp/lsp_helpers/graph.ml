open Core
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

module Make (Ord : Map.Key) : S with type vertex = Ord.t = struct
  module Node_map = Map.Make (Ord)
  module Node_set = Set.Make (Ord)

  type vertex = Ord.t
  type t = Node_set.t Node_map.t

  let empty = Node_map.empty
  let vertex node g = Map.update g node ~f:Option.(value ~default:Node_set.empty)
  let vertices vs g = List.fold vs ~init:g ~f:(Fn.flip vertex)
  let mem = Fn.flip Map.mem

  let edge from to' g =
    Map.update
      (vertex to' g)
      from
      ~f:Option.(value_map ~default:(Node_set.singleton to') ~f:(Fn.flip Set.add to'))


  let from_assoc vs g = List.fold vs ~init:g ~f:(fun g (v1, v2) -> edge v1 v2 g)

  let from_assocs edges g =
    List.fold edges ~init:g ~f:(fun g (node, nodes) ->
        List.fold nodes ~init:(vertex node g) ~f:(fun g v -> edge node v g))


  let to_assocs =
    List.map ~f:(Tuple2.map_snd ~f:Set.to_list) <@ Map.to_alist ~key_order:`Increasing


  let to_vertices = List.map ~f:fst <@ to_assocs
  let post node g = Option.map (Map.find g node) ~f:Set.to_list

  (** Internal: exposed implementation of [reachable] that performs no check that [node]
      is in [g], and returns the set of visited vertices, taking the initial set of
      [vis]ited nodes and [acc]umulated reachability graph. *)
  let rec reachable_impl (vis : Node_set.t) (acc : t) (node : vertex) (g : t)
      : Node_set.t * t
    =
    if Set.mem vis node
    then vis, acc
    else
      Set.fold
        (Map.find_exn g node)
        ~init:(Set.add vis node, acc)
        ~f:(fun (vis, acc) child -> reachable_impl vis (edge node child acc) child g)


  let reachable node g =
    if mem node g then Some (snd @@ reachable_impl Node_set.empty empty node g) else None


  let transpose =
    Map.fold ~init:empty ~f:(fun ~key:parent ~data:children acc ->
        Set.fold children ~init:acc ~f:(fun acc child -> edge child parent acc))


  let overlay = Map.merge_skewed ~combine:(fun ~key:_parent -> Set.union)
  let to_undirected g = overlay g (transpose g)

  let wcc g =
    let double_edge_g = to_undirected g in
    snd
    @@ Map.fold
         g
         ~init:(Node_set.empty, [])
         ~f:(fun ~key:parent ~data:_children (vis, acc) ->
           if Set.mem vis parent
           then vis, acc
           else
             Tuple2.map_snd (reachable_impl vis empty parent double_edge_g) ~f:(fun g' ->
                 Map.mapi g' ~f:(fun ~key:parent ~data:_children -> Map.find_exn g parent)
                 :: acc))


  let pp pp_node ppf =
    Map.iteri ~f:(fun ~key:parent ~data:children ->
        Format.fprintf
          ppf
          "* %a -> %a\n"
          pp_node
          parent
          Fmt.Dump.(list pp_node)
          (Set.to_list children))
end
