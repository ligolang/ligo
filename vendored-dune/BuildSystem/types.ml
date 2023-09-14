module Node = struct
  type t = String.t [@@deriving eq, compare]

  let hash = Hashtbl.hash
end

module G = Graph.Persistent.Digraph.Concrete (Node)
module Dfs = Graph.Traverse.Dfs (G)
module TopSort = Graph.Topological.Make (G)
module SMap = Map.Make (String)
module SSet = Set.Make (String)
