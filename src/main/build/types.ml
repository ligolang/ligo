module Node = struct
  type t = String.t
  let compare = String.compare
  let hash    = Hashtbl.hash
  let equal   = String.equal
end

module G = Graph.Persistent.Digraph.Concrete(Node)
module Dfs = Graph.Traverse.Dfs(G)

module SMap = Map.String
module SSet = Set.Make(String)
