open Types

type state = {
  pad_path : string;
  pad_node : string;
}

let mk_state () = {
  pad_path = "";
  pad_node = "";
}

let pad arity rank {pad_path=_;pad_node} =
  { pad_path =
        pad_node ^ (if rank = arity-1 then "`-- " else "|-- ");
      pad_node =
        pad_node ^ (if rank = arity-1 then "    " else "|   ")
  }

let graph' f (dep_g,filename) =
  let exception Dependency_cycle of string in
  let len node =
    let aux _node i = i + 1 in
    G.fold_succ aux dep_g node 0
  in
  let state = mk_state () in
  let set = SSet.empty in
  let rec pp_node state set arity name rank =
    let state = pad arity rank @@ state in
    f state.pad_path name;
    if SSet.mem name set then raise (Dependency_cycle name);
    let set = SSet.add name set in
    let len = len name in
    let _ = G.fold_succ (pp_node state set len) dep_g name 0 in
    rank+1
  in
  let _ = try pp_node state set 1 filename 0
    with Dependency_cycle _ -> 0 in
  ()
  
let graph ppf (dep_g,filename) =
  let module TopSort = Graph.Topological.Make(G) in
  let order,final = TopSort.fold 
    (fun filename (m,order) -> SMap.add filename order m, order + 1)
    dep_g (SMap.empty, 1) in
  graph' (fun pad_path name ->
    match SMap.find_opt name order with
      Some n -> Format.fprintf ppf "%s%d -- %s\n%!" pad_path (final - n) name
    | None -> ()) 
    (dep_g,filename)
  