open Types

let scopes : Format.formatter -> scopes -> unit = fun f s ->
  let pp_scope f (s:scope) =
    let pp_list f = List.iter (fun (k,_) -> Format.fprintf f "%s " k ) in
    let a = Def_map.to_kv_list s.env in
    Format.fprintf f "[ %a] %a" pp_list a Location.pp s.range
  in
  let pp_scopes f = List.iter (Format.fprintf f "@[<v>%a@ @]" pp_scope) in
  Format.fprintf f "@[<v>Scopes:@ %a@]" pp_scopes s

let definitions : Format.formatter -> def_map -> unit = fun f dm ->
  let kvl = Def_map.to_kv_list dm in
  let (variables,types) = List.partition (fun (_,def) -> match def with Type _ -> false | Variable _ -> true) kvl in
  let pp_def f = List.iter (fun (k,v) -> Format.fprintf f "(%s -> %s) %a@ " k (get_def_name v) Location.pp (get_range v)) in
  Format.fprintf f "@[<v>Variable definitions:@ %aType definitions:@ %a@]" pp_def variables pp_def types

let def_to_yojson : def -> Yojson.Safe.t = function
  | Variable { name ; range ; body_range ; t ; references=_ } ->
    `Assoc [
      ("name", `String name);
      ("range", Location.to_yojson range);
      ("body_range", Location.to_yojson body_range);
      ("t", match t with None -> `Null | Some t -> Ast_typed.Yojson.type_expression t );
      ("references", `Null);
    ]
  | Type { name ; range ; body_range ; content=_ } ->
    `Assoc [
      ("name", `String name);
      ("range", Location.to_yojson range);
      ("body_range", Location.to_yojson body_range);
      ("content", `String "TODO" );
    ]

let defs_json d : Yojson.Safe.t =
  let get_defs d = 
    let (v,tv) = List.partition (fun (_,def) -> match def with Variable _ -> true | Type _ -> false) (Def_map.to_kv_list d) in
    [ 
      ("variables", `Assoc (List.map (fun (def_id,def) -> (def_id,def_to_yojson def)) v));
      ("types", `Assoc (List.map (fun (def_id,def) -> (def_id,def_to_yojson def)) tv))
    ]
  in
  `Assoc (get_defs d)

let scopes_json s : Yojson.Safe.t = `List (
  List.map
    (fun scope ->
      let sd = Def_map.to_kv_list scope.env in
      let (variables,types) = List.partition (fun (_,def) -> match def with Type _ -> false | Variable _ -> true) sd in
      let v = List.map (fun (k,_) -> `String k) variables in
      let t = List.map (fun (k,_) -> `String k) types in
      (`Assoc [("range", Location.to_yojson scope.range) ; ("expression_environment", `List v) ; ("type_environment", `List t)])
    )
    s
  )

let to_json (d,s) = `Assoc [("definitions", (defs_json d)) ; ("scopes", (scopes_json s))]
