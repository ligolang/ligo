module I = Ast_imperative.Types
module O = Types

module Location = Simple_utils.Location

module Container = Ligo_prim.Pattern.Container

module Conv = struct
  let rec i_to_o (pl : _ I.Pattern.t) : _ O.Pattern.t =
    let loc = Location.get_location pl in
    match (Location.unwrap pl) with
    | P_unit -> Location.wrap ~loc O.Pattern.P_unit
    | P_var b -> Location.wrap ~loc (O.Pattern.P_var b)
    | P_list Cons (h, t) ->
        let h = i_to_o h in
        let t = i_to_o t in
        Location.wrap ~loc (O.Pattern.P_list (Cons(h, t)))
    | P_list List ps ->
        let ps = List.map ~f:i_to_o ps in
        Location.wrap ~loc (O.Pattern.P_list (List ps))
    | P_variant (l, p) ->
        let p = i_to_o p in
        Location.wrap ~loc (O.Pattern.P_variant (l, p))
    | P_tuple ps -> 
        let ps = List.map ~f:i_to_o ps in
        Location.wrap ~loc (O.Pattern.P_tuple ps)
    | P_record lps ->
        let lps = Container.List.map i_to_o lps in
        let lps = Container.Record.of_list (Container.List.to_list lps) in
        Location.wrap ~loc (O.Pattern.P_record lps)

  let rec o_to_i (pr : _ O.Pattern.t) : _ I.Pattern.t =
    let loc = Location.get_location pr in
    match (Location.unwrap pr) with
    | P_unit -> Location.wrap ~loc I.Pattern.P_unit
    | P_var b -> Location.wrap ~loc (I.Pattern.P_var b)
    | P_list Cons (h, t) ->
        let h = o_to_i h in
        let t = o_to_i t in
        Location.wrap ~loc (I.Pattern.P_list (Cons(h, t)))
    | P_list List ps ->
        let ps = List.map ~f:o_to_i ps in
        Location.wrap ~loc (I.Pattern.P_list (List ps))
    | P_variant (l, p) ->
        let p = o_to_i p in
        Location.wrap ~loc (I.Pattern.P_variant (l, p))
    | P_tuple ps -> 
        let ps = List.map ~f:o_to_i ps in
        Location.wrap ~loc (I.Pattern.P_tuple ps)
    | P_record lps ->
        let lps = Container.Record.map o_to_i lps in
        let lps = Container.List.of_list (Container.Record.to_list lps) in
        Location.wrap ~loc (I.Pattern.P_record lps)
end