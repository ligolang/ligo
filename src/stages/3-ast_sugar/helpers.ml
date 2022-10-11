module I = Ast_imperative.Types
module O = Types

module Location = Simple_utils.Location

module Container = Ligo_prim.Pattern.Container

module Conv = struct
  let rec list_to_record (pl : _ I.Pattern.t) : _ O.Pattern.t =
    let loc = Location.get_location pl in
    match (Location.unwrap pl) with
    | P_unit -> Location.wrap ~loc O.Pattern.P_unit
    | P_var b -> Location.wrap ~loc (O.Pattern.P_var b)
    | P_list Cons (h, t) ->
        let h = list_to_record h in
        let t = list_to_record t in
        Location.wrap ~loc (O.Pattern.P_list (Cons(h, t)))
    | P_list List ps ->
        let ps = List.map ~f:list_to_record ps in
        Location.wrap ~loc (O.Pattern.P_list (List ps))
    | P_variant (l, p) ->
        let p = list_to_record p in
        Location.wrap ~loc (O.Pattern.P_variant (l, p))
    | P_tuple ps -> 
        let ps = List.map ~f:list_to_record ps in
        Location.wrap ~loc (O.Pattern.P_tuple ps)
    | P_record lps ->
        let lps = Container.List.map list_to_record lps in
        let lps = Container.Record.of_list (Container.List.to_list lps) in
        Location.wrap ~loc (O.Pattern.P_record lps)

  let rec record_to_list (pr : _ O.Pattern.t) : _ I.Pattern.t =
    let loc = Location.get_location pr in
    match (Location.unwrap pr) with
    | P_unit -> Location.wrap ~loc I.Pattern.P_unit
    | P_var b -> Location.wrap ~loc (I.Pattern.P_var b)
    | P_list Cons (h, t) ->
        let h = record_to_list h in
        let t = record_to_list t in
        Location.wrap ~loc (I.Pattern.P_list (Cons(h, t)))
    | P_list List ps ->
        let ps = List.map ~f:record_to_list ps in
        Location.wrap ~loc (I.Pattern.P_list (List ps))
    | P_variant (l, p) ->
        let p = record_to_list p in
        Location.wrap ~loc (I.Pattern.P_variant (l, p))
    | P_tuple ps -> 
        let ps = List.map ~f:record_to_list ps in
        Location.wrap ~loc (I.Pattern.P_tuple ps)
    | P_record lps ->
        let lps = Container.Record.map record_to_list lps in
        let lps = Container.List.of_list (Container.Record.to_list lps) in
        Location.wrap ~loc (I.Pattern.P_record lps)
end