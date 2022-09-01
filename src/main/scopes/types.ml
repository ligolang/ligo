let generated_flag = "#?generated"
let get_binder_name : Ast_typed.ValueVar.t -> string = fun v ->
  if Ast_typed.ValueVar.is_generated v
  then generated_flag
  else Ast_typed.ValueVar.to_name_exn v

let get_type_binder_name : Ast_typed.TypeVar.t -> string = fun v ->
  if Ast_typed.TypeVar.is_generated v
  then generated_flag
  else Ast_typed.TypeVar.to_name_exn v
let get_mod_binder_name : Ast_typed.ModuleVar.t -> string = fun v ->
  if Ast_typed.ModuleVar.is_generated v
  then generated_flag
  else Ast_typed.ModuleVar.to_name_exn v


module Definitions = struct
  module Location = Simple_utils.Location
  module List     = Simple_utils.List
  module Def_map = Simple_utils.Map.Make(String)

  type type_case =
    | Core of Ast_core.type_expression
    | Resolved of Ast_typed.type_expression
    | Unresolved

  type vdef = {
    name  : string ;
    range : Location.t ;
    body_range : Location.t ;
    t : type_case ;
    references : Location.t list (* TODO: make this Location set *)
  }

  type tdef = {
    name  : string ;
    range : Location.t ;
    body_range : Location.t ;
    content : Ast_core.type_expression ;
  }

  type mod_case = Def of def_map | Alias of string list

  and mdef = {
    name : string ;
    range : Location.t ;
    body_range : Location.t ;
    references : Location.t list ; (* TODO: make this Location set *)
    mod_case : mod_case ;
  }

  and def = Variable of vdef | Type of tdef | Module of mdef
  and def_map = def Def_map.t

  let def_equal a b =
    match a , b with
    | Variable x , Variable y -> String.equal x.name y.name
    | Type x , Type y -> String.equal x.name y.name
    | Module x , Module y -> String.equal x.name y.name
    | (Variable _ | Type _ | Module _) , (Variable _ | Type _ | Module _) -> false

  let rec merge_refs : string -> def -> def -> def option = fun _ a b ->
    match a,b with
    | Variable a , Variable b ->
      let references = List.dedup_and_sort ~compare:Location.compare (a.references @ b.references) in
      Some (Variable { a with references })
    | Module ({ mod_case=Alias _ ; _ } as a), Module ({ mod_case=Alias _ ; _ } as b) ->
      let references = List.dedup_and_sort ~compare:Location.compare (a.references @ b.references) in
      Some (Module { a with references })
    | Module ({ mod_case=Def d1 ; _ } as a), Module ({ mod_case=Def d2 ; _ } as b) ->
      let references = List.dedup_and_sort ~compare:Location.compare (a.references @ b.references) in
      let mod_case = Def (merge_defs d1 d2) in
      Some (Module { a with references ; mod_case })
    | (Variable _ |Type _ | Module _) , (Variable _ |Type _ | Module _) -> Some a

  and merge_defs a b =
    Def_map.union merge_refs a b

  let get_def_name = function
    | Variable    d -> d.name
    | Type        d -> d.name
    | Module      d -> d.name

  let get_range = function
    | Type        t -> t.range
    | Variable    v -> v.range
    | Module      m -> m.range

  let make_v_def : string -> type_case -> Location.t -> Location.t -> def =
    fun name t range body_range ->
      Variable { name ; range ; body_range ; t ; references = [] }

  let make_t_def : string -> Location.t -> Ast_core.type_expression -> def =
    fun name loc te ->
      Type { name ; range = loc ; body_range = te.location ; content = te }

  let make_m_def : range:Location.t -> body_range:Location.t -> string -> def_map -> def =
    fun ~range ~body_range name members ->
      let mod_case = Def members in      
      Module { name ; range ; body_range ; mod_case ; references = [] }

  let make_m_alias_def : range:Location.t -> body_range:Location.t -> string -> string list -> def =
    fun ~range ~body_range name alias ->
      let mod_case = Alias alias in
      Module { name ; range ; body_range ; mod_case ; references = [] }

  let add_reference : Ast_core.expression_variable -> def_map -> def_map = fun x env ->
    let aux : string * def -> bool = fun (_,d) ->
      match d with
      | Variable v -> Ast_core.ValueVar.is_name x v.name
      | (Type _ | Module _) -> false
    in
    match List.find ~f:aux (Def_map.bindings env) with
    | Some (k,_) ->
      let aux : def option -> def option = fun d_opt ->
        match d_opt with
        | Some (Variable v) -> Some (Variable { v with references = (Ast_core.ValueVar.get_location x :: v.references) })
        | Some x -> Some x
        | None -> None
      in
      Def_map.update k aux env
    | None -> env

  let add_module_reference : Ast_core.module_variable list -> def_map -> def_map = fun mvs env ->
    let rec update_module_reference env mv =
      let aux d =
        match d with
        | Module ({ name ; mod_case ; references ; _ } as m)  ->
            (match Ast_core.ModuleVar.is_name mv name with
            | true ->
              let references = (Ast_core.ModuleVar.get_location mv) :: references in
              Module { m with references ; mod_case } 
            | false ->
              let mod_case = match mod_case with
              | Alias _ -> mod_case
              | Def env -> Def (update_module_reference env mv)
              in
              Module { m with mod_case }
            )
        | Variable _ | Type _ -> d
      in
      Def_map.map aux env
    in
    List.fold_left mvs ~init:env ~f:update_module_reference

  (* TODO: simplify this *)
  let add_module_element_reference : Ast_core.module_variable list -> Ast_core.expression_variable -> def_map -> def_map =
    fun mvs element env ->
      let (let*) x f = Option.bind x ~f in
      (* let open Core.Option in *)
      let find_module acc mv =
        let* (xs,env) = acc in
        match Def_map.find_opt (get_mod_binder_name mv) env with
        | Some (Module { mod_case = Alias alias ; _ }) ->
          let aux env n =
            let* env = env in
            match Def_map.find_opt n env with
            | Some (Module { mod_case = Def env ; _ } ) -> Some env
            | Some _ -> None
            | None -> None
          in
          let* def = List.fold_left alias ~init:(Some env) ~f:aux in
          Some (alias@xs,def)
        | Some (Module { mod_case = Def def ; _ }) ->
          Some ((get_mod_binder_name mv)::xs,def)
        | Some _ -> None
        | None -> None
      in
      match List.fold_left mvs ~init:(Some ([],env)) ~f:find_module with
      | Some (names,inner_env) ->
        let loc = Ast_core.ValueVar.get_location element in
        let inner_env = Def_map.update (get_binder_name element) 
          (function 
          | Some (Variable v) -> Some (Variable { v with references = loc :: v.references })
          | Some x -> Some x
          | None -> None) inner_env in
        let names = List.rev names in
        let rec update_module names env =
          match names with
          | [] -> inner_env
          | name::names ->
            Def_map.update name 
            (function 
            | Some (Module ({ mod_case = Def def ; _ } as m)) -> 
              let def = update_module names def in
              Some (Module { m with mod_case = Def def })
            | None -> None 
            | Some x -> Some x) env
        in
        update_module names inner_env
      | None -> env
end

include Definitions

type scope = { range : Location.t ; env : def_map }
type scopes = scope list

let add_scope (range,env) (scopes:scopes) =
  let def_map_equal = Def_map.equal def_equal in
  let replaced,scopes = List.fold_map scopes ~init:false
    ~f:(fun replaced scope ->
          if replaced then replaced,scope else (
            if def_map_equal env scope.env then
              true , {scope with range = Location.cover scope.range range}
            else
              replaced,scope
          )
    )
  in
  if replaced then scopes
  else { range ; env } :: scopes

module Bindings_map = Simple_utils.Map.Make ( struct type t = Ast_typed.expression_variable let compare = Ast_typed.Compare.expression_variable end )
type bindings_map = Ast_typed.type_expression Bindings_map.t
