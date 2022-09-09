open Ligo_prim
let generated_flag = "#?generated"
let get_binder_name : Value_var.t -> string = fun v ->
  if Value_var.is_generated v
  then generated_flag
  else Value_var.to_name_exn v

let get_type_binder_name : Type_var.t -> string = fun v ->
  if Type_var.is_generated v
  then generated_flag
  else Type_var.to_name_exn v
let get_mod_binder_name : Module_var.t -> string = fun v ->
  if Module_var.is_generated v
  then generated_flag
  else Module_var.to_name_exn v


module Definitions = struct
  open Ligo_prim
  module Location = Simple_utils.Location
  module List     = Simple_utils.List
  module Def_map  = Simple_utils.Map.Make(String)

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

  let get_body_range = function
    | Type        t -> t.body_range
    | Variable    v -> v.body_range
    | Module      m -> m.body_range

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

  let add_reference : Value_var.t -> def_map -> def_map = fun x env ->
    let aux : string * def -> bool = fun (_,d) ->
      match d with
      | Variable v -> Value_var.is_name x v.name
      | (Type _ | Module _ ) -> false
    in
    match List.find ~f:aux (Def_map.bindings env) with
    | Some (k,_) ->
      let aux : def option -> def option = fun d_opt ->
        match d_opt with
        | Some (Variable v) -> Some (Variable { v with references = (Value_var.get_location x :: v.references) })
        | Some x -> Some x
        | None -> None
      in
      Def_map.update k aux env
    | None -> env

  let update_module_reference : Module_var.t list -> def_map -> def_map = fun mvs env ->
    let rec aux env mv =
      let aux d =
        match d with
        | Module ({ name ; mod_case ; references ; _ } as m)  ->
            (match Module_var.is_name mv name with
            | true ->
              let references = (Module_var.get_location mv) :: references in
              Module { m with references ; mod_case } 
            | false ->
              let mod_case = match mod_case with
              | Alias _ -> mod_case
              | Def env -> Def (aux env mv)
              in
              Module { m with mod_case }
            )
        | Variable _ | Type _ -> d
      in
      Def_map.map aux env
    in
    List.fold_left mvs ~init:env ~f:aux

  let rec find_modules_to_update : string list -> def_map -> string list =
    fun mvs env ->
      match mvs with
      | [] -> []
      | mv::mvs ->
        let aux (_,d) = 
          match d with
          | Module { name ; _ } -> String.(name = mv)
          | Variable _ | Type _ -> false
        in
        (match List.find (Def_map.bindings env) ~f:aux with
        | Some (k,Module { mod_case=Def d ; _ }) -> k :: find_modules_to_update mvs d
        | Some (_,Module { mod_case=Alias a ; _ }) -> find_modules_to_update (a@mvs) env
        | Some _ -> []
        | None -> [] 
        )

  let add_module_element_reference : Module_var.t list -> Value_var.t -> def_map -> def_map =
    fun mvs element env ->
      let e = get_binder_name element in
      let e_loc = Value_var.get_location element in
      let mvs = List.map mvs ~f:get_mod_binder_name in
      let ms = find_modules_to_update mvs env in
      let rec aux ms env =
        match ms with
        | [] ->
          Def_map.map (function 
          | Variable v when String.(v.name = e)->
            let references = e_loc :: v.references in
            Variable { v with references }
          | Variable v -> Variable v
          | Type t -> Type t | Module m -> Module m) env
        | m::ms -> Def_map.update m (function 
          | None -> None
          | Some (Module ({ mod_case=Def d ; _ } as modul)) ->
            let mod_case = Def (aux ms d) in
            Some (Module { modul with mod_case })
          | Some x -> Some x) env
      in
      aux ms env
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

module Bindings_map = Simple_utils.Map.Make (Ligo_prim.Value_var)
type bindings_map = Ast_typed.type_expression Bindings_map.t
