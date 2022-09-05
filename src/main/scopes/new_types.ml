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

  type mod_case = Def of def list | Alias of string list

  and mdef = {
    name : string ;
    range : Location.t ;
    body_range : Location.t ;
    references : Location.t list ; (* TODO: make this Location set *)
    mod_case : mod_case ;
  }

  and def = Variable of vdef | Type of tdef | Module of mdef

  let def_equal a b =
    match a , b with
    | Variable x , Variable y -> String.equal x.name y.name
    | Type x , Type y -> String.equal x.name y.name
    | Module x , Module y -> String.equal x.name y.name
    | (Variable _ | Type _ | Module _) , (Variable _ | Type _ | Module _) -> false

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

  let make_m_def : range:Location.t -> body_range:Location.t -> string -> def list -> def =
    fun ~range ~body_range name members ->
      let mod_case = Def members in
      Module { name ; range ; body_range ; mod_case ; references = [] }

  let make_m_alias_def : range:Location.t -> body_range:Location.t -> string -> string list -> def =
    fun ~range ~body_range name alias ->
      let mod_case = Alias alias in
      Module { name ; range ; body_range ; mod_case ; references = [] }
end

include Definitions

type scope = { range : Location.t ; env : def list }
type scopes = scope list

module Bindings_map = Simple_utils.Map.Make ( struct type t = Ast_typed.expression_variable let compare = Ast_typed.Compare.expression_variable end )
type bindings_map = Ast_typed.type_expression Bindings_map.t
