module Definitions = struct
  module Def_map = Map.Make( struct type t = string let compare = String.compare end)

  type vdef = {
    name : string ;
    range : Location.t ;
    body_range : Location.t ;
    t : Ast_typed.type_expression option ;
    references : (Location.t list) option
  }
  type tdef = {
    name : string ;
    range : Location.t ;
    body_range : Location.t ;
    content : Ast_core.type_expression
  }
  type def = Variable of vdef | Type of tdef
  type def_map = def Def_map.t

  let merge_defs a b = Def_map.union (fun _ a _  -> Some a) a b

  let get_def_name = function
    | Variable d -> d.name
    | Type d -> d.name
  
  let get_range = function
    | Type t -> t.range
    | Variable v -> v.range

  let make_v_def : with_types:bool -> string -> Ast_typed.type_expression option -> Location.t -> Location.t -> def =
    fun ~with_types name t range body_range ->
      let t = if with_types then t else None in
      Variable { name ; range ; body_range ; t ; references = None }

  let make_t_def : string -> Ast_core.declaration Location.wrap -> Ast_core.type_expression -> def =
    fun name decl te ->
      Type { name ; range = decl.location ; body_range = te.location ; content = te }

end

include Definitions

type scope = { range : Location.t ; env : def_map }
type scopes = scope list

let add_scope (range,env) scopes = { range ; env } :: scopes