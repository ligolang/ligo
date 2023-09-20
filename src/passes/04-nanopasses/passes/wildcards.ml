open Ast_unified
open Pass_type
module Location = Simple_utils.Location

(* morph wildcards (variable with name "_") into unreachable variable *)
include Flag.No_arg ()

let name = __MODULE__

let compile ~raise:_ =
  let pattern : _ pattern_ -> pattern =
   fun p ->
    let loc = Location.get_location p in
    match Location.unwrap p with
    | P_var v when Variable.is_name v "_" -> p_var ~loc (Variable.fresh_like ~loc v)
    | p -> make_p ~loc p
  in
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_variable v when Variable.is_name v "_" ->
      e_variable ~loc (Variable.fresh_like ~loc v)
    | e -> make_e ~loc e
  in
  let ty_expr : _ ty_expr_ -> ty_expr =
   fun t ->
    let loc = Location.get_location t in
    match Location.unwrap t with
    | T_var v when Ty_variable.is_name v "_" -> t_var ~loc (Ty_variable.fresh_like ~loc v)
    | t -> make_t ~loc t
  in
  let mod_expr : _ mod_expr_ -> mod_expr =
   fun me ->
    let loc = Location.get_location me in
    match Location.unwrap me with
    | M_var v when Mod_variable.is_name v "_" ->
      m_var ~loc (Mod_variable.fresh_like ~loc v)
    | M_path path ->
      m_path
        ~loc
        (List.Ne.map
           (fun v ->
             if Mod_variable.is_name v "_" then Mod_variable.fresh_like ~loc v else v)
           path)
    | t -> make_m ~loc t
  in
  Fold { idle_fold with pattern; expr; ty_expr; mod_expr }


let reduction ~raise:_ = Iter.defaults
let decompile ~raise:_ = Nothing
