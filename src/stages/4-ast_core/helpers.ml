open Types

(* This function transforms an application expression `l e1 ... en` into the pair `([ e1 ; ... ; en ] , l)` *)
let destruct_applications (e : expression) =
  let rec destruct_applications acc (lamb : expression) =
    match lamb.expression_content with
    | E_application {lamb;args} ->
       destruct_applications (args :: acc) lamb
    | _ ->
       (lamb, acc) in
  destruct_applications [] e

(* This function transforms a type `∀ v1 ... vn . t` into the pair `([ v1 ; .. ; vn ] , t)` *)
let destruct_for_alls (t : type_expression) =
  let rec destruct_for_alls type_vars (t : type_expression) = match t.type_content with
    | T_for_all { ty_binder ; type_ ; _ } ->
       destruct_for_alls (ty_binder :: type_vars) type_
    | _ -> (List.rev type_vars, t)
  in destruct_for_alls [] t

module Free_type_variables = struct
  open Ligo_prim

  module VarSet = Caml.Set.Make(Type_var)

  let unions : VarSet.t list -> VarSet.t =
    fun l -> List.fold l ~init:VarSet.empty
      ~f:(fun y1 y2 -> VarSet.union y1 y2)

  let rec map_type_expression : Type_var.t list -> type_expression -> VarSet.t = fun type_env te ->
    let self = map_type_expression type_env in
    match te.type_content with
    | T_sum { fields ; _ } ->
       let fields = Record.LMap.to_list fields |> List.map ~f:(fun ({ associated_type ; _ } : _ Rows.row_element_mini_c) -> self associated_type) in
       unions fields
    | T_record { fields ; _ } ->
       let fields = Record.LMap.to_list fields |> List.map ~f:(fun ({ associated_type ; _ } : _ Rows.row_element_mini_c) -> self associated_type) in
       unions fields
    | T_arrow { type1 ; type2 } ->
       VarSet.union (self type1) (self type2)
    | T_app { arguments ; _ } ->
       let arguments = List.map ~f:self arguments in
       unions arguments
    | T_variable v when List.mem type_env v ~equal:(fun v1 v2 -> Type_var.compare v1 v2 = 0) -> VarSet.empty
    | T_variable _ -> VarSet.empty
    | T_module_accessor _ -> VarSet.empty
       (* self element *)
    | T_singleton _ -> VarSet.empty
    | T_abstraction { ty_binder ; type_ ; _ } ->
       let v = self type_ in
       VarSet.remove ty_binder v
    | T_for_all { ty_binder ; type_ ; _ } ->
       let v = self type_ in
       VarSet.remove ty_binder v

  let type_expression : Type_var.t list -> type_expression -> Type_var.t list = fun type_env t ->
    VarSet.fold (fun v r -> v :: r) (map_type_expression type_env t) []
end

module Conv = struct
   module O = Types
   module I = Ast_sugar
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
         let lps = Ligo_prim.Container.Record.map i_to_o lps in
         Location.wrap ~loc (O.Pattern.P_record lps)
 
     
 end