open Types

let range i j =
  let rec aux i j acc = if i >= j then acc else aux i (j-1) (j-1 :: acc) in
  aux i j []

let label_range i j =
  List.map ~f:(fun i -> Label (string_of_int i)) @@ range i j

let is_tuple_lmap m =
  List.for_all ~f:(fun i -> LMap.mem i m) @@ (label_range 0 (LMap.cardinal m))

let get_pair m =
  match (LMap.find_opt (Label "0") m , LMap.find_opt (Label "1") m) with
  | Some {associated_type=e1;_}, Some {associated_type=e2;_} -> Some (e1,e2)
  | _ -> None

let tuple_of_record (m: _ LMap.t) =
  let aux i =
    let label = Label (string_of_int i) in
    let opt = LMap.find_opt (label) m in
    Option.bind ~f: (fun opt -> Some ((label,opt),i+1)) opt
  in
  Base.Sequence.to_list @@ Base.Sequence.unfold ~init:0 ~f:aux

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
    | _ -> (type_vars, t)
  in destruct_for_alls [] t

module Free_type_variables = struct

  module VarSet = Caml.Set.Make(TypeVar)

  let unions : VarSet.t list -> VarSet.t =
    fun l -> List.fold l ~init:VarSet.empty
      ~f:(fun y1 y2 -> VarSet.union y1 y2)

  let rec map_type_expression : type_variable list -> type_expression -> VarSet.t = fun type_env te ->
    let self = map_type_expression type_env in
    match te.type_content with
    | T_sum { fields ; _ } ->
       let fields = LMap.to_list fields |> List.map ~f:(fun ({ associated_type ; _ } : _ row_element_mini_c) -> self associated_type) in
       unions fields
    | T_record { fields ; _ } ->
       let fields = LMap.to_list fields |> List.map ~f:(fun ({ associated_type ; _ } : _ row_element_mini_c) -> self associated_type) in
       unions fields
    | T_arrow { type1 ; type2 } ->
       VarSet.union (self type1) (self type2)
    | T_app { arguments ; _ } ->
       let arguments = List.map ~f:self arguments in
       unions arguments
    | T_variable v when List.mem type_env v ~equal:(fun v1 v2 -> TypeVar.compare v1 v2 = 0) -> VarSet.empty
    | T_variable v when TypeVar.is_generalizable v  -> VarSet.singleton v
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

  let type_expression : type_variable list -> type_expression -> type_variable list = fun type_env t ->
    VarSet.fold (fun v r -> v :: r) (map_type_expression type_env t) []
end
