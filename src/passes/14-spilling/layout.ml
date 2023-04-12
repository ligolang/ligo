open Ligo_prim
open Errors
open Simple_utils.Trace
module Append_tree = Tree.Append
module I = Ast_expanded
module O = Mini_c
include Layout

(* [comb_or] creates an `or` type tree from the list of annotated types [lst] 
   `[ a ; b ; c ] => T_or (a, T_or (b,c))`
*)
let comb_or ?source_type lst =
  let ne_lst = Simple_utils.List.Ne.of_list lst in
  let rec aux lst =
    match lst with
    | last, [] -> last
    | hd, tl ->
      let rest = aux (Simple_utils.List.Ne.of_list tl) in
      None, O.Expression.make_t ?source_type (T_or (hd, rest))
  in
  snd (aux ne_lst)

(* Find actual annot to use on Michelson type *)
let actual_annot (annot : string option) (label : Label.t) =
  match annot with
  (* if no annot specified, default to label *)
  | None ->
    let label = Label.to_string label in
    (* unless label is an integer for a tuple index *)
    (match int_of_string_opt label with
     | None ->
      (* lowercase for convenience and backwards compat *)
      let l = match String.to_list label with
      | [] -> ""
      | hd::tl -> String.of_char_list (Char.lowercase hd::tl)
      in
      Some l
     | Some _ -> None)
  | Some annot ->
    (* no annot if user specified empty string *)
    if String.is_empty annot
    then None
    else Some annot

(* [from_layout] [f] [field] [layout] creates a type from [fields] which
   structure follows the one given by [layout].
  
   [f] defines the structure of the Inner layout case, it could define
   any type structure but will typically be used to create a pair/or comb
*)
let from_layout
    : type a.
      raise:_ -> ((string option * a) list -> a) -> a Record.t -> Ligo_prim.Layout.t -> a
  =
 fun ~raise f fields layout ->
  let annot_layout =
    Layout.map_f (fun Layout.{ name; annot } -> actual_annot annot name, Record.find fields name) layout
  in
  let rec aux = function
    | Layout.Inner [] -> raise.error (corner_case ~loc:__LOC__ "t_record empty")
    | Inner lst -> None, f (List.map ~f:aux lst)
    | Field t -> t
  in
  snd @@ aux annot_layout


(* [t_sum] and [t_record] are the specialized version of [t_layouted_type]
   for combs *)
let t_sum ?source_type = from_layout (comb_or ?source_type)
let t_record ?source_type = from_layout (fun lst -> O.Expression.make_t ?source_type (T_tuple lst))

let record_to_pairs ~raise ~source_type return =
  from_layout ~raise
    (fun lst ->
      let tv =
        let tys = List.map
          ~f:(fun (ann,(e:O.expression)) -> ann, e.type_expression) lst 
        in
        O.t_tuple ~loc:Location.generated ~source_type tys
      in
      return ~tv @@ O.E_tuple (List.map ~f:snd lst))


(* [explode_row] [row] [binders] [matchee] returns all the let bindings necessary
  to destruct [matchee] of a given layout (define in [row])
  e.g.
  ```
  match x with
  | (a, b, c, d) -> a + b + c + d
  ```
  with x layout being :
    Inner [ Inner [ Field (a,type_a) ; Field (b,type_b) ]
          ; Inner [ Field (c,type_c) ; Field (d,type_d) ]]
  =>
  ```
  let (l, r) = x in
  let (a, b) = l in
  let (c, d) = r in
  a + b + c + d
  ```
*)
let explode_row ~loc (row : _ I.Row.t) ty' binders matchee
    : ([ `Let of Value_var.t * O.type_expression
       | `Let_tuple of (Value_var.t * O.type_expression) list
       ]
      * O.expression)
    list
  =
  let rec loop layout ~matchee ty' =
    match layout with
    | Field { name; annot = _ } ->
      let type_ = Map.find_exn row.fields name in
      let binder = Map.find_exn binders name in
      [ `Let (Binder.get_var binder, type_), matchee ]
    | Inner layouts ->
      let subtree_types =
        match Mini_c.get_t_tuple ty' with
        | None -> failwith (Format.asprintf "internal error: tuple not a tuple @ %s" __LOC__)
        | Some subtree_types -> subtree_types in
      let layouts_with_types =
        List.zip_exn layouts subtree_types in
      let let_tuple_bindings, bindings =
        List.fold_map layouts_with_types
          ~init:[]
          ~f:(fun let_tuple_bindings (layout, ty') ->
              let var = Value_var.fresh ~loc () in
              let var_exp = O.Expression.make ~loc (E_variable var) ty' in
              let bindings = loop layout ~matchee:var_exp ty' in
              (var, ty') :: let_tuple_bindings, bindings)
      in
      (`Let_tuple (List.rev let_tuple_bindings), matchee) :: List.concat bindings
  in
  loop row.layout ~matchee ty'

let path_to_field layout field (ty' : Mini_c.type_expression) =
  let rec aux layout field ty' =
    match layout with
    | Field { name; _ } ->
      (* Empty path *)
      Option.some_if Label.(field = name) []
    | Inner layouts ->
      let subtree_types =
        match Mini_c.get_t_tuple ty' with
        | None -> failwith (Format.asprintf "internal error: tuple not a tuple @ %s" __LOC__)
        | Some subtree_types -> subtree_types in
      let layouts_with_types =
        List.zip_exn layouts subtree_types in
      List.find_mapi
        layouts_with_types
        ~f:(fun idx (layout, field_ty') ->
            match aux layout field field_ty' with
            | None -> None
            | Some path ->
              Some ((idx, List.length layouts, field_ty', ty') :: path))
  in
  match aux layout field ty' with
  | None -> failwith (Format.asprintf "internal error: field not found @ %s" __LOC__)
  | Some ret -> ret

let constructor_to_lr
      ~(layout : Ligo_prim.Layout.t)
      (ty : Mini_c.type_expression)
      (constructor : Label.t)
    : (Mini_c.type_expression * [`Left | `Right]) list =
  let layout = Layout.to_binary layout in
  let open Option.Let_syntax in
  let rec find layout path ty =
    match layout with
    | Empty -> None
    | Node (l, r) ->
       (let%bind (lt, rt) = Mini_c.get_t_or ty in
        match find l ((ty, `Left) :: path) lt with
        | None -> find r ((ty, `Right) :: path) rt
        | Some path -> Some path)
    | Leaf f ->
       if Label.equal constructor f.name
       then Some path
       else None in
  match find layout [] ty with
  | None -> failwith (Format.asprintf "internal error: constructor not found @ %s" __LOC__)
  | Some path -> path

type variant_tree = [
  | `Leaf of Label.t
  | `Node of variant_pair * variant_pair
  ]

and variant_pair = variant_tree * Mini_c.type_expression

let match_variant_to_tree ~raise ~layout ty : variant_pair =
  let layout = Layout.to_binary layout in
  let rec aux t ty =
    match t with
    | Leaf field ->
       (`Leaf field.name, ty)
    | Node (l, r) ->
       (match Mini_c.get_t_or ty with
        | None -> raise.error (corner_case ~loc:__LOC__ "variant not translated to or")
        | Some (lty, rty) ->
           let lt = aux l lty in
           let rt = aux r rty in
           (`Node (lt, rt), ty))
    | Empty -> raise.error (corner_case ~loc:__LOC__ "empty variant") in
  aux layout ty