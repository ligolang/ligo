open Ast_typed
open Errors
open Simple_utils.Trace

let assert_type_expression_eq ~raise (loc:Location.t) ((tv',tv):type_expression * type_expression) : unit = 
  trace_option ~raise (assert_equal loc tv' tv) @@
    assert_type_expression_eq (tv' , tv)

type typer = type_expression list -> type_expression option -> type_expression

let typer_0 ~raise : Location.t -> string -> (type_expression option -> type_expression) -> typer = fun l s f lst tv_opt ->
  match lst with
  | [] -> f tv_opt
  | _ -> raise.raise @@ wrong_param_number l s 0 lst

let typer_1 ~raise : Location.t -> string -> (type_expression -> type_expression) -> typer = fun l s f lst _ ->
  match lst with
  | [ a ] -> f a
  | _ -> raise.raise @@ wrong_param_number l s 1 lst

let typer_1_opt ~raise : Location.t -> string -> (type_expression -> type_expression option -> type_expression) -> typer = fun l s f lst tv_opt ->
  match lst with
  | [ a ] -> f a tv_opt
  | _ -> raise.raise @@ wrong_param_number l s 1 lst

let typer_2 ~raise : Location.t -> string -> (type_expression -> type_expression -> type_expression) -> typer = fun l s f lst _ ->
  match lst with
  | [ a ; b ] -> f a b
  | _ -> raise.raise @@ wrong_param_number l s 2 lst

let typer_2_opt ~raise : Location.t -> string -> (type_expression -> type_expression -> type_expression option -> type_expression) -> typer = fun l s f lst tv_opt ->
  match lst with
  | [ a ; b ] -> f a b tv_opt
  | _ -> raise.raise @@ wrong_param_number l s 2 lst

let typer_3 ~raise : Location.t -> string -> (type_expression -> type_expression -> type_expression -> type_expression) -> typer = fun l s f lst _ ->
  match lst with
  | [ a ; b ; c ] -> f a b c
  | _ -> raise.raise @@ wrong_param_number l s 3 lst

let typer_3_opt ~raise : Location.t -> string -> (type_expression -> type_expression -> type_expression -> type_expression option -> type_expression) -> typer = fun l s f lst tv_opt ->
  match lst with
  | [ a ; b ; c ] -> f a b c tv_opt
  | _ -> raise.raise @@ wrong_param_number l s 3 lst

let typer_4 ~raise : Location.t -> string -> (type_expression -> type_expression -> type_expression -> type_expression -> type_expression) -> typer = fun l s f lst _ ->
  match lst with
  | [ a ; b ; c ; d ] -> f a b c d
  | _ -> raise.raise @@ wrong_param_number l s 4 lst

let typer_5 ~raise : Location.t -> string -> (type_expression -> type_expression -> type_expression -> type_expression -> type_expression -> type_expression) -> typer = fun l s f lst _ ->
  match lst with
  | [ a ; b ; c ; d ; e ] -> f a b c d e 
  | _ -> raise.raise @@ wrong_param_number l s 5 lst

let typer_6 ~raise : Location.t -> string
  -> (type_expression -> type_expression -> type_expression -> type_expression -> type_expression -> type_expression -> type_expression) -> typer = fun l s f lst _ ->
  match lst with
  | [ a ; b ; c ; d ; e ; f_ ] -> f a b c d e f_
  | _ -> raise.raise @@ wrong_param_number l s 6 lst

let constant' ~raise loc name cst = typer_0 ~raise loc name (fun _ -> cst)
let eq_1 a cst = type_expression_eq (a , cst)
let eq_2 (a , b) cst = type_expression_eq (a , cst) && type_expression_eq (b , cst)

let assert_eq_1 ~raise ?(loc=(Location.Virtual "assert_eq_1")) a b = if eq_1 a b then () else raise.raise @@ not_matching loc a b

module I = Ast_core
module O = Ast_typed

(* The `table` represents the substitutions that have been inferred.
   For example, if matching `a -> b -> a` with `int -> bool -> int`,
   it should have information such as `[a ↦ int; b ↦ bool]`. *)
module TMap = Simple_utils.Map.Make(struct type t = O.type_variable let compare x y = I.Var.compare x y end)

let rec infer_type_application ~raise ~loc ?(default_error = fun loc t t' -> assert_equal loc t t') table (type_matched : O.type_expression) (type_ : O.type_expression) =
  let open O in
  let self = infer_type_application ~raise ~loc ~default_error in
  let default_error = default_error loc type_matched type_ in
  let inj_mod_equal a b = (* TODO: cleanup with polymorphic functions in value env *)
    let a = Ligo_string.extract a in
    let b = Ligo_string.extract b in
    let ad_hoc_maps_unification a b = match a,b with
      | "map_or_big_map", x -> (x,x)
      | x, "map_or_big_map" -> (x,x)
      | _ -> a,b
    in
    let (a,b) = ad_hoc_maps_unification a b in
    String.equal a b
  in
  match type_matched.type_content, type_.type_content with
  | T_variable v, _ -> (
     match TMap.find_opt v table with
     | Some t -> trace_option ~raise (not_matching loc t type_) (assert_type_expression_eq (type_, t));
                 table
     | None -> TMap.add v type_ table)
  | T_arrow {type1;type2}, T_arrow {type1=type1_;type2=type2_} ->
     let table = self table type1 type1_ in
     let table = self table type2 type2_ in
     table
  | T_constant {language;injection;parameters}, T_constant {language=language';injection=injection';parameters=parameters'} ->
     if String.equal language language' && inj_mod_equal injection injection' && Int.equal (List.length parameters) (List.length parameters') then
       let table = List.fold_right (List.zip_exn parameters parameters') ~f:(fun (t, t') table ->
                       self table t t') ~init:table in
       table
     else
       raise.raise default_error
  | T_record {content; layout}, T_record {content=content'; layout=layout'} ->
     let content_kv = O.LMap.to_kv_list content in
     let content'_kv = O.LMap.to_kv_list content' in
     if layout_eq layout layout' &&
          List.equal equal_label (List.map content_kv ~f:fst) (List.map content'_kv ~f:fst) then
       let elements = List.zip_exn content_kv content'_kv in
       let aux ((_, {associated_type;michelson_annotation;decl_pos}), (_, {associated_type=associated_type';michelson_annotation=michelson_annotation';decl_pos=decl_pos'})) table =
         if Int.equal decl_pos decl_pos' && Option.equal String.equal michelson_annotation michelson_annotation' then
           self table associated_type associated_type'
         else
           raise.raise default_error in
       let table = List.fold_right elements ~f:aux ~init:table in
       table
     else
       raise.raise default_error
  | T_sum {content; layout}, T_sum {content=content'; layout=layout'} ->
     let content_kv = O.LMap.to_kv_list content in
     let content'_kv = O.LMap.to_kv_list content' in
     if layout_eq layout layout' &&
          List.equal equal_label (List.map content_kv ~f:fst) (List.map content'_kv ~f:fst) then
       let elements = List.zip_exn content_kv content'_kv in
       let aux ((_, {associated_type;michelson_annotation;decl_pos}), (_, {associated_type=associated_type';michelson_annotation=michelson_annotation';decl_pos=decl_pos'})) table =
         if Int.equal decl_pos decl_pos' && Option.equal String.equal michelson_annotation michelson_annotation' then
           self table associated_type associated_type'
         else
           raise.raise default_error in
       let table = List.fold_right elements ~f:aux ~init:table in
       table
     else
       raise.raise default_error
  | T_singleton l, T_singleton l' when Int.equal 0 (Stage_common.Enums.compare_literal l l') -> table
  | (T_arrow _ | T_record _ | T_sum _ | T_constant _ | T_module_accessor _ | T_singleton _ | T_abstraction _ | T_for_all _),
    (T_arrow _ | T_record _ | T_sum _ | T_constant _ | T_module_accessor _ | T_singleton _ | T_abstraction _ | T_for_all _ | T_variable _)
    -> raise.raise default_error

(* This function does some inference for applications: it takes a type
   `typed_matched` of the form `t1 -> ... -> tn -> t`, a list of types
   `args` of the form `[t'1;...;t'n]` (representing types on which the
   function is applied) and possibly a final type `tv_opt` of the form
   `t'` (representing an annotation for the final result).
   It will try to infer a table s.t. when substituting variables in
   `t1 -> ... -> tn -> t`, we get `t'1 -> ... > t'n -> t'`. It works
   by matching iteratively on each type: `t1` with `t'1`, ..., `tn`
   with `t'n`, and finally `t` with `t'`. *)
let infer_type_applications ~raise ~loc type_matched args tv_opt =
  let table, type_matched = List.fold_left args ~init:(TMap.empty, type_matched) ~f:(fun ((table, type_matched) : _ TMap.t * O.type_expression) matched ->
                  match type_matched.type_content with
                  | T_arrow { type1 ; type2 } ->
                     infer_type_application ~raise ~loc table type1 matched, type2
                  | (T_record _ | T_sum _ | T_constant _ | T_module_accessor _ | T_singleton _ | T_abstraction _ | T_for_all _ | T_variable _) ->
                     table, type_matched) in
  match tv_opt with
  | Some t -> infer_type_application ~raise ~loc ~default_error:(fun loc t t' -> assert_equal loc t' t) table type_matched t
  | None -> table

(* This wraps a `∀ a . (∀ b . (∀ c . some_type))` with type instantiations,
   e.g. given the table `[a ↦ int; b ↦ string; c ↦ bool` it will return
   `(((∀ a . (∀ b . (∀ c . some_type))) @@ int) @@ string) @@ bool` *)
let build_type_insts ~raise ~loc (forall : O.expression) table bound_variables =
  let bound_variables = List.rev bound_variables in
  let rec build_type_insts (forall : O.expression) = function
    | [] -> forall
    | av :: avs' ->
       let O.{ ty_binder ; type_ = t ; kind = _ } = trace_option ~raise (corner_case "Expected a for all type quantifier") @@ O.get_t_for_all forall.type_expression in
       assert (I.Var.equal ty_binder av);
       let type_ = trace_option ~raise (Errors.not_annotated loc) @@ TMap.find_opt av table in
       build_type_insts (make_e (E_type_inst {forall ; type_ }) (Ast_typed.Helpers.subst_type av type_ t)) avs' in
  build_type_insts forall bound_variables
