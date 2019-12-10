open Types
open Combinators
open Trace

module Errors = struct

  let missing_entry_point name =
    let title () = "missing entry point" in
    let content () = "no entry point with the given name" in
    let data = [
      ("name" , fun () -> name) ;
    ] in
    error ~data title content

  let not_functional_main name =
    let title () = "not functional main" in
    let content () = "main should be a function" in
    let data = [
      ("name" , fun () -> Format.asprintf "%s" name) ;
    ] in
    error ~data title content

end

module Free_variables = struct

  type bindings = expression_variable list
  let mem : expression_variable -> bindings -> bool = List.mem
  let singleton : expression_variable -> bindings = fun s -> [ s ]
  let mem_count : expression_variable -> bindings -> int =
    fun x fvs ->
    List.length (List.filter (Var.equal x) fvs)
  let union : bindings -> bindings -> bindings = (@)
  let unions : bindings list -> bindings = List.concat
  let empty : bindings = []
  let of_list : expression_variable list -> bindings = fun x -> x

  let rec expression : bindings -> expression -> bindings = fun b e ->
    let self = expression b in
    match e.content with
    | E_literal v -> value b v
    | E_closure f -> lambda b f
    | E_skip -> empty
    | E_constant (_, xs) -> unions @@ List.map self xs
    | E_application (f, x) -> unions @@ [ self f ; self x ]
    | E_variable n -> var_name b n
    | E_make_empty_map _ -> empty
    | E_make_empty_big_map _ -> empty
    | E_make_empty_list _ -> empty
    | E_make_empty_set _ -> empty
    | E_make_none _ -> empty
    | E_iterator (_, ((v, _), body), expr) ->
      unions [ expression (union (singleton v) b) body ;
               self expr ;
             ]
    | E_fold (((v, _), body), collection, initial) ->
      unions [ expression (union (singleton v) b) body ;
               self collection ;
               self initial ;
             ]
    | E_if_bool (x, bt, bf) -> unions [ self x ; self bt ; self bf ]
    | E_if_none (x, bn, ((s, _), bs)) ->
      unions [ self x ;
               self bn ;
               expression (union (singleton s) b) bs ;
             ]
    | E_if_cons (x, bnil , (((h, _) , (t, _)) , bcons)) ->
      unions [ self x ;
               self bnil ;
               expression (unions [ singleton h ; singleton t ; b ]) bcons ;
             ]
    | E_if_left (x, ((l, _), bl), ((r, _), br)) ->
      unions [ self x ;
               expression (union (singleton l) b) bl ;
               expression (union (singleton r) b) br ;
             ]
    | E_let_in ((v , _) , expr , body) ->
      unions [ self expr ;
               expression (union (singleton v) b) body ;
             ]
    | E_sequence (x, y) -> union (self x) (self y)
    (* NB different from ast_typed... *)
    | E_assignment (v, _, e) -> unions [ var_name b v ; self e ]
    | E_while (cond , body) -> union (self cond) (self body)

  and var_name : bindings -> var_name -> bindings = fun b n ->
    if mem n b
    then empty
    else singleton n

  and value : bindings -> value -> bindings = fun b v ->
    let self = value b in
    match v with
    | D_unit
    | D_bool _
    | D_nat _
    | D_timestamp _
    | D_mutez _
    | D_int _
    | D_string _
    | D_bytes _
    | D_none
    | D_operation _
      -> empty
    | D_pair (x, y) -> unions [ self x ; self y ]
    | D_left x
    | D_right x
    | D_some x
      -> self x
    | D_map kvs
    | D_big_map kvs
      -> unions @@ List.map (fun (k, v) -> unions [ self k ; self v ]) kvs
    | D_list xs
    | D_set xs
      -> unions @@ List.map self xs

  and lambda : bindings -> anon_function -> bindings = fun b l ->
    let b = union (singleton l.binder) b in
    expression b l.body

end

let get_entry (lst : program) (name : string) : (expression * int) result =
  let%bind entry_expression =
    trace_option (Errors.missing_entry_point name) @@
    let aux x =
      let (((decl_name , decl_expr) , _)) = x in
      if (Var.equal decl_name (Var.of_name name))
      then Some decl_expr
      else None
    in
    List.find_map aux lst
  in
  let entry_index =
    let aux x =
      let (((decl_name , _) , _)) = x in
      Var.equal decl_name (Var.of_name name)
    in
    List.find_index aux lst
  in
  ok (entry_expression , entry_index)

(*
  Assume the following program:
  ```
    const x = 42
    const y = 120
    const f = () -> x + y
  ```
  aggregate_entry program "f" (Some [unit]) would return:
  ```
    let x = 42 in
    let y = 120 in
    const f = () -> x + y
    f(unit)
  ```

  if arg_lst is None, it means that the entry point is not an arbitrary expression
*)
type form_t =
  | ContractForm of (expression * int)
  | ExpressionForm of ((expression * int) * expression list)

let aggregate_entry (lst : program) (form : form_t) : expression result =
  let (entry_expression , entry_index, arg_lst) = match form with
    | ContractForm (exp,i) -> (exp,i,[])
    | ExpressionForm ((exp,i),argl) -> (exp,i,argl) in
  let pre_declarations = List.until entry_index lst in
  let wrapper =
    let aux prec cur =
      let (((name , expr) , _)) = cur in
      e_let_in name expr.type_value expr prec
    in
    fun expr -> List.fold_right' aux expr pre_declarations
  in
  match (entry_expression.content , arg_lst) with
  | (E_closure _ , (hd::tl)) -> (
      let%bind type_value' = match entry_expression.type_value with
        | T_function (_,t) -> ok t
        | _ -> simple_fail "Trying to aggregate closure which does not have function type" in
      let entry_expression' = List.fold_left
        (fun acc el ->
          let type_value' = match acc.type_value with
            | T_function (_,t) -> t
            | e -> e in
          {
            content = E_application (acc,el) ;
            type_value = type_value' ;
          }
        )
        {
          content = E_application (entry_expression, hd) ;
          type_value = type_value' ;
        } tl in
      ok @@ wrapper entry_expression'
    )
  | (_ , _) -> (
      ok @@ wrapper entry_expression
    )