open Types

module Free_variables = struct

  type bindings = expression_variable list
  let var_equal = Location.equal_content ~equal:Var.equal
  let mem : bindings -> expression_variable -> bool = List.mem ~equal:var_equal
  let singleton : expression_variable -> bindings = fun s -> [ s ]
  let mem_count : expression_variable -> bindings -> int =
    fun x fvs ->
    List.length (List.filter ~f:(var_equal x) fvs)
  let union : bindings -> bindings -> bindings = (@)
  let unions : bindings list -> bindings = List.concat
  let empty : bindings = []
  let of_list : expression_variable list -> bindings = fun x -> x

  let rec expression : bindings -> expression -> bindings = fun b e ->
    let self = expression b in
    match e.content with
    | E_literal _ -> empty
    | E_closure f -> lambda b f
    | E_constant (c) -> unions @@ List.map ~f:self c.arguments
    | E_application (f, x) -> unions @@ [ self f ; self x ]
    | E_variable n -> var_name b n
    | E_iterator (_, ((v, _), body), expr) ->
      unions [ expression (union (singleton v) b) body ;
               self expr ;
             ]
    | E_fold (((v, _), body), collection, initial) ->
      unions [ expression (union (singleton v) b) body ;
               self collection ;
               self initial ;
             ]
    | E_fold_right (((v, _), body), (collection,_elem_type), initial) ->
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
    | E_let_in (expr, _ , ((v , _) , body) )->
      unions [ self expr ;
               expression (union (singleton v) b) body ;
             ]
    | E_tuple exprs ->
      unions (List.map ~f:self exprs)
    | E_let_tuple (expr, (fields , body)) ->
      unions [ self expr ;
               expression (unions (List.map ~f:(fun (x, _) -> singleton x) fields @ [b])) body
             ]
    | E_proj (expr, _i, _n) ->
      self expr
    | E_update (expr, _i, update, _n) ->
      unions [ self expr; self update ]
    | E_raw_michelson _ -> empty

  and var_name : bindings -> var_name -> bindings = fun b n ->
    if mem b n
    then empty
    else singleton n

  and lambda : bindings -> anon_function -> bindings = fun b l ->
    let b = union (singleton l.binder) b in
    expression b l.body

end
