open Types
open Ligo_prim

module Free_variables = struct
  type bindings = (Value_var.t * type_expression) list

  let eq (v1, _t1) (v2, _t2) = Value_var.equal v1 v2

  let mem : bindings -> Value_var.t -> bool =
   fun bs -> bs |> List.map ~f:fst |> List.mem ~equal:Value_var.equal


  let singleton : Value_var.t -> type_expression -> bindings = fun v t -> [ v, t ]

  let mem_count : Value_var.t -> bindings -> int =
   fun x fvs -> List.length (List.filter ~f:(fun (v, _) -> Value_var.equal x v) fvs)


  let union : bindings -> bindings -> bindings = ( @ )
  let unions : bindings list -> bindings = List.concat
  let empty : bindings = []
  let of_list : (Value_var.t * type_expression) list -> bindings = fun x -> x

  let rec expression ~count_var_ref ~count_deref ~count_assign
      : bindings -> expression -> bindings
    =
   fun b e ->
    let expression = expression ~count_var_ref ~count_deref ~count_assign in
    let self = expression b in
    match e.content with
    | E_variable n -> if count_var_ref then var_name b n e.type_expression else empty
    | E_deref x -> if count_deref then var_name b x e.type_expression else empty
    | E_assign (x, e) ->
      if count_assign
      then union (var_name b x e.type_expression) (self e)
      else union b (self e)
    | E_literal _ -> empty
    | E_closure f ->
      let src, _ =
        Option.value_exn ~here:[%here] @@ Combinators.get_t_function e.type_expression
      in
      lambda ~count_var_ref ~count_deref ~count_assign b f src
    | E_rec f ->
      rec_lambda ~count_var_ref ~count_deref ~count_assign b f e.type_expression
    | E_constant c -> unions @@ List.map ~f:self c.arguments
    | E_application (f, x) -> unions @@ [ self f; self x ]
    | E_iterator (_, ((v, t), body), expr) ->
      unions [ expression (union (singleton v t) b) body; self expr ]
    | E_fold (((v, t), body), collection, initial) ->
      unions [ expression (union (singleton v t) b) body; self collection; self initial ]
    | E_fold_right (((v, t), body), (collection, _elem_type), initial) ->
      unions [ expression (union (singleton v t) b) body; self collection; self initial ]
    | E_if_bool (x, bt, bf) -> unions [ self x; self bt; self bf ]
    | E_if_none (x, bn, ((s, t), bs)) ->
      unions [ self x; self bn; expression (union (singleton s t) b) bs ]
    | E_if_cons (x, bnil, (((h, ht), (t, tt)), bcons)) ->
      unions
        [ self x
        ; self bnil
        ; expression (unions [ singleton h ht; singleton t tt; b ]) bcons
        ]
    | E_if_left (x, ((l, lt), bl), ((r, rt), br)) ->
      unions
        [ self x
        ; expression (union (singleton l lt) b) bl
        ; expression (union (singleton r rt) b) br
        ]
    | E_let_in (expr, _, ((v, _), body)) ->
      unions [ self expr; expression (union (singleton v expr.type_expression) b) body ]
    | E_tuple exprs -> unions (List.map ~f:self exprs)
    | E_let_tuple (expr, (fields, body)) ->
      unions
        [ self expr
        ; expression
            (unions (List.map ~f:(fun (x, t) -> singleton x t) fields @ [ b ]))
            body
        ]
    | E_proj (expr, _i, _n) -> self expr
    | E_update (expr, _i, update, _n) -> unions [ self expr; self update ]
    | E_raw_michelson _ -> empty
    | E_global_constant (_hash, args) -> unions (List.map ~f:self args)
    (* the code is not allowed to have any free variables ... but
       maybe it still could if they are going to be inlined? *)
    | E_create_contract (_p, _s, ((x, t), code), args) ->
      let b = union (singleton x t) b in
      union (expression b code) (unions (List.map ~f:self args))
    | E_let_mut_in (expr, ((x, t), body)) ->
      union (self expr) (expression (union (singleton x t) b) body)
    | E_for (start, final, incr, ((x, t), body)) ->
      unions
        [ self start; self final; self incr; expression (union (singleton x t) b) body ]
    | E_for_each (coll, _, (xs, body)) ->
      let b' = unions (List.map ~f:(fun (x, t) -> singleton x t) xs @ [ b ]) in
      unions [ self coll; expression b' body ]
    | E_while (cond, body) -> unions [ self cond; self body ]


  and var_name : bindings -> var_name -> type_expression -> bindings =
   fun b n t -> if mem b n then empty else singleton n t


  and lambda ~count_var_ref ~count_deref ~count_assign
      : bindings -> anon_function -> type_expression -> bindings
    =
   fun b l t ->
    let b = union (singleton l.binder t) b in
    expression ~count_var_ref ~count_deref ~count_assign b l.body


  and rec_lambda ~count_var_ref ~count_deref ~count_assign
      : bindings -> rec_function -> type_expression -> bindings
    =
   fun b l t ->
    let b = union (singleton l.rec_binder t) b in
    let src, _ = Option.value_exn @@ Combinators.get_t_function t in
    lambda ~count_var_ref ~count_deref ~count_assign b l.func src
end

let get_fv =
  Free_variables.expression ~count_var_ref:true ~count_deref:true ~count_assign:true


let assigned_and_free_vars bs e =
  Free_variables.expression
    ~count_var_ref:false
    ~count_deref:false
    ~count_assign:true
    bs
    e
