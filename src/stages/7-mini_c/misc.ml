open Types
open Ligo_prim

module Free_variables = struct
  type bindings = Value_var.t list

  let mem : bindings -> Value_var.t -> bool = List.mem ~equal:Value_var.equal
  let singleton : Value_var.t -> bindings = fun s -> [ s ]

  let mem_count : Value_var.t -> bindings -> int =
   fun x fvs -> List.length (List.filter ~f:(Value_var.equal x) fvs)


  let union : bindings -> bindings -> bindings = ( @ )
  let unions : bindings list -> bindings = List.concat
  let empty : bindings = []
  let of_list : Value_var.t list -> bindings = fun x -> x

  let rec expression ~count_var_ref ~count_deref ~count_assign
      : bindings -> expression -> bindings
    =
   fun b e ->
    let expression = expression ~count_var_ref ~count_deref ~count_assign in
    let self = expression b in
    match e.content with
    | E_variable n -> if count_var_ref then var_name b n else empty
    | E_deref x -> if count_deref then var_name b x else empty
    | E_assign (x, e) ->
      if count_assign then union (var_name b x) (self e) else union b (self e)
    | E_literal _ -> empty
    | E_closure f -> lambda ~count_var_ref ~count_deref ~count_assign b f
    | E_constant c -> unions @@ List.map ~f:self c.arguments
    | E_application (f, x) -> unions @@ [ self f; self x ]
    | E_iterator (_, ((v, _), body), expr) ->
      unions [ expression (union (singleton v) b) body; self expr ]
    | E_fold (((v, _), body), collection, initial) ->
      unions [ expression (union (singleton v) b) body; self collection; self initial ]
    | E_fold_right (((v, _), body), (collection, _elem_type), initial) ->
      unions [ expression (union (singleton v) b) body; self collection; self initial ]
    | E_if_bool (x, bt, bf) -> unions [ self x; self bt; self bf ]
    | E_if_none (x, bn, ((s, _), bs)) ->
      unions [ self x; self bn; expression (union (singleton s) b) bs ]
    | E_if_cons (x, bnil, (((h, _), (t, _)), bcons)) ->
      unions
        [ self x; self bnil; expression (unions [ singleton h; singleton t; b ]) bcons ]
    | E_if_left (x, ((l, _), bl), ((r, _), br)) ->
      unions
        [ self x
        ; expression (union (singleton l) b) bl
        ; expression (union (singleton r) b) br
        ]
    | E_let_in (expr, _, ((v, _), body)) ->
      unions [ self expr; expression (union (singleton v) b) body ]
    | E_tuple exprs -> unions (List.map ~f:self exprs)
    | E_let_tuple (expr, (fields, body)) ->
      unions
        [ self expr
        ; expression
            (unions (List.map ~f:(fun (x, _) -> singleton x) fields @ [ b ]))
            body
        ]
    | E_proj (expr, _i, _n) -> self expr
    | E_update (expr, _i, update, _n) -> unions [ self expr; self update ]
    | E_raw_michelson _ -> empty
    | E_global_constant (_hash, args) -> unions (List.map ~f:self args)
    (* the code is not allowed to have any free variables ... but
       maybe it still could if they are going to be inlined? *)
    | E_create_contract (_p, _s, ((x, _), code), args) ->
      let b = union (singleton x) b in
      union (expression b code) (unions (List.map ~f:self args))
    | E_let_mut_in (expr, ((x, _), body)) ->
      union (self expr) (expression (union (singleton x) b) body)
    | E_for (start, final, incr, ((x, _), body)) ->
      unions
        [ self start; self final; self incr; expression (union (singleton x) b) body ]
    | E_for_each (coll, _, (xs, body)) ->
      let b' = unions (List.map ~f:(fun (x, _) -> singleton x) xs @ [ b ]) in
      unions [ self coll; expression b' body ]
    | E_while (cond, body) -> unions [ self cond; self body ]


  and var_name : bindings -> var_name -> bindings =
   fun b n -> if mem b n then empty else singleton n


  and lambda ~count_var_ref ~count_deref ~count_assign
      : bindings -> anon_function -> bindings
    =
   fun b l ->
    let b = union (singleton l.binder) b in
    expression ~count_var_ref ~count_deref ~count_assign b l.body
end

let get_fv =
  Free_variables.expression ~count_var_ref:true ~count_deref:true ~count_assign:true


let assigned_and_free_vars =
  Free_variables.expression ~count_var_ref:false ~count_deref:false ~count_assign:true
