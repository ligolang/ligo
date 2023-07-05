open Ligo_prim
open Types

(* This function parse te and replace all occurence of binder by value *)
let rec subst_type (binder : Type_var.t) (value : type_expression) (te : type_expression) =
  let self = subst_type binder value in
  let return type_content = { te with type_content } in
  match te.type_content with
  | T_variable var when Type_var.equal binder var -> value
  | T_variable _ -> te
  | T_constant { language; injection; parameters } ->
    let parameters = List.map ~f:self parameters in
    return @@ T_constant { language; injection; parameters }
  | T_singleton _ -> te
  | T_arrow { type1; type2 } ->
    let type1 = self type1 in
    let type2 = self type2 in
    return @@ T_arrow { type1; type2 }
  | T_sum m -> return @@ T_sum (Row.map self m)
  | T_record m -> return @@ T_record (Row.map self m)
  | T_for_all { ty_binder; kind; type_ } ->
    let type_ = self type_ in
    return @@ T_for_all { ty_binder; kind; type_ }


(* This function transforms a type `∀ v1 ... vn . t` into the pair `([ v1 ; .. ; vn ] , t)` *)
let destruct_for_alls (t : type_expression) =
  let rec destruct_for_alls type_vars (t : type_expression) =
    match t.type_content with
    | T_for_all { ty_binder; type_; _ } ->
      destruct_for_alls (ty_binder :: type_vars) type_
    | _ -> type_vars, t
  in
  destruct_for_alls [] t


(* This function transforms a type `t1 -> ... -> tn -> t` into the pair `([ t1 ; .. ; tn ] , t)` *)
let destruct_arrows_n (t : type_expression) (n : int) =
  let rec destruct_arrows type_vars (t : type_expression) =
    match t.type_content with
    | T_arrow { type1; type2 } when List.length type_vars < n ->
      destruct_arrows (type1 :: type_vars) type2
    | _ -> List.rev type_vars, t
  in
  destruct_arrows [] t


(* This function transforms a type `t1 -> ... -> tn -> t` into the pair `([ t1 ; .. ; tn ] , t)` *)
let destruct_arrows (t : type_expression) =
  let rec destruct_arrows type_vars (t : type_expression) =
    match t.type_content with
    | T_arrow { type1; type2 } -> destruct_arrows (type1 :: type_vars) type2
    | _ -> List.rev type_vars, t
  in
  destruct_arrows [] t


let assert_eq a b = if Caml.( = ) a b then Some () else None
let assert_same_size a b = if List.length a = List.length b then Some () else None

(* ~unforged_tickets allows type containing tickets to be decompiled to 'unforged' tickets (e.g. `int * int ticket` |-> `int * {ticketer : address ; value : int ; amount : nat }
   TODO: we could think of a better way to inject those "comparison expeptions" to assert_type_expression `*)
let rec assert_type_expression_eq
    ?(unforged_tickets = false)
    ((a, b) : type_expression * type_expression)
    : unit option
  =
  let open Simple_utils.Option in
  match a.type_content, b.type_content with
  | ( T_constant
        { language = _; injection = Ligo_prim.Literal_types.Ticket; parameters = [ _ty ] }
    , _human_t )
  | ( _human_t
    , T_constant
        { language = _; injection = Ligo_prim.Literal_types.Ticket; parameters = [ _ty ] }
    ) -> if unforged_tickets then Some () else None
  | ( T_constant { language = la; injection = ia; parameters = lsta }
    , T_constant { language = lb; injection = ib; parameters = lstb } ) ->
    if String.equal la lb && Literal_types.equal ia ib
    then
      let* _ = assert_same_size lsta lstb in
      List.fold_left
        ~f:(fun acc p ->
          match acc with
          | None -> None
          | Some () -> assert_type_expression_eq ~unforged_tickets p)
        ~init:(Some ())
        (List.zip_exn lsta lstb)
    else None
  | T_constant _, _ -> None
  | T_sum sa, T_sum sb ->
    let sa' = Record.to_list sa.fields in
    let sb' = Record.to_list sb.fields in
    let aux ((ka, va), (kb, vb)) =
      let* _ = assert_eq ka kb in
      assert_type_expression_eq ~unforged_tickets (va, vb)
    in
    let* _ = assert_same_size sa' sb' in
    List.fold_left
      ~f:(fun acc p ->
        match acc with
        | None -> None
        | Some () -> aux p)
      ~init:(Some ())
      (List.zip_exn sa' sb')
  | T_sum _, _ -> None
  | T_record ra, T_record rb
    when Bool.( <> ) (Record.is_tuple ra.fields) (Record.is_tuple rb.fields) -> None
  | T_record ra, T_record rb ->
    let sort_lmap r' = List.sort ~compare:(fun (a, _) (b, _) -> Label.compare a b) r' in
    let ra' = sort_lmap @@ Record.to_list ra.fields in
    let rb' = sort_lmap @@ Record.to_list rb.fields in
    let aux ((ka, va), (kb, vb)) =
      let* _ = assert_eq ka kb in
      assert_type_expression_eq ~unforged_tickets (va, vb)
    in
    let* _ = assert_eq ra.layout rb.layout in
    let* _ = assert_same_size ra' rb' in
    List.fold_left
      ~f:(fun acc p ->
        match acc with
        | None -> None
        | Some () -> aux p)
      ~init:(Some ())
      (List.zip_exn ra' rb')
  | T_record _, _ -> None
  | T_arrow { type1; type2 }, T_arrow { type1 = type1'; type2 = type2' } ->
    let* _ = assert_type_expression_eq ~unforged_tickets (type1, type1') in
    assert_type_expression_eq ~unforged_tickets (type2, type2')
  | T_arrow _, _ -> None
  | T_variable x, T_variable y ->
    (* TODO : we must check that the two types were bound at the same location (even if they have the same name), i.e. use something like De Bruijn indices or a propper graph encoding *)
    if Type_var.equal x y then Some () else None
  | T_variable _, _ -> None
  | T_singleton a, T_singleton b -> assert_literal_eq (a, b)
  | T_singleton _, _ -> None
  | T_for_all a, T_for_all b ->
    assert_type_expression_eq ~unforged_tickets (a.type_, b.type_)
    >>= fun _ -> Some (assert (Kind.equal a.kind b.kind))
  | T_for_all _, _ -> None


and assert_literal_eq ((a, b) : Literal_value.t * Literal_value.t) : unit option =
  if Literal_value.equal a b then Some () else None


type 'a fold_mapper = 'a -> expression -> bool * 'a * expression

let rec fold_map_expression : 'a fold_mapper -> 'a -> expression -> 'a * expression =
 fun f a e ->
  let self = fold_map_expression f in
  let idle acc a = acc, a in
  let continue, init, e' = f a e in
  if not continue
  then init, e'
  else (
    let return expression_content = { e' with expression_content } in
    match e'.expression_content with
    | E_matching { matchee; cases } ->
      let res, matchee = self init matchee in
      let res, cases = fold_map_cases f res cases in
      res, return @@ E_matching { matchee; cases }
    | E_record m ->
      let res, m' = Record.fold_map ~f:self ~init m in
      res, return @@ E_record m'
    | E_accessor acc ->
      let res, acc = Types.Accessor.fold_map self init acc in
      res, return @@ E_accessor acc
    | E_update u ->
      let res, u = Types.Update.fold_map self init u in
      res, return @@ E_update u
    | E_constructor c ->
      let res, e' = self init c.element in
      res, return @@ E_constructor { c with element = e' }
    | E_application { lamb; args } ->
      let ab = lamb, args in
      let res, (a, b) = Simple_utils.Pair.fold_map ~f:self ~init ab in
      res, return @@ E_application { lamb = a; args = b }
    | E_let_in { let_binder; rhs; let_result; attributes } ->
      let res, rhs = self init rhs in
      let res, let_result = self res let_result in
      res, return @@ E_let_in { let_binder; rhs; let_result; attributes }
    | E_type_inst { forall; type_ } ->
      let res, forall = self init forall in
      res, return @@ E_type_inst { forall; type_ }
    | E_lambda l ->
      let res, l = Lambda.fold_map self idle init l in
      res, return @@ E_lambda l
    | E_type_abstraction ta ->
      let res, ta = Type_abs.fold_map self init ta in
      res, return @@ E_type_abstraction ta
    | E_recursive r ->
      let res, r = Recursive.fold_map self idle init r in
      res, return @@ E_recursive r
    | E_constant c ->
      let res, c = Constant.fold_map self init c in
      res, return @@ E_constant c
    | E_raw_code { language; code } ->
      let res, code = self init code in
      res, return @@ E_raw_code { language; code }
    | E_assign a ->
      let res, a = Assign.fold_map self idle init a in
      res, return @@ E_assign a
    | E_coerce asc ->
      let res, asc = Ascription.fold_map self idle init asc in
      res, return @@ E_coerce asc
    | E_for f ->
      let res, f = For_loop.fold_map self init f in
      res, return @@ E_for f
    | E_for_each fe ->
      let res, fe = For_each_loop.fold_map self init fe in
      res, return @@ E_for_each fe
    | E_while w ->
      let res, w = While_loop.fold_map self init w in
      res, return @@ E_while w
    | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
      let res, rhs = self init rhs in
      let res, let_result = self res let_result in
      res, return @@ E_let_mut_in { let_binder; rhs; let_result; attributes }
    | (E_deref _ | E_literal _ | E_variable _) as e' -> init, return e')


and fold_map_case
    :  'a fold_mapper -> 'a -> (expression, type_expression) Types.Match_expr.match_case
    -> 'a * (expression, type_expression) Types.Match_expr.match_case
  =
 fun f init { pattern; body } ->
  let init, body = fold_map_expression f init body in
  init, { pattern; body }


and fold_map_cases
    :  'a fold_mapper -> 'a
    -> (expression, type_expression) Types.Match_expr.match_case list
    -> 'a * (expression, type_expression) Types.Match_expr.match_case list
  =
 fun f init ms -> List.fold_map ms ~init ~f:(fold_map_case f)


module Free_variables : sig
  val expression : expression -> Value_var.t list
  val expression_only_var : expression -> Value_var.t list
end = struct
  open Ligo_prim
  module VarSet = Caml.Set.Make (Value_var)

  type t =
    { var : VarSet.t
    ; mut_var : VarSet.t
    }

  let empty = { var = VarSet.empty; mut_var = VarSet.empty }
  let singleton_var v = { var = VarSet.singleton v; mut_var = VarSet.empty }
  let singleton_mut_var v = { var = VarSet.empty; mut_var = VarSet.singleton v }

  let remove name t =
    { var = VarSet.remove name t.var; mut_var = VarSet.remove name t.mut_var }


  let union { var = y1; mut_var = z1 } { var = y2; mut_var = z2 } =
    { var = VarSet.union y1 y2; mut_var = VarSet.union z1 z2 }


  let unions : t list -> t = fun l -> List.fold l ~init:empty ~f:union

  let rec get_fv_expr : expression -> t =
   fun e ->
    let self = get_fv_expr in
    match e.expression_content with
    | E_variable v -> singleton_var v
    | E_literal _ -> empty
    | E_raw_code { language = _; code } -> self code
    | E_constant { arguments; _ } -> unions @@ List.map ~f:self arguments
    | E_application { lamb; args } -> union (self lamb) (self args)
    | E_type_inst { forall; _ } -> self forall
    | E_lambda { binder; result; _ } ->
      let fv = self result in
      remove (Param.get_var binder) @@ fv
    | E_type_abstraction { type_binder = _; result } -> self result
    | E_recursive { fun_name; lambda = { binder; result; _ }; _ } ->
      let fv = self result in
      remove fun_name @@ remove (Param.get_var binder) fv
    | E_constructor { element; _ } -> self element
    | E_matching { matchee; cases } -> union (self matchee) (get_fv_cases cases)
    | E_record m ->
      let res = Record.map ~f:self m in
      let res = Record.values res in
      unions res
    | E_accessor { struct_; _ } -> self struct_
    | E_update { struct_; update; _ } -> union (self struct_) (self update)
    | E_let_in { let_binder; rhs; let_result; _ } ->
      let fv2 = self let_result in
      let fv2 =
        List.fold (Pattern.binders let_binder) ~init:fv2 ~f:(fun acc binder ->
            remove (Binder.get_var binder) acc)
      in
      union (self rhs) fv2
    | E_let_mut_in { let_binder; rhs; let_result; _ } ->
      let fv2 = self let_result in
      let fv2 =
        List.fold (Pattern.binders let_binder) ~init:fv2 ~f:(fun acc binder ->
            remove (Binder.get_var binder) acc)
      in
      union (self rhs) fv2
    | E_assign { binder; expression } ->
      union (singleton_mut_var (Binder.get_var binder)) (self expression)
    | E_coerce { anno_expr ; _ } ->
      self anno_expr
    | E_deref v -> singleton_mut_var v
    | E_for { binder; start; final; incr; f_body } ->
      unions [ self start; self final; self incr; remove binder (self f_body) ]
    | E_for_each { fe_binder = binder, None; collection; fe_body; collection_type = _ } ->
      unions [ self collection; remove binder (self fe_body) ]
    | E_for_each { fe_binder = binder1, Some binder2; collection; fe_body; _ } ->
      unions [ self collection; remove binder1 @@ remove binder2 @@ self fe_body ]
    | E_while { cond; body } -> union (self cond) (self body)


  and get_fv_cases : _ Types.Match_expr.match_case list -> t =
   fun m ->
    unions
    @@ List.map m ~f:(fun { pattern; body } ->
           let varSet = get_fv_expr body in
           let vars = Pattern.binders pattern |> List.map ~f:Binder.get_var in
           let varSet = List.fold vars ~init:varSet ~f:(fun vs v -> remove v vs) in
           varSet)


  let to_list x = VarSet.fold (fun v r -> v :: r) x []

  let expression e =
    let varSet = get_fv_expr e in
    to_list (VarSet.union varSet.mut_var varSet.var)


  let expression_only_var e =
    let varSet = get_fv_expr e in
    to_list varSet.var
end

type 'err mapper = expression -> expression

let rec map_expression : 'err mapper -> expression -> expression =
 fun f e ->
  let self = map_expression f in
  let self_type = Fun.id in
  let e' = f e in
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_matching { matchee = e; cases } ->
    let e' = self e in
    let cases' = map_cases f cases in
    return @@ E_matching { matchee = e'; cases = cases' }
  | E_record m ->
    let m' = Record.map ~f:self m in
    return @@ E_record m'
  | E_accessor acc ->
    let acc = Types.Accessor.map self acc in
    return @@ E_accessor acc
  | E_update u ->
    let u = Types.Update.map self u in
    return @@ E_update u
  | E_constructor c ->
    let c = Constructor.map self c in
    return @@ E_constructor c
  | E_application { lamb; args } ->
    let ab = lamb, args in
    let a, b = Simple_utils.Pair.map ~f:self ab in
    return @@ E_application { lamb = a; args = b }
  | E_let_in { let_binder; rhs; let_result; attributes } ->
    let rhs = self rhs in
    let let_result = self let_result in
    return @@ E_let_in { let_binder; rhs; let_result; attributes }
  | E_lambda l ->
    let l = Lambda.map self self_type l in
    return @@ E_lambda l
  | E_type_abstraction ta ->
    let ta = Type_abs.map self ta in
    return @@ E_type_abstraction ta
  | E_type_inst { forall; type_ } ->
    let forall = self forall in
    return @@ E_type_inst { forall; type_ }
  | E_recursive r ->
    let r = Recursive.map self self_type r in
    return @@ E_recursive r
  | E_constant c ->
    let args = List.map ~f:self c.arguments in
    return @@ E_constant { c with arguments = args }
  | E_assign a ->
    let a = Assign.map self (fun a -> a) a in
    return @@ E_assign a
  | E_for f ->
    let f = For_loop.map self f in
    return @@ E_for f
  | E_for_each fe ->
    let fe = For_each_loop.map self fe in
    return @@ E_for_each fe
  | E_while w ->
    let w = While_loop.map self w in
    return @@ E_while w
  | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    let rhs = self rhs in
    let let_result = self let_result in
    return @@ E_let_mut_in { let_binder; rhs; let_result; attributes }
  | E_raw_code { language; code } ->
    let code = self code in
    return @@ E_raw_code { language; code }
  | E_coerce asc ->
    let asc = Ascription.map self (fun a -> a) asc in
    return @@ E_coerce asc
  | (E_deref _ | E_literal _ | E_variable _) as e' -> return e'


and map_cases
    :  'err mapper -> _ Types.Match_expr.match_case list
    -> _ Types.Match_expr.match_case list
  =
 fun f m ->
  List.map m ~f:(Types.Match_expr.map_match_case (map_expression f) (fun t -> t))


and map_program : 'err mapper -> program -> program =
 fun g (ctxt, expr) ->
  let f d =
    Location.map
      (function
        | D_value { binder; expr; attr } ->
          D_value { binder; expr = map_expression g expr; attr }
        | D_irrefutable_match { pattern; expr; attr } ->
          D_irrefutable_match { pattern; expr = map_expression g expr; attr })
      d
  in
  let ctxt = List.map ~f ctxt in
  let expr = map_expression g expr in
  ctxt, expr
