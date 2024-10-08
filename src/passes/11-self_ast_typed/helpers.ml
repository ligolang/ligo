open Core
open Ligo_prim
open Ast_typed
open Ast_typed.Helpers
module Ligo_pair = Simple_utils.Ligo_pair

type 'a decl_folder = 'a -> declaration -> 'a
type 'a folder = 'a -> expression -> 'a

let rec fold_expression : 'a folder -> 'a -> expression -> 'a =
 fun f init e ->
  let self = fold_expression f in
  let self_type = Fun.const in
  let init = f init e in
  match e.expression_content with
  | E_literal _
  | E_variable _
  | E_contract _
  | E_raw_code _
  | E_module_accessor _
  | E_error _ -> init
  | E_constant { arguments = lst; cons_name = _ } ->
    let res = List.fold ~f:self ~init lst in
    res
  | E_application { lamb; args } ->
    let ab = lamb, args in
    let res = Ligo_pair.fold ~f:self ~init ab in
    res
  | E_type_inst { forall = e; type_ = _ }
  | E_lambda { binder = _; output_type = _; result = e }
  | E_type_abstraction { type_binder = _; result = e }
  | E_recursive
      { lambda = { result = e; output_type = _; binder = _ }
      ; fun_name = _
      ; fun_type = _
      ; force_lambdarec = _
      }
  | E_constructor { element = e; constructor = _ } ->
    let res = self init e in
    res
  | E_matching { matchee = e; cases } ->
    let res = self init e in
    let res = fold_cases f res cases in
    res
  | E_union_injected inj -> Union.Injected.fold self self_type init inj
  | E_union_match match_ -> Union.Match.fold self self_type init match_
  | E_union_use use -> Union.Use.fold self init use
  | E_record m ->
    let res = Record.fold ~f:self ~init m in
    res
  | E_update { struct_; update; path = _ } ->
    let res = self init struct_ in
    let res = fold_expression self res update in
    res
  | E_accessor { struct_; path = _ } ->
    let res = self init struct_ in
    res
  | E_let_in { let_binder = _; rhs; let_result; attributes = _ } ->
    let res = self init rhs in
    let res = self res let_result in
    res
  | E_mod_in { module_binder = _; rhs; let_result } ->
    let res = fold_expression_in_module_expr self init rhs in
    let res = self res let_result in
    res
  | E_let_mut_in { let_binder = _; rhs; let_result; attributes = _ } ->
    let res = self init rhs in
    let res = self res let_result in
    res
  | E_deref _ -> init
  | E_assign a -> Assign.fold self self_type init a
  | E_coerce a -> Ascription.fold self self_type init a
  | E_for f -> For_loop.fold self init f
  | E_for_each fe -> For_each_loop.fold self init fe
  | E_while w -> While_loop.fold self init w


and fold_expression_in_module self acc (decls : declaration list) =
  List.fold decls ~init:acc ~f:(fun acc x ->
      match x.wrap_content with
      | D_value x -> self acc x.expr
      | D_irrefutable_match x -> self acc x.expr
      | D_module x -> fold_expression_in_module_expr self acc x.module_
      | D_module_include x -> fold_expression_in_module_expr self acc x
      | D_type _ -> acc
      | D_signature _ -> acc
      | D_import _ -> acc)


and fold_expression_in_module_expr : ('a -> expression -> 'a) -> 'a -> module_expr -> 'a =
 fun self acc x ->
  match x.module_content with
  | M_struct decls -> fold_expression_in_module self acc decls
  | M_module_path _ -> acc
  | M_variable _ -> acc


and fold_cases : 'a folder -> 'a -> _ Match_expr.match_case list -> 'a =
 fun f init m ->
  List.fold m ~init ~f:(fun init { body; _ } -> fold_expression f init body)


type ty_mapper = type_expression -> unit
type 'err mapper = expression -> expression

let rec map_expression : 'err mapper -> expression -> expression =
 fun f e ->
  let self = map_expression f in
  let e' = f e in
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_matching { matchee = e; cases } ->
    let e' = self e in
    let cases' = map_cases f cases in
    return @@ E_matching { matchee = e'; cases = cases' }
  | E_union_injected inj ->
    let inj = Union.Injected.map self Fn.id inj in
    return @@ E_union_injected inj
  | E_union_match match_ ->
    let match_ = Union.Match.map self Fn.id match_ in
    return @@ E_union_match match_
  | E_union_use use ->
    let use = Union.Use.map self use in
    return @@ E_union_use use
  | E_accessor { struct_; path } ->
    let struct_ = self struct_ in
    return @@ E_accessor { struct_; path }
  | E_record m ->
    let m' = Record.map ~f:self m in
    return @@ E_record m'
  | E_update { struct_; path; update } ->
    let struct_ = self struct_ in
    let update = self update in
    return @@ E_update { struct_; path; update }
  | E_constructor c ->
    let e' = self c.element in
    return @@ E_constructor { c with element = e' }
  | E_application { lamb; args } ->
    let ab = lamb, args in
    let a, b = Ligo_pair.map ~f:self ab in
    return @@ E_application { lamb = a; args = b }
  | E_let_in { let_binder; rhs; let_result; attributes } ->
    let rhs = self rhs in
    let let_result = self let_result in
    return @@ E_let_in { let_binder; rhs; let_result; attributes }
  | E_mod_in { module_binder; rhs; let_result } ->
    let rhs = map_expression_in_module_expr f rhs in
    let let_result = self let_result in
    return @@ E_mod_in { module_binder; rhs; let_result }
  | E_lambda { binder; output_type; result } ->
    let result = self result in
    return @@ E_lambda { binder; output_type; result }
  | E_type_abstraction ta ->
    let ta = Type_abs.map self ta in
    return @@ E_type_abstraction ta
  | E_type_inst { forall; type_ } ->
    let forall = self forall in
    return @@ E_type_inst { forall; type_ }
  | E_recursive
      { fun_name; fun_type; lambda = { binder; output_type; result }; force_lambdarec } ->
    let result = self result in
    return
    @@ E_recursive
         { fun_name; fun_type; lambda = { binder; output_type; result }; force_lambdarec }
  | E_constant c ->
    let args = List.map ~f:self c.arguments in
    return @@ E_constant { c with arguments = args }
  | E_module_accessor ma -> return @@ E_module_accessor ma
  | E_assign a ->
    let a = Assign.map self (fun a -> a) a in
    return @@ E_assign a
  | E_coerce asc ->
    let asc = Ascription.map self (fun a -> a) asc in
    return @@ E_coerce asc
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
  | (E_deref _ | E_literal _ | E_variable _ | E_contract _ | E_raw_code _ | E_error _) as
    e' -> return e'


and map_expression_in_module_expr
    : (expression -> expression) -> module_expr -> module_expr
  =
 fun self x ->
  let return module_content : module_expr = { x with module_content } in
  match x.module_content with
  | M_struct decls ->
    let decls = map_module self decls in
    return (M_struct decls)
  | M_module_path _ -> x
  | M_variable _ -> x


and map_cases
    : 'err mapper -> _ Match_expr.match_case list -> _ Match_expr.match_case list
  =
 fun f m -> List.map m ~f:(Match_expr.map_match_case (map_expression f) (fun t -> t))


and map_declaration m (x : declaration) =
  let return (d : declaration_content) = { x with wrap_content = d } in
  match x.wrap_content with
  | D_value { binder; expr; attr } ->
    let expr = map_expression m expr in
    return @@ D_value { binder; expr; attr }
  | D_irrefutable_match { pattern; expr; attr } ->
    let expr = map_expression m expr in
    return @@ D_irrefutable_match { pattern; expr; attr }
  | D_type t -> return @@ D_type t
  | D_module { module_binder; module_; module_attr; annotation } ->
    let module_ = map_expression_in_module_expr m module_ in
    return @@ D_module { module_binder; module_; module_attr; annotation }
  | D_module_include module_ ->
    return @@ D_module_include (map_expression_in_module_expr m module_)
  | D_signature ds -> return @@ D_signature ds
  | D_import import -> return @@ D_import import


and map_decl m d = map_declaration m d
and map_module : 'err mapper -> module_ -> module_ = fun m -> List.map ~f:(map_decl m)

and map_program : 'err mapper -> program -> program =
 fun m p -> { p with pr_module = List.map ~f:(map_declaration m) p.pr_module }


(* update_module [mp] [f] [p] looks for the module struct to which
   module path [mp] points in program [p] and replaces it by applying
   [f] to [mp] *)
let update_module (type a) module_path (f : module_ -> module_ * a) (module_ : module_)
    : module_ * a
  =
  let rec find_module acc module_path decls =
    match decls, module_path with
    | _, [] -> `Updated (f (List.rev decls))
    | [], _ -> `Not_here module_path
    | decl :: rest, m :: ms ->
      let loc = Location.get_location decl in
      (match Location.unwrap decl with
      | D_module
          { module_binder
          ; module_ =
              { module_content = M_struct inner_decls; module_location; signature }
          ; module_attr
          ; annotation = ()
          }
        when Module_var.equal module_binder m ->
        (match find_module [] ms (List.rev inner_decls) with
        | `Updated (inner_decls, a) ->
          let decl =
            Location.wrap ~loc
            @@ D_module
                 { module_binder
                 ; module_ =
                     { module_content = M_struct inner_decls; module_location; signature }
                 ; module_attr
                 ; annotation = ()
                 }
          in
          `Updated (List.rev rest @ (decl :: acc), a)
        | `Not_here module_path -> find_module (decl :: acc) module_path rest)
      | D_module
          { module_binder
          ; module_ = { module_content = M_variable module_var; _ }
          ; module_attr = _
          ; annotation = ()
          }
        when Module_var.equal module_binder m ->
        find_module (decl :: acc) (module_var :: ms) rest
      | D_module
          { module_binder
          ; module_ = { module_content = M_module_path module_path'; _ }
          ; module_attr = _
          ; annotation = ()
          }
        when Module_var.equal module_binder m ->
        find_module (decl :: acc) (Nonempty_list.to_list module_path' @ ms) rest
      | _ -> find_module (decl :: acc) module_path rest)
  in
  match find_module [] module_path (List.rev module_) with
  | `Updated prg -> prg
  | `Not_here f ->
    failwith
      (Format.asprintf
         "Module %a not found with last %a"
         Simple_utils.PP_helpers.(list_sep_d Module_var.pp)
         module_path
         Simple_utils.PP_helpers.(list_sep_d Module_var.pp)
         f)


(* TODO: this is unused ; used this instead of Contract_passes.get_fv_program I think??? *)
module Free_variables : sig
  val expression : expression -> Module_var.t list * Value_var.t list * Value_var.t list
end = struct
  module VarSet = Set.Make (Value_var)
  module ModVarSet = Set.Make (Module_var)
  module VarMap = Map.Make (Module_var)

  type moduleEnv' =
    { modVarSet : ModVarSet.t
    ; moduleEnv : moduleEnv
    ; varSet : VarSet.t
    ; mutSet : VarSet.t
    }

  and moduleEnv = moduleEnv' VarMap.t

  let empty =
    { modVarSet = ModVarSet.empty
    ; moduleEnv = VarMap.empty
    ; varSet = VarSet.empty
    ; mutSet = VarSet.empty
    }


  let union f m1 m2 =
    let f ~key = function
      | `Left v1 -> Some v1
      | `Right v2 -> Some v2
      | `Both (v1, v2) -> f key v1 v2
    in
    Map.merge ~f m1 m2


  let rec merge
      { modVarSet = x1; moduleEnv = y1; varSet = z1; mutSet = m1 }
      { modVarSet = x2; moduleEnv = y2; varSet = z2; mutSet = m2 }
    =
    let aux : Module_var.t -> moduleEnv' -> moduleEnv' -> moduleEnv' option =
     fun _ a b -> Some (merge a b)
    in
    { modVarSet = Set.union x1 x2
    ; moduleEnv = union aux y1 y2
    ; varSet = Set.union z1 z2
    ; mutSet = Set.union m1 m2
    }


  let unions : moduleEnv' list -> moduleEnv' =
   fun l ->
    List.fold
      l
      ~init:
        { modVarSet = ModVarSet.empty
        ; moduleEnv = VarMap.empty
        ; varSet = VarSet.empty
        ; mutSet = VarSet.empty
        }
      ~f:merge


  let rec get_fv_expr : expression -> moduleEnv' =
   fun e ->
    let self = get_fv_expr in
    match e.expression_content with
    | E_contract x ->
      { modVarSet = ModVarSet.of_list (Nonempty_list.to_list x)
      ; moduleEnv = VarMap.empty
      ; varSet = VarSet.empty
      ; mutSet = VarSet.empty
      }
    | E_variable v ->
      { modVarSet = ModVarSet.empty
      ; moduleEnv = VarMap.empty
      ; varSet = VarSet.singleton v
      ; mutSet = VarSet.empty
      }
    | E_literal _ | E_raw_code _ ->
      { modVarSet = ModVarSet.empty
      ; moduleEnv = VarMap.empty
      ; varSet = VarSet.empty
      ; mutSet = VarSet.empty
      }
    | E_constant { cons_name = _; arguments } -> unions @@ List.map ~f:self arguments
    | E_application { lamb; args } -> merge (self lamb) (self args)
    | E_type_inst { forall; type_ = _ } -> self forall
    | E_lambda { binder; output_type = _; result } ->
      let env = self result in
      (match Param.get_mut_flag binder with
      | Immutable -> { env with varSet = Set.remove env.varSet (Param.get_var binder) }
      | Mutable -> { env with mutSet = Set.remove env.mutSet (Param.get_var binder) })
    | E_type_abstraction { type_binder = _; result } -> self result
    | E_recursive
        { fun_name
        ; lambda = { binder; output_type = _; result }
        ; fun_type = _
        ; force_lambdarec = _
        } ->
      let { modVarSet; moduleEnv; varSet = fv; mutSet } = self result in
      { modVarSet
      ; moduleEnv
      ; varSet = Set.remove (Set.remove fv (Param.get_var binder)) fun_name
      ; mutSet
      }
    | E_constructor { constructor = _; element } -> self element
    | E_matching { matchee; cases } -> merge (self matchee) (get_fv_cases cases)
    | E_union_injected inj -> self (Union.Injected.expr_in_source inj)
    | E_union_match match_ ->
      let matchee, branches = Union.Match.(matchee match_, branches match_) in
      merge (self matchee) (get_fv_match_branches branches)
    | E_union_use use -> self (Union.Use.before_expansion use)
    | E_record m ->
      let res = Record.map ~f:self m in
      let res = Record.values res in
      unions res
    | E_update { struct_; update; path = _ } -> merge (self struct_) (self update)
    | E_accessor { struct_; path = _ } -> self struct_
    | E_let_in { let_binder; rhs; let_result; attributes = _ } ->
      let { modVarSet; moduleEnv; varSet = fv2; mutSet } = self let_result in
      let binders = Pattern.binders let_binder in
      let fv2 =
        List.fold binders ~init:fv2 ~f:(fun fv2 b -> Set.remove fv2 (Binder.get_var b))
      in
      merge (self rhs) { modVarSet; moduleEnv; varSet = fv2; mutSet }
    | E_mod_in { module_binder; rhs; let_result } ->
      let { modVarSet; moduleEnv; varSet; mutSet } = self let_result in
      let modVarSet = Set.remove modVarSet module_binder in
      merge (get_fv_module_expr rhs) { modVarSet; moduleEnv; varSet; mutSet }
    | E_module_accessor { module_path; element } ->
      ignore element;
      { modVarSet = ModVarSet.of_list module_path (* FIXME: not sure about that *)
      ; moduleEnv = VarMap.empty
      ; varSet = VarSet.empty
      ; mutSet = VarSet.empty
      }
    | E_assign { binder; expression } ->
      let fvs = self expression in
      { fvs with mutSet = Set.add fvs.mutSet (Binder.get_var binder) }
    | E_coerce { anno_expr; _ } -> self anno_expr
    | E_for { binder; start; final; incr; f_body } ->
      let f_body_fvs = self f_body in
      let f_body_fvs = { f_body_fvs with mutSet = Set.remove f_body_fvs.mutSet binder } in
      unions [ self start; self final; self incr; f_body_fvs ]
    | E_for_each { fe_binder = binder1, binder2; collection; fe_body; _ } ->
      let binders =
        binder1 :: Option.value_map binder2 ~f:(fun binder2 -> [ binder2 ]) ~default:[]
        |> VarSet.of_list
      in
      let fe_body_fvs = self fe_body in
      let fe_body_fvs =
        { fe_body_fvs with mutSet = Set.diff fe_body_fvs.mutSet binders }
      in
      unions [ self collection; fe_body_fvs ]
    | E_while { cond; body } -> unions [ self cond; self body ]
    | E_deref mut_var -> { empty with mutSet = VarSet.singleton mut_var }
    | E_let_mut_in { let_binder; rhs; let_result; attributes = _ } ->
      let { modVarSet; moduleEnv; varSet; mutSet = fv2 } = self let_result in
      let binders = Pattern.binders let_binder in
      let fv2 =
        List.fold binders ~init:fv2 ~f:(fun fv2 b -> Set.remove fv2 (Binder.get_var b))
      in
      merge (self rhs) { modVarSet; moduleEnv; varSet; mutSet = fv2 }
    | E_error _ -> empty


  and get_fv_cases : _ Match_expr.match_case list -> moduleEnv' =
   fun m ->
    unions
    @@ List.map m ~f:(fun { pattern; body } ->
           let { modVarSet; moduleEnv; varSet; mutSet } = get_fv_expr body in
           let vars = Pattern.binders pattern |> List.map ~f:Binder.get_var in
           let varSet = List.fold vars ~init:varSet ~f:(fun vs v -> Set.remove vs v) in
           { modVarSet; moduleEnv; varSet; mutSet })


  and get_fv_match_branches : _ Union.Match.Branch.t list -> moduleEnv' =
   fun m ->
    unions
    @@ List.map m ~f:(fun branch ->
           let pattern, body = Union.Match.Branch.(pattern branch, body branch) in
           let { modVarSet; moduleEnv; varSet; mutSet } = get_fv_expr body in
           let var = Union.Match.Pattern.var pattern in
           let varSet = Set.remove varSet var in
           { modVarSet; moduleEnv; varSet; mutSet })


  and get_fv_module_expr : module_expr -> moduleEnv' =
   fun x ->
    match x.module_content with
    | M_struct prg -> get_fv_module prg
    | M_variable _ ->
      { modVarSet = ModVarSet.empty
      ; moduleEnv = VarMap.empty
      ; varSet = VarSet.empty
      ; mutSet = VarSet.empty
      }
    | M_module_path _ ->
      { modVarSet = ModVarSet.empty
      ; moduleEnv = VarMap.empty
      ; varSet = VarSet.empty
      ; mutSet = VarSet.empty
      }


  and get_fv_module : module_ -> moduleEnv' =
   fun m ->
    let aux x =
      match Location.unwrap x with
      | D_value { binder = _; expr; attr = _ } -> get_fv_expr expr
      | D_irrefutable_match { pattern = _; expr; attr = _ } -> get_fv_expr expr
      | D_module_include module_
      | D_module { module_binder = _; module_; module_attr = _; annotation = () } ->
        get_fv_module_expr module_
      | D_type _t -> empty
      | D_signature _s -> empty
      | D_import _import -> empty
    in
    unions @@ List.map ~f:aux m


  let expression e =
    let { modVarSet; moduleEnv = _; varSet; mutSet } = get_fv_expr e in
    let fmv = Set.fold ~f:(fun r v -> v :: r) modVarSet ~init:[] in
    let fv = Set.fold ~f:(fun r v -> v :: r) varSet ~init:[] in
    let fmutvs = Set.fold ~f:(fun r v -> v :: r) mutSet ~init:[] in
    fmv, fv, fmutvs
end

module Declaration_mapper = struct
  type 'err mapper = declaration -> declaration

  let rec map_expression : 'err mapper -> expression -> expression =
   fun f e ->
    let self = map_expression f in
    let return expression_content = { e with expression_content } in
    match e.expression_content with
    | E_matching { matchee = e; cases } ->
      let e' = self e in
      let cases' = map_cases f cases in
      return @@ E_matching { matchee = e'; cases = cases' }
    | E_union_injected injected ->
      let injected = Union.Injected.map self Fn.id injected in
      return @@ E_union_injected injected
    | E_union_match match_ ->
      let match_ = Union.Match.map self Fn.id match_ in
      return @@ E_union_match match_
    | E_union_use use ->
      let use = Union.Use.map self use in
      return @@ E_union_use use
    | E_accessor { struct_; path } ->
      let struct_ = self struct_ in
      return @@ E_accessor { struct_; path }
    | E_record m ->
      let m' = Record.map ~f:self m in
      return @@ E_record m'
    | E_update { struct_; path; update } ->
      let struct_ = self struct_ in
      let update = self update in
      return @@ E_update { struct_; path; update }
    | E_constructor c ->
      let e' = self c.element in
      return @@ E_constructor { c with element = e' }
    | E_application { lamb; args } ->
      let ab = lamb, args in
      let a, b = Ligo_pair.map ~f:self ab in
      return @@ E_application { lamb = a; args = b }
    | E_let_in { let_binder; rhs; let_result; attributes } ->
      let rhs = self rhs in
      let let_result = self let_result in
      return @@ E_let_in { let_binder; rhs; let_result; attributes }
    | E_mod_in { module_binder; rhs; let_result } ->
      let rhs = map_expression_in_module_expr f rhs in
      let let_result = self let_result in
      return @@ E_mod_in { module_binder; rhs; let_result }
    | E_lambda { binder; output_type; result } ->
      let result = self result in
      return @@ E_lambda { binder; output_type; result }
    | E_type_abstraction ta ->
      let ta = Type_abs.map self ta in
      return @@ E_type_abstraction ta
    | E_type_inst { forall; type_ } ->
      let forall = self forall in
      return @@ E_type_inst { forall; type_ }
    | E_recursive
        { fun_name; fun_type; lambda = { binder; output_type; result }; force_lambdarec }
      ->
      let result = self result in
      return
      @@ E_recursive
           { fun_name
           ; fun_type
           ; lambda = { binder; output_type; result }
           ; force_lambdarec
           }
    | E_constant c ->
      let args = List.map ~f:self c.arguments in
      return @@ E_constant { c with arguments = args }
    | E_module_accessor ma -> return @@ E_module_accessor ma
    | E_assign a ->
      let a = Assign.map self (fun a -> a) a in
      return @@ E_assign a
    | E_coerce a ->
      let a = Ascription.map self (fun a -> a) a in
      return @@ E_coerce a
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
    | (E_deref _ | E_literal _ | E_variable _ | E_contract _ | E_raw_code _ | E_error _)
      as e' -> return e'


  and map_expression_in_module_expr
      : (declaration -> declaration) -> module_expr -> module_expr
    =
   fun f x ->
    let return module_content : module_expr = { x with module_content } in
    match x.module_content with
    | M_struct decls ->
      let decls = map_module f decls in
      return (M_struct decls)
    | M_module_path _ -> x
    | M_variable _ -> x


  and map_cases
      : 'err mapper -> _ Match_expr.match_case list -> _ Match_expr.match_case list
    =
   fun f m -> List.map m ~f:(Match_expr.map_match_case (map_expression f) (fun t -> t))


  and map_declaration f (x : declaration) =
    let return (d : declaration_content) = { x with wrap_content = d } in
    let x = f x in
    match Location.unwrap x with
    | D_value { binder; expr; attr } ->
      let expr = map_expression f expr in
      return @@ D_value { binder; expr; attr }
    | D_type t -> return @@ D_type t
    | D_module { module_binder; module_; module_attr; annotation } ->
      let module_ = map_expression_in_module_expr f module_ in
      return @@ D_module { module_binder; module_; module_attr; annotation }
    | D_irrefutable_match { pattern; expr; attr } ->
      let expr = map_expression f expr in
      return @@ D_irrefutable_match { pattern; expr; attr }
    | D_module_include module_ ->
      let module_ = map_expression_in_module_expr f module_ in
      return @@ D_module_include module_
    | D_signature signature -> return @@ D_signature signature
    | D_import import -> return @@ D_import import


  and map_decl m d = map_declaration m d
  and map_module : 'err mapper -> module_ -> module_ = fun m -> List.map ~f:(map_decl m)
end

module Type_mapper = struct
  type mapper = type_expression -> type_expression

  let rec map_expression (f : mapper) (expr : expression) : expression =
    let map_expr expr' = map_expression f expr' in
    let map_type typ' = map_type_expression f typ' in
    let map_mod mod' = map_module_expression f mod' in
    let return expression_content =
      let type_expression = map_type expr.type_expression in
      { expr with expression_content; type_expression }
    in
    match expr.expression_content with
    | E_matching x -> return @@ E_matching (Match_expr.map map_expr map_type x)
    | E_union_injected x ->
      return @@ E_union_injected (Union.Injected.map map_expr map_type x)
    | E_union_match x -> return @@ E_union_match (Union.Match.map map_expr map_type x)
    | E_union_use x -> return @@ E_union_use (Union.Use.map map_expr x)
    | E_accessor x -> return @@ E_accessor (Accessor.map map_expr x)
    | E_record x -> return @@ E_record (Record.map ~f:map_expr x)
    | E_update x -> return @@ E_update (Update.map map_expr x)
    | E_constructor x -> return @@ E_constructor (Constructor.map map_expr x)
    | E_application x -> return @@ E_application (Application.map map_expr x)
    | E_let_in x -> return @@ E_let_in (Let_in.map map_expr map_type x)
    | E_mod_in x -> return @@ E_mod_in (Mod_in.map map_expr map_mod x)
    | E_lambda x -> return @@ E_lambda (Lambda.map map_expr map_type x)
    | E_type_abstraction x -> return @@ E_type_abstraction (Type_abs.map map_expr x)
    | E_type_inst x ->
      return @@ E_type_inst { forall = map_expr x.forall; type_ = map_type x.type_ }
    | E_recursive x -> return @@ E_recursive (Recursive.map map_expr map_type x)
    | E_constant x -> return @@ E_constant (Constant.map map_expr x)
    | E_assign x -> return @@ E_assign (Assign.map map_expr map_type x)
    | E_coerce x -> return @@ E_coerce (Ascription.map map_expr map_type x)
    | E_for x -> return @@ E_for (For_loop.map map_expr x)
    | E_for_each x -> return @@ E_for_each (For_each_loop.map map_expr x)
    | E_while x -> return @@ E_while (While_loop.map map_expr x)
    | E_let_mut_in x -> return @@ E_let_mut_in (Let_in.map map_expr map_type x)
    | ( E_module_accessor _
      | E_deref _
      | E_literal _
      | E_variable _
      | E_contract _
      | E_raw_code _
      | E_error _ ) as expr_content -> return expr_content


  and map_type_expression (f : mapper) (typ : type_expression) : type_expression =
    let map_type typ' = map_type_expression f typ' in
    let return type_content = { typ with type_content } in
    f
      (match typ.type_content with
      | T_constant { language; injection; parameters } ->
        let parameters = List.map ~f:map_type parameters in
        return @@ T_constant { language; injection; parameters }
      | T_sum row -> return @@ T_sum (Row.map map_type row)
      | T_union union -> return @@ T_union (Union.map map_type union)
      | T_record row -> return @@ T_record (Row.map map_type row)
      | T_arrow arr -> return @@ T_arrow (Arrow.map map_type arr)
      | T_abstraction abs -> return @@ T_abstraction (Abstraction.map map_type abs)
      | T_for_all abs -> return @@ T_for_all (Abstraction.map map_type abs)
      | (T_variable _ | T_exists _ | T_singleton _) as type_content -> return type_content)


  and map_module_expression (f : mapper) (module_ : module_expr) : module_expr =
    let return module_content : module_expr = { module_ with module_content } in
    let map_mod mod' = map_module f mod' in
    match module_.module_content with
    | M_struct decls -> return (M_struct (map_mod decls))
    | (M_module_path _ | M_variable _) as module_content -> return module_content


  and map_declaration (f : mapper) (decl : declaration) : declaration =
    let return (d : declaration_content) = { decl with wrap_content = d } in
    let map_expr expr' = map_expression f expr' in
    let map_type typ' = map_type_expression f typ' in
    let map_mod mod' = map_module_expression f mod' in
    let map_sig sig' = map_signature f sig' in
    match Location.unwrap decl with
    | D_value val_decl -> return @@ D_value (Value_decl.map map_expr map_type val_decl)
    | D_type t -> return @@ D_type (Type_decl.map map_type t)
    | D_module module_decl ->
      return @@ D_module (Module_decl.map map_mod (fun () -> ()) module_decl)
    | D_irrefutable_match irr_match_decl ->
      return @@ D_irrefutable_match (Pattern_decl.map map_expr map_type irr_match_decl)
    | D_module_include module_ -> return @@ D_module_include (map_mod module_)
    | D_signature signature_decl ->
      return @@ D_signature (Signature_decl.map map_sig signature_decl)
    | D_import _ as decl_content -> return decl_content


  and map_module (f : mapper) (mod_ : module_) : module_ =
    List.map ~f:(map_declaration f) mod_


  and map_signature_item (f : mapper) (sig_item : sig_item) =
    let return (wrap_content : sig_item_content) = { sig_item with wrap_content } in
    let map_type typ' = map_type_expression f typ' in
    let map_sig sig' = map_signature f sig' in
    match sig_item.wrap_content with
    | S_value (var, typ, attr) -> return @@ S_value (var, map_type typ, attr)
    | S_type (type_var, typ, attr) -> return @@ S_type (type_var, map_type typ, attr)
    | S_type_var _ as sig_item_content -> return sig_item_content
    | S_module (module_var, sig_) -> return @@ S_module (module_var, map_sig sig_)
    | S_module_type (module_var, sig_) -> return @@ S_module (module_var, map_sig sig_)


  and map_signature_sort (f : mapper) (sig_sort : signature_sort) =
    match sig_sort with
    | Ss_module -> Ss_module
    | Ss_contract contract_sig -> Ss_contract (map_contract_sig f contract_sig)


  and map_contract_sig (f : mapper) (contract_sig : contract_sig) =
    let map_type typ' = map_type_expression f typ' in
    { storage = map_type contract_sig.storage
    ; parameter = map_type contract_sig.parameter
    }


  and map_signature (f : mapper) (sig_ : signature) : signature =
    { sig_items = List.map ~f:(map_signature_item f) sig_.sig_items
    ; sig_sort = map_signature_sort f sig_.sig_sort
    }


  and map_program (f : mapper) (prog : program) : program =
    { pr_module = map_module f prog.pr_module; pr_sig = map_signature f prog.pr_sig }
end
