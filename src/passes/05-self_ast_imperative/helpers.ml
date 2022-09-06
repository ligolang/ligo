module Pair = Simple_utils.Pair
module Var  = Simple_utils.Var
open Ast_imperative
open Ligo_prim

type 'a folder = 'a -> expression -> 'a
let rec fold_expression : 'a folder -> 'a -> expression -> 'a = fun f init e ->
  let self = fold_expression f in
  let self_type = Fun.const in
  let init = f init e in
  match e.expression_content with
  | E_literal _ | E_variable _ | E_raw_code _ | E_skip _ | E_module_accessor _ -> init
  | E_list lst | E_set lst | E_constant {arguments=lst;cons_name=_} -> (
    let res = List.fold ~f:self ~init lst in
    res
  )
  | E_map lst | E_big_map lst -> (
    let res = List.fold ~f:(fun init -> Pair.fold ~f:self ~init) ~init lst in
    res
  )
  | E_application app -> Application.fold self init app
  | E_lambda l -> Lambda.fold self (fun a _ -> a) init l
  | E_type_abstraction ta -> Type_abs.fold self init ta
  | E_ascription a -> Ascription.fold self (fun a _ -> a) init a
  | E_constructor c -> Constructor.fold self init c
  | E_matching  m -> Match_expr.fold self self_type init m
  | E_record m -> List.fold ~f:(fun acc (_,e) -> self acc e) ~init m
  | E_update    u -> Types.Update.fold self init u
  | E_accessor  a -> Types.Accessor.fold self init a
  | E_tuple     t -> List.fold ~f:self ~init t
  | E_let_in   li -> Let_in.fold self self_type init li
  | E_type_in  ti -> Type_in.fold self self_type init ti
  | E_mod_in   mi -> Mod_in.fold  self self_type init mi
  | E_cond      c -> Conditional.fold self init c
  | E_recursive r -> Recursive.fold self self_type init r
  | E_sequence  s -> Sequence.fold self init s
  | E_assign    a -> Assign.fold self self_type init a
  | E_for       f -> For_loop.fold self init f
  | E_for_each fe -> For_each_loop.fold self init fe
  | E_while     w -> While_loop.fold self init w

type exp_mapper = expression -> expression
type ty_exp_mapper = type_expression -> type_expression
type mod_mapper = module_ -> module_
type program_mapper = program -> program
type abs_mapper =
  | Expression of exp_mapper
  | Type_expression of ty_exp_mapper
  | Module of mod_mapper
  | Program of program_mapper

let rec map_expression : exp_mapper -> expression -> expression = fun f e ->
  let self = map_expression f in
  let self_type = Fun.id in
  let e' = f e in
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_list lst -> (
    let lst' = List.map ~f:self lst in
    return @@ E_list lst'
  )
  | E_set lst -> (
    let lst' = List.map ~f:self lst in
    return @@ E_set lst'
  )
  | E_map lst -> (
    let lst' = List.map ~f:(Pair.map ~f:self) lst in
    return @@ E_map lst'
  )
  | E_big_map lst -> (
    let lst' = List.map ~f:(Pair.map ~f:self) lst in
    return @@ E_big_map lst'
  )
  | E_ascription ascr -> (
      let ascr = Ascription.map self (fun a -> a) ascr in
      return @@ E_ascription ascr
    )
  | E_matching match_expr ->
    let match_expr = Match_expr.map self (fun a -> a) match_expr in
    return @@ E_matching match_expr
  | E_record m -> (
    let m' = List.map ~f:(fun (k,v) -> (k,self v)) m in
    return @@ E_record m'
  )
  | E_accessor acc -> (
      let acc = Types.Accessor.map self acc in
      return @@ E_accessor acc
    )
  | E_update u -> (
    let u = Types.Update.map self u in
    return @@ E_update u
  )
  | E_tuple t -> (
    let t' = List.map ~f:self t in
    return @@ E_tuple t'
  )
  | E_constructor c -> (
      let c = Constructor.map self c in
      return @@ E_constructor c
  )
  | E_application app -> (
    let app = Application.map self app in
    return @@ E_application app
  )
  | E_let_in li -> (
      let li = Let_in.map self (fun a -> a) li in
      return @@ E_let_in li
    )
  | E_type_in ti -> (
      let ti = Type_in.map self (fun a -> a) ti in
      return @@ E_type_in ti
    )
  | E_mod_in mi -> (
      let mi = Mod_in.map self self_type mi in
      return @@ E_mod_in mi
    )
  | E_lambda l -> (
      let l = Lambda.map self self_type l in
      return @@ E_lambda l
    )
  | E_type_abstraction ta -> (
      let ta = Type_abs.map self ta in
      return @@ E_type_abstraction ta
    )
  | E_recursive r ->
      let r = Recursive.map self self_type r in
      return @@ E_recursive r
  | E_constant c -> (
      let args = List.map ~f:self c.arguments in
      return @@ E_constant {c with arguments=args}
    )
  | E_cond c ->
      let c = Conditional.map self c in
      return @@ E_cond c
  | E_sequence s -> (
      let s = Sequence.map self s in
      return @@ E_sequence s
    )
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
  | E_literal _ | E_variable _ | E_raw_code _ | E_skip _ | E_module_accessor _ as e' -> return e'

and map_type_expression : ty_exp_mapper -> type_expression -> type_expression = fun f te ->
  let self = map_type_expression f in
  let te' = f te in
  let return type_content = { type_content; location=te.location } in
  match te'.type_content with
  | T_sum temap ->
    let fields = List.map ~f:(fun (k,v) -> (k, Rows.map_row_element self v)) temap.fields in
    return @@ T_sum {temap with fields}
  | T_record temap ->
    let fields = List.map ~f:(fun (k,v) -> (k, Rows.map_row_element self v)) temap.fields in
    return @@ T_record {temap with fields}
  | T_tuple telst ->
    let telst' = List.map ~f:self telst in
    return @@ (T_tuple telst')
  | T_arrow arr ->
    let arr = Arrow.map self arr in
    return @@ T_arrow arr
  | T_annoted (ty, str) ->
    let ty = self ty in
    return @@ T_annoted (ty, str)
  | T_app {type_operator;arguments} ->
    let arguments = List.map ~f:self arguments in
    return @@ T_app {type_operator;arguments}
  | T_variable _ -> te'
  | T_module_accessor _ -> te'
  | T_singleton _ -> te'
  | T_abstraction x ->
    let x = Abstraction.map self x in
    return @@ T_abstraction x
  | T_for_all x ->
    let x = Abstraction.map self x in
    return @@ T_for_all x

and map_module_expr : mod_mapper -> module_expr -> module_expr = fun f m ->
  let return wrap_content : module_expr = { m with wrap_content } in
  match m.wrap_content with
  | M_struct decls ->
    let decls = f decls in
    return (M_struct decls)
  | M_module_path _ -> m
  | M_variable _ -> m

and map_module : abs_mapper -> module_ -> module_ = fun m p ->
  let p = match m with
    | Module m' -> m' p
    | _ -> p
  in
  List.map ~f:(map_decl m) p

and map_declaration_content = fun (m: abs_mapper) (x : declaration_content) : declaration_content ->
  match x,m with
  | D_value dc, Expression m' -> (
      let dc = Types.ValueDecl.map (map_expression m') (fun a -> a) dc in
      D_value dc
    )
  | D_type dt, Type_expression m' -> (
      let dt = Types.TypeDecl.map (map_type_expression m') dt in
      D_type dt
    )
  | D_module dm, Module m' -> (
    let module_ = map_module_expr m' dm.module_ in
    let dm = { dm with module_ } in
    D_module dm
  )
  | D_module dm, Expression _ -> (
      let dm = Types.ModuleDecl.map (Location.map @@ Module_expr.map (map_decl m)) dm in
      D_module dm
    )
  | decl,_ -> decl

and map_decl m = Location.map (map_declaration_content m)

let map_program m (p : program) =
  let p = match m with
    | Program m' -> m' p
    | _ -> p
  in
  List.map ~f:(Location.map (map_declaration_content m)) p

type ('a, 'err) fold_mapper = 'a -> expression -> (bool * 'a * expression)
let rec fold_map_expression : ('a, 'err) fold_mapper -> 'a -> expression -> 'a * expression = fun f a e ->
  let self = fold_map_expression f in
  let idle acc a = (acc,a) in
  let (continue, init,e') = f a e in
  if (not continue) then (init,e')
  else
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_list lst -> (
    let (res, lst') = List.fold_map ~f:self ~init lst in
    (res, return @@ E_list lst')
  )
  | E_set lst -> (
    let (res, lst') = List.fold_map ~f:self ~init lst in
    (res, return @@ E_set lst')
  )
  | E_map lst -> (
    let (res, lst') = List.fold_map ~f:(fun init -> Pair.fold_map ~f:self ~init) ~init lst in
    (res, return @@ E_map lst')
  )
  | E_big_map lst -> (
    let (res, lst') = List.fold_map ~f:(fun init -> Pair.fold_map ~f:self ~init) ~init lst in
    (res, return @@ E_big_map lst')
  )
  | E_ascription ascr -> (
      let (res,ascr) = Ascription.fold_map self idle init ascr in
      (res, return @@ E_ascription ascr)
    )
  | E_matching m ->
    let res,m = Match_expr.fold_map self idle init m in
    (res, return @@ E_matching m)
  | E_record r -> (
    let (res, r) = List.fold_map ~f:(fun res (k,v) -> let res,v = self res v in res, (k,v)) ~init r in
    (res, return @@ E_record r)
  )
  | E_accessor acc -> (
      let (res, acc) = Types.Accessor.fold_map self init acc in
      (res, return @@ E_accessor acc)
    )
  | E_update u -> (
    let res,u = Types.Update.fold_map self init u in
    (res, return @@ E_update u)
  )
  | E_tuple t -> (
    let (res, t') = List.fold_map ~f:self ~init:init t in
    (res, return @@ E_tuple t')
  )
  | E_constructor c -> (
      let (res,c) = Constructor.fold_map self init c in
      (res, return @@ E_constructor c)
  )
  | E_application app -> (
      let res,app = Application.fold_map self init app in
      (res, return @@ E_application app)
    )
  | E_let_in li -> (
      let res,li = Let_in.fold_map self idle init li in
      (res, return @@ E_let_in li)
    )
  | E_type_in ti -> (
      let res,ti = Type_in.fold_map self idle init ti in
      (res, return @@ E_type_in ti)
    )
  | E_mod_in mi -> (
      let res,mi = Mod_in.fold_map self idle init mi in
      (res, return @@ E_mod_in mi)
    )
  | E_lambda l -> (
      let res,l = Lambda.fold_map self idle init l in
      ( res, return @@ E_lambda l)
    )
  | E_type_abstraction ta -> (
      let res, ta = Type_abs.fold_map self init ta in
      res, return @@ E_type_abstraction ta
    )
  | E_recursive r ->
      let res,r = Recursive.fold_map self idle init r in
      ( res, return @@ E_recursive r)
  | E_constant c -> (
      let (res,args) = List.fold_map ~f:self ~init c.arguments in
      (res, return @@ E_constant {c with arguments=args})
    )
  | E_cond c ->
      let res,c = Conditional.fold_map self init c in
      (res, return @@ E_cond c)
  | E_sequence s -> (
      let res,s = Sequence.fold_map self init s in
      (res, return @@ E_sequence s)
    )
  | E_assign a ->
    let (res,a) = Assign.fold_map self idle init a in
    (res, return @@ E_assign a)
  | E_for f ->
      let res,f = For_loop.fold_map self init f in
      (res, return @@ E_for f)
  | E_for_each fe ->
      let res,fe = For_each_loop.fold_map self init fe in
      (res, return @@ E_for_each fe)
  | E_while w ->
      let res,w = While_loop.fold_map self init w in
      (res, return @@ E_while w)
  | E_literal _ | E_variable _ | E_raw_code _ | E_skip _ | E_module_accessor _ as e' -> (init, return e')

let remove_from var vars =
  let f v vars = if ValueVar.equal var v then vars else v :: vars in
  List.fold_right ~f vars ~init:[]

let get_pattern ?(pred = fun _ -> true) (pattern : type_expression option Pattern.t) =
  Pattern.fold_pattern (fun vars p ->
      match p.wrap_content with
      | Pattern.P_var {var;attributes;_} when pred attributes ->
         var :: vars
      | _ -> vars) [] pattern

module Free_variables :
  sig
    val expression : expression -> ValueVar.t list
  end
  = struct

  module VarSet = Caml.Set.Make(ValueVar)

  let unions : VarSet.t list -> VarSet.t =
    fun l -> List.fold l ~init:VarSet.empty ~f:VarSet.union

  let rec get_fv_expr : expression -> VarSet.t = fun e ->
    let self = get_fv_expr in
    match e.expression_content with
    | E_variable v ->
       VarSet.singleton v
    | E_literal _ | E_raw_code _ | E_skip _ | E_module_accessor _ ->
       VarSet.empty
    | E_list lst ->
      unions @@ List.map ~f:self lst
    | E_set lst ->
      unions @@ List.map ~f:self lst
    | E_map lst ->
      unions @@ List.map ~f:(fun (l, r) -> VarSet.union (self l) (self r)) lst
    | E_big_map lst ->
      unions @@ List.map ~f:(fun (l, r) -> VarSet.union (self l) (self r)) lst
    | E_ascription {anno_expr;_} ->
      self anno_expr
    | E_matching {matchee;cases} ->
      let aux ({ pattern ; body } : _ Match_expr.match_case) =
        let pattern = get_pattern pattern in
        List.fold_right pattern ~f:VarSet.remove ~init:(self body)
      in
      VarSet.union (self matchee) (unions @@ List.map ~f:aux cases)
    | E_record m ->
      let res = List.map ~f:(fun (_,v) -> self v) m in
      unions res
    | E_accessor {record;path} ->
      let aux = function
        | Access_path.Access_tuple _ | Access_record _ -> VarSet.empty
        | Access_map e -> self e in
      VarSet.union (self record) (unions @@ List.map ~f:aux path)
    | E_update {record;path;update} ->
      let aux = function
        | Access_path.Access_tuple _ | Access_record _ -> VarSet.empty
        | Access_map e -> self e in
      unions ([self record; self update] @ List.map ~f:aux path)
    | E_tuple t ->
      unions @@ List.map ~f:self t
    | E_constructor {element;_} ->
      self element
    | E_application {lamb; args} ->
      VarSet.union (self lamb) (self args)
    | E_let_in {let_binder = {var;ascr=_;attributes=_}; rhs; let_result;_} ->
      VarSet.union (self rhs) (VarSet.remove var (self let_result))
    | E_type_in {let_result;type_binder=_;rhs=_} ->
      self let_result
    | E_mod_in {rhs;let_result;module_binder=_} ->
      VarSet.union (get_fv_module_expr rhs.wrap_content) (self let_result)
    | E_lambda {binder = {var;ascr=_;attributes=_}; result;output_type=_} ->
      VarSet.remove var @@ self result
    | E_type_abstraction {type_binder=_;result} ->
      self result
    | E_recursive {fun_name; lambda = {binder = {var;ascr=_;attributes=_}; result;_};fun_type=_} ->
      VarSet.remove fun_name @@ VarSet.remove var @@ self result
    | E_constant {arguments;cons_name=_} ->
      unions @@ List.map ~f:self arguments
    | E_cond {condition; then_clause; else_clause} ->
      unions @@ [self condition; self then_clause; self else_clause]
    | E_sequence {expr1; expr2} ->
      VarSet.union (self expr1) (self expr2)
    | E_assign {binder; expression} ->
      unions @@ [VarSet.singleton binder.var; self expression]
    | E_for {binder; start; final; incr; f_body} ->
      VarSet.remove binder @@ unions [self start; self final; self incr; self f_body]
    | E_for_each {fe_binder = (binder, None); collection; fe_body; collection_type = _} ->
      unions [self collection; VarSet.remove binder @@ self fe_body]
    | E_for_each {fe_binder = (binder, Some binder'); collection; fe_body;_} ->
      unions [self collection; VarSet.remove binder @@ VarSet.remove binder' @@ self fe_body]
    | E_while {cond; body} ->
      unions [self cond; self body]

  and get_fv_module_expr : module_expr_content -> VarSet.t = function
    | M_struct prg -> get_fv_module prg
    | M_variable _ -> VarSet.empty
    | M_module_path _ -> VarSet.empty

  and get_fv_module : module_ -> VarSet.t = fun p ->
    let aux = fun (x : decl) ->
      match Location.unwrap x with
      | D_value {binder=_; expr;attr=_} ->
        get_fv_expr expr
      | D_type _t -> VarSet.empty
      | D_module {module_binder=_;module_;module_attr=_} ->
        get_fv_module_expr module_.wrap_content
    in
    unions @@ List.map ~f:aux p

  let expression e = VarSet.fold (fun v r -> v :: r) (get_fv_expr e) []
end
