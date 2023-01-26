include Fuzz_shared.Monad
open Ligo_prim
open Ast_imperative

module Fold_helpers (M : Monad) = struct
  open Monad_context (M)

  type 'a monad = 'a t

  let rec all ts =
    match ts with
    | [] -> return []
    | t :: ts ->
      let* x = t in
      let* xs = all ts in
      return (x :: xs)


  let all_opt t =
    match t with
    | None -> return None
    | Some t ->
      let* x = t in
      return (Some x)


  let ok x = return x

  let constructor : ('a -> 'b monad) -> 'a Constructor.t -> 'b Constructor.t monad =
   fun f { constructor; element } ->
    let* element = f element in
    ok @@ Constructor.{ constructor; element }


  let application : ('a -> 'b monad) -> 'a Application.t -> 'b Application.t monad =
   fun f { lamb; args } ->
    let* lamb = f lamb in
    let* args = f args in
    ok @@ Application.{ lamb; args }


  and binder : ('a -> 'b monad) -> 'a Binder.t -> 'b Binder.t monad =
   fun f binder ->
    let ascr = Binder.get_ascr binder in
    let* ascr = f ascr in
    let binder = Binder.map (Fn.const ascr) binder in
    ok @@ binder


  let rec pattern : ('a -> 'b monad) -> 'a Pattern.t -> 'b Pattern.t monad =
   fun f p ->
    let loc = p.location in
    match p.wrap_content with
    | P_unit -> ok @@ p
    | P_var b ->
      let* b = binder f b in
      ok @@ Location.wrap ~loc (Pattern.P_var b)
    | P_list (Cons (h, t)) ->
      let* h = pattern f h in
      let* t = pattern f t in
      ok @@ Location.wrap ~loc (Pattern.P_list (Cons (h, t)))
    | P_list (List ps) ->
      let* ps = pattern_list f ps in
      ok @@ Location.wrap ~loc (Pattern.P_list (List ps))
    | P_variant (c, p) ->
      let* p = pattern f p in
      ok @@ Location.wrap ~loc (Pattern.P_variant (c, p))
    | P_tuple ps ->
      let* ps = pattern_list f ps in
      ok @@ Location.wrap ~loc (Pattern.P_tuple ps)
    | P_record lps ->
      let ls, ps = List.unzip lps in
      let* ps = pattern_list f ps in
      let lps = List.zip_exn ls ps in
      ok @@ Location.wrap ~loc (Pattern.P_record lps)


  and pattern_list : ('a -> 'b monad) -> 'a Pattern.t list -> 'b Pattern.t list monad =
   fun f ps ->
    match ps with
    | [] -> ok @@ ps
    | p :: ps ->
      let* ps = pattern_list f ps in
      let* p = pattern f p in
      ok @@ (p :: ps)


  and param : ('a -> 'b monad) -> 'a Param.t -> 'b Param.t monad =
   fun f param ->
    let ascr = Param.get_ascr param in
    let* ascr = f ascr in
    let binder = Param.map (Fn.const ascr) param in
    ok @@ binder


  let let_in
      :  ('a -> 'b monad) -> ('c -> 'd monad) -> ('a, 'c) Let_in.t
      -> ('b, 'd) Let_in.t monad
    =
   fun f g { let_binder; rhs; let_result; attributes } ->
    let* let_binder = pattern g let_binder in
    let* rhs = f rhs in
    let* let_result = f let_result in
    ok @@ Let_in.{ let_binder; rhs; let_result; attributes }


  let type_in
      :  ('a -> 'b monad) -> ('c -> 'd monad) -> ('a, 'c) Type_in.t
      -> ('b, 'd) Type_in.t monad
    =
   fun f g { type_binder; rhs; let_result } ->
    let* rhs = g rhs in
    let* let_result = f let_result in
    ok @@ Type_in.{ type_binder; rhs; let_result }


  let lambda
      :  ('a -> 'b monad) -> ('c -> 'd monad) -> ('a, 'c) Lambda.t
      -> ('b, 'd) Lambda.t monad
    =
   fun f g { binder = b; output_type; result } ->
    let* binder = param g b in
    let* output_type = g output_type in
    let* result = f result in
    ok @@ Lambda.{ binder; output_type; result }


  let type_abs : ('a -> 'b monad) -> 'a Type_abs.t -> 'b Type_abs.t monad =
   fun f { type_binder; result } ->
    let* result = f result in
    ok @@ Type_abs.{ type_binder; result }


  let path : ('a -> 'b monad) -> 'a Access_path.t -> 'b Access_path.t monad =
   fun f path ->
    let open Access_path in
    let aux a =
      match a with
      | Access_record s -> ok @@ Access_record s
      | Access_tuple i -> ok @@ Access_tuple i
      | Access_map e ->
        let* e = f e in
        ok @@ Access_map e
    in
    bind_map_list aux path


  let recursive
      :  ('a -> 'b monad) -> ('c -> 'd monad) -> ('a, 'c) Recursive.t
      -> ('b, 'd) Recursive.t monad
    =
   fun f g { fun_name; fun_type; lambda = l } ->
    let* fun_type = g fun_type in
    let* lambda = lambda f g l in
    ok @@ Recursive.{ fun_name; fun_type; lambda }


  let accessor : ('a -> 'b monad) -> 'a Accessor.t -> 'b Accessor.t monad =
   fun f { struct_; path = p } ->
    let* struct_ = f struct_ in
    let* path = path f p in
    ok @@ ({ struct_; path } : 'b Accessor.t)


  let update : ('a -> 'b monad) -> 'a Update.t -> 'b Update.t monad =
   fun f { struct_; path = p; update } ->
    let* struct_ = f struct_ in
    let* path = path f p in
    let* update = f update in
    ok @@ ({ struct_; path; update } : 'b Update.t)


  let sequence : ('a -> 'b monad) -> 'a Sequence.t -> 'b Sequence.t monad =
   fun f { expr1; expr2 } ->
    let* expr1 = f expr1 in
    let* expr2 = f expr2 in
    ok @@ Sequence.{ expr1; expr2 }


  let originate : ('a -> 'b monad) -> 'a Originate.t -> 'b Originate.t monad =
   fun f { contract; storage; key_hash; tez } ->
    let* storage = f storage in
    let* key_hash = f key_hash in
    let* tez = f tez in
    ok @@ Originate.{ contract; storage; key_hash; tez }


  let contract_call =
    let open Contract_call in
    fun f { contract; address; method_; params; on_none } ->
      let* address = f address in
      let* params = List.map ~f params |> all in
      let* on_none = Option.map ~f on_none |> all_opt in
      ok @@ { contract; address; method_; params; on_none }


  let ascription
      :  ('a -> 'b monad) -> ('c -> 'd monad) -> ('a, 'c) Ascription.t
      -> ('b, 'd) Ascription.t monad
    =
   fun f g { anno_expr; type_annotation } ->
    let* anno_expr = f anno_expr in
    let* type_annotation = g type_annotation in
    ok @@ Ascription.{ anno_expr; type_annotation }


  let conditional : ('a -> 'b monad) -> 'a Conditional.t -> 'b Conditional.t monad =
   fun f { condition; then_clause; else_clause } ->
    let* condition = f condition in
    let* then_clause = f then_clause in
    let* else_clause = f else_clause in
    ok @@ Conditional.{ condition; then_clause; else_clause }


  let assign
      :  ('a -> 'b monad) -> ('c -> 'd monad) -> ('a, 'c) Assign.t
      -> ('b, 'd) Assign.t monad
    =
   fun f g { binder = b; expression } ->
    let* binder = binder g b in
    let* expression = f expression in
    ok @@ Assign.{ binder; expression }


  let for_ : ('a -> 'b monad) -> 'a For_loop.t -> 'b For_loop.t monad =
   fun f { binder; start; final; incr; f_body } ->
    let* f_body = f f_body in
    ok @@ For_loop.{ binder; start; final; incr; f_body }


  let for_each : ('a -> 'b monad) -> 'a For_each_loop.t -> 'b For_each_loop.t monad =
   fun f { fe_binder; collection; fe_body; collection_type } ->
    let* collection = f collection in
    let* fe_body = f fe_body in
    ok @@ For_each_loop.{ fe_binder; collection; fe_body; collection_type }


  let while_loop : ('a -> 'b monad) -> 'a While_loop.t -> 'b While_loop.t monad =
   fun f { cond; body } ->
    let* cond = f cond in
    let* body = f body in
    ok @@ While_loop.{ cond; body }


  (* Declaration *)
  let declaration_type : ('a -> 'b monad) -> 'a Type_decl.t -> 'b Type_decl.t monad =
   fun g { type_binder; type_expr; type_attr } ->
    let* type_expr = g type_expr in
    ok @@ Type_decl.{ type_binder; type_expr; type_attr }


  let declaration_constant
      :  ('a -> 'b monad) -> ('c -> 'd monad) -> ('a, 'c) Value_decl.t
      -> ('b, 'd) Value_decl.t monad
    =
   fun f g { binder = b; attr; expr } ->
    let* binder = binder g b in
    let* expr = f expr in
    ok @@ Value_decl.{ binder; attr; expr }


  let rec declaration_module
      : ('a -> 'b monad) -> 'a Module_decl.t -> 'b Module_decl.t monad
    =
   fun f { module_binder; module_; module_attr } ->
    let* module_ = f module_ in
    ok @@ Module_decl.{ module_binder; module_; module_attr }


  and module' : _ -> module_ -> module_ monad = fun f prg -> bind_map_list f prg

  and module_expr : (decl -> decl monad) -> module_expr -> module_expr monad =
   fun f mexp ->
    let open Module_expr in
    bind_map_location
      (function
        | M_struct prg ->
          let* prg = module' f prg in
          ok (M_struct prg)
        | M_variable x -> ok (M_variable x)
        | M_module_path path -> ok (M_module_path path))
      mexp


  let mod_in
      :  ('a -> 'b monad) -> ('c -> 'd monad) -> ('a, 'c) Mod_in.t
      -> ('b, 'd) Mod_in.t monad
    =
   fun f g { module_binder; rhs; let_result } ->
    let* rhs = g rhs in
    let* let_result = f let_result in
    ok @@ Mod_in.{ module_binder; rhs; let_result }


  type 'err exp_mapper = expression -> expression monad
  type 'err ty_exp_mapper = type_expression -> type_expression monad
  type 'err abs_mapper = Expression of 'err exp_mapper

  let rec map_expression : 'err exp_mapper -> expression -> expression monad =
   fun f e ->
    let self = map_expression f in
    let* e' = f e in
    let return expression_content = ok { e' with expression_content } in
    match e'.expression_content with
    | E_list lst ->
      let* lst' = bind_map_list self lst in
      return @@ E_list lst'
    | E_set lst ->
      let* lst' = bind_map_list self lst in
      return @@ E_set lst'
    | E_map lst ->
      let* lst' = bind_map_list (bind_map_pair self) lst in
      return @@ E_map lst'
    | E_big_map lst ->
      let* lst' = bind_map_list (bind_map_pair self) lst in
      return @@ E_big_map lst'
    | E_ascription ascr ->
      let* ascr = ascription self ok ascr in
      return @@ E_ascription ascr
    | E_matching { matchee = e; cases } ->
      let* e' = self e in
      let aux Match_expr.{ pattern; body } =
        let* body' = self body in
        ok @@ Match_expr.{ pattern; body = body' }
      in
      let* cases' = bind_map_list aux cases in
      return @@ E_matching { matchee = e'; cases = cases' }
    | E_record m ->
      let* m' =
        bind_map_list
          (fun (l, e) ->
            let* e = self e in
            ok (l, e))
          m
      in
      return @@ E_record m'
    | E_accessor acc ->
      let* acc = accessor self acc in
      return @@ E_accessor acc
    | E_update u ->
      let* u = update self u in
      return @@ E_update u
    | E_tuple t ->
      let* t' = bind_map_list self t in
      return @@ E_tuple t'
    | E_constructor c ->
      let* c = constructor self c in
      return @@ E_constructor c
    | E_application app ->
      let* app = application self app in
      return @@ E_application app
    | E_let_in li ->
      let* li = let_in self ok li in
      return @@ E_let_in li
    | E_type_in ti ->
      let* ti = type_in self ok ti in
      return @@ E_type_in ti
    | E_mod_in mi ->
      let* mi = mod_in self ok mi in
      return @@ E_mod_in mi
    | E_lambda l ->
      let* l = lambda self ok l in
      return @@ E_lambda l
    | E_type_abstraction ta ->
      let* ta = type_abs self ta in
      return @@ E_type_abstraction ta
    | E_recursive r ->
      let* r = recursive self ok r in
      return @@ E_recursive r
    | E_constant c ->
      let* args = bind_map_list self c.arguments in
      return @@ E_constant { c with arguments = args }
    | E_cond c ->
      let* c = conditional self c in
      return @@ E_cond c
    | E_sequence s ->
      let* s = sequence self s in
      return @@ E_sequence s
    | E_assign a ->
      let* a = assign self ok a in
      return @@ E_assign a
    | E_for f ->
      let* f = for_ self f in
      return @@ E_for f
    | E_for_each fe ->
      let* fe = for_each self fe in
      return @@ E_for_each fe
    | E_while w ->
      let* w = while_loop self w in
      return @@ E_while w
    | E_let_mut_in li ->
      let* li = let_in self ok li in
      return @@ E_let_in li
    | E_originate orig ->
      let* orig = originate self orig in
      return @@ E_originate orig
    | E_contract_call call ->
      let* call = contract_call self call in
      return @@ E_contract_call call
    | (E_literal _ | E_variable _ | E_raw_code _ | E_skip _ | E_module_accessor _) as e'
      -> return e'


  and declaration m : declaration -> declaration monad =
   fun d ->
    match d.wrap_content, m with
    | D_value dc, Expression m' ->
      let* dc = declaration_constant (map_expression m') ok dc in
      ok { d with wrap_content = D_value dc }
    | _, _ -> ok @@ d


  and decl m (d : decl) : decl monad = declaration m d

  and map_module : 'err abs_mapper -> module_ -> module_ monad =
   fun m -> bind_map_list (decl m)


  let map_program : 'err abs_mapper -> program -> program monad =
   fun m -> bind_map_list (declaration m)
end
