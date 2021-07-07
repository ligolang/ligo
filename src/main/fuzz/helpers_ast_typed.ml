include Fuzz_shared.Monad
open Ast_typed

module Fold_helpers(M : Monad) = struct
  open Monad_context(M)

  let bind_lmap (l:_ label_map) =
    let open LMap in
    let aux k v prev =
      let* prev' = prev in
      let* v' = v in
      return @@ add k v' prev' in
    fold aux l (return empty)

  let bind_map_lmap f map = bind_lmap (LMap.map f map)

  type 'a monad = 'a t
  let ok x = return x


let type_in :  ('a -> 'b monad) -> ('c -> 'd monad) -> ('a, 'c) type_in -> (('b,'d) type_in) monad
= fun f g {type_binder; rhs; let_result} ->
  let* rhs        = g rhs in
  let* let_result = f let_result in
  ok @@ {type_binder; rhs; let_result}

type 'err mapper = expression -> (bool * expression) monad
let rec map_expression : 'err mapper -> expression -> expression monad = fun f e ->
  let* b, e' = f e in
  let self = if b then map_expression f else return in
  let return expression_content = ok { e' with expression_content } in
  match e'.expression_content with
  | E_matching {matchee=e;cases} -> (
    let* e' = self e in
    let* cases' = map_cases f cases in
    return @@ E_matching {matchee=e';cases=cases'}
  )
  | E_record_accessor {record; path} -> (
    let* record = self record in
    return @@ E_record_accessor {record; path}
  )
  | E_record m -> (
    let* m' = bind_map_lmap self m in
    return @@ E_record m'
  )
  | E_record_update {record; path; update} -> (
    let* record = self record in
    let* update = self update in
    return @@ E_record_update {record;path;update}
  )
  | E_constructor c -> (
    let* e' = self c.element in
    return @@ E_constructor {c with element = e'}
  )
  | E_application {lamb; args} -> (
    let ab = (lamb, args) in
    let* (a,b) = bind_map_pair self ab in
    return @@ E_application {lamb=a;args=b}
  )
  | E_let_in { let_binder ; rhs ; let_result; inline } -> (
    let* rhs = self rhs in
    let* let_result = self let_result in
    return @@ E_let_in { let_binder ; rhs ; let_result; inline }
  )
  | E_type_in ti -> (
    let* ti = type_in self ok ti in
    return @@ E_type_in ti
  )
  | E_mod_in { module_binder ; rhs ; let_result } -> (
    let* rhs = map_module f rhs in
    let* let_result = self let_result in
    return @@ E_mod_in { module_binder ; rhs ; let_result }
  )
  | E_mod_alias { alias ; binders ; result } -> (
    let* result = self result in
    return @@ E_mod_alias { alias ; binders ; result }
  )
  | E_lambda { binder ; result } -> (
    let* result = self result in
    return @@ E_lambda { binder ; result }
  )
  | E_recursive { fun_name; fun_type; lambda = {binder;result}} -> (
    let* result = self result in
    return @@ E_recursive { fun_name; fun_type; lambda = {binder;result}}
  )
  | E_constant c -> (
    let* args = bind_map_list self c.arguments in
    return @@ E_constant {c with arguments=args}
  )
  | E_module_accessor { module_name; element } -> (
    let* element = self element in
    return @@ E_module_accessor { module_name; element }
  )
  | E_literal _ | E_variable _ | E_raw_code _ as e' -> return e'


and map_cases : 'err mapper -> matching_expr -> matching_expr monad = fun f m ->
  match m with
  | Match_variant {cases;tv} -> (
      let aux { constructor ; pattern ; body } =
        let* body = map_expression f body in
        ok {constructor;pattern;body}
      in
      let* cases = bind_map_list aux cases in
      ok @@ Match_variant {cases ; tv}
    )
  | Match_record {fields; body; tv} ->
    let* body = map_expression f body in
    ok @@ Match_record {fields; body; tv}

and map_module : 'err mapper -> module_fully_typed -> module_fully_typed monad = fun m (Module_Fully_Typed p) ->
  let aux = fun (x : declaration) ->
    let return (d : declaration) = ok @@ d in
    match x with
    | Declaration_constant {name; binder; expr ; inline} -> (
        let* expr = map_expression m expr in
        return @@ Declaration_constant {name; binder; expr ; inline}
    )
    | Declaration_type t -> return @@ Declaration_type t
    | Declaration_module {module_binder;module_} ->
      let* module_ = map_module m module_ in
      return @@ Declaration_module {module_binder; module_}
    | Module_alias _ -> return x
  in
  let* p = bind_map_list (bind_map_location aux) p in
  ok @@ Module_Fully_Typed p

type 'err traverser = {
  literal: literal -> literal monad;
  constant: constant -> constant monad;
}

let rec traverse_expression : 'err traverser -> expression -> expression monad = fun f e ->
  let self = traverse_expression f in
  let e' = e in
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_matching {matchee=e;cases} -> (
    let+ e' = self e
    and+ cases' = traverse_cases f cases in
    return @@ E_matching {matchee=e';cases=cases'}
  )
  | E_record_accessor {record; path} -> (
    let+ record = self record in
    return @@ E_record_accessor {record; path}
  )
  | E_record m -> (
    let+ m' = bind_map_lmap self m in
    return @@ E_record m'
  )
  | E_record_update {record; path; update} -> (
    let+ record = self record
    and+ update = self update in
    return @@ E_record_update {record;path;update}
  )
  | E_constructor c -> (
    let+ e' = self c.element in
    return @@ E_constructor {c with element = e'}
  )
  | E_application {lamb; args} -> (
    let ab = (lamb, args) in
    let+ (a,b) = bind_map_pair self ab in
    return @@ E_application {lamb=a;args=b}
  )
  | E_let_in { let_binder ; rhs ; let_result; inline } -> (
    let+ rhs = self rhs
    and+ let_result = self let_result in
    return @@ E_let_in { let_binder ; rhs ; let_result; inline }
  )
  | E_type_in ti -> (
    let+ ti = type_in self ok ti in
    return @@ E_type_in ti
  )
  | E_mod_in { module_binder ; rhs ; let_result } -> (
    let+ rhs = traverse_module f rhs
    and+ let_result = self let_result in
    return @@ E_mod_in { module_binder ; rhs ; let_result }
  )
  | E_mod_alias { alias ; binders ; result } -> (
    let+ result = self result in
    return @@ E_mod_alias { alias ; binders ; result }
  )
  | E_lambda { binder ; result } -> (
    let+ result = self result in
    return @@ E_lambda { binder ; result }
  )
  | E_recursive { fun_name; fun_type; lambda = {binder;result}} -> (
    let+ result = self result in
    return @@ E_recursive { fun_name; fun_type; lambda = {binder;result}}
  )
  | E_constant c -> (
    let+ c = f.constant c in
    return @@ E_constant {c with arguments=c.arguments}
  )
  | E_module_accessor { module_name; element } -> (
    let+ element = self element in
    return @@ E_module_accessor { module_name; element }
  )
  | E_literal l -> (
    let+ l = f.literal l in
    return @@ E_literal l
  )
  | E_variable _ | E_raw_code _ as e' -> ok @@ (return e')


and traverse_cases : 'err traverser -> matching_expr -> matching_expr monad = fun f m ->
  match m with
  | Match_variant {cases;tv} -> (
      let aux { constructor ; pattern ; body } =
        let* body = traverse_expression f body in
        ok {constructor;pattern;body}
      in
      let* cases = bind_map_list aux cases in
      ok @@ Match_variant {cases ; tv}
    )
  | Match_record {fields; body; tv} ->
    let* body = traverse_expression f body in
    ok @@ Match_record {fields; body; tv}

and traverse_module : 'err traverser -> module_fully_typed -> module_fully_typed monad = fun m (Module_Fully_Typed p) ->
  let aux = fun (x : declaration) ->
    let return (d : declaration) = ok @@ d in
    match x with
    | Declaration_constant {name; binder; expr ; inline} -> (
        let* expr = traverse_expression m expr in
        return @@ Declaration_constant {name; binder; expr ; inline}
    )
    | Declaration_type t -> return @@ Declaration_type t
    | Declaration_module {module_binder;module_} ->
      let* module_ = traverse_module m module_ in
      return @@ Declaration_module {module_binder; module_}
    | Module_alias _ -> return x
  in
  let* p = bind_map_list (bind_map_location aux) p in
  ok @@ Module_Fully_Typed p

end
