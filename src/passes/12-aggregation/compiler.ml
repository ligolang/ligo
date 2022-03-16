module I = Ast_typed
module O = Ast_aggregated
open Stage_common.Types
open Trace
type err = Errors.aggregation_error raise

(*
  This pass does the following:
  - aggregates declarations into a chain of let-ins
  - flatten modules into let-ins
*)

module Data = struct
  type scope = { exp : exp_ list ; mod_ : mod_ list }
  and mod_ = { name: module_variable ; items : scope }
  and exp_ = { name: expression_variable ; fresh_name : expression_variable ; item: Ast_aggregated.expression }
  and path = module_variable list
  let empty = { exp = [] ; mod_ = [] }
  let resolve_path : scope -> path -> scope =
    fun scope requested_path ->
      let f : scope -> module_variable -> scope = fun acc module_variable ->
        match List.find acc.mod_ ~f:(fun x -> ModuleVar.equal x.name module_variable) with
        | Some x -> x.items
        | _ -> failwith "couldnt resolve"
      in
      List.fold requested_path ~init:scope ~f
  let rm_exp : scope -> I.expression_variable -> scope = fun items to_rm ->
    let exp = List.filter items.exp ~f:(fun x -> not @@ ValueVar.equal x.name to_rm) in
    { items with exp }
  let add_exp : scope -> exp_ -> scope =
    fun scope new_exp ->
      let exp = List.filter scope.exp ~f:(fun x -> not (ValueVar.equal x.name new_exp.name)) in
      { scope with exp = new_exp :: exp}
  let shadow_module : scope -> module_variable -> scope -> scope = fun scope mod_var new_items ->
    let mod_ = List.filter scope.mod_ ~f:(fun x -> not (ModuleVar.equal x.name mod_var)) in
    let mod_ = {name = mod_var ; items = new_items } :: mod_ in
    { scope with mod_}
  let resolve_variable : scope -> expression_variable -> expression_variable =
    fun scope v ->
      match List.find scope.exp ~f:(fun x -> ValueVar.equal v x.name) with
      | Some x -> x.fresh_name
      | None -> v
  let resolve_variable_in_path : scope -> module_variable list -> expression_variable -> expression_variable =
    fun scope path v ->
      let x = resolve_path scope path in
      resolve_variable x v
end

type irepr =
  | New_decl of (O.expression_variable * O.expression_variable * O.expression * O.known_attributes)
  | New_mod of module_variable * irepr list
  | Alias of Data.scope

let rec compile ~raise : Data.scope -> Data.path -> I.expression -> I.program -> O.expression =
  fun scope path hole module_ ->
    let scope, decls = compile_declarations ~raise scope path module_ in
    let init = compile_expression ~raise scope [] hole in
    let rec flatten = fun acc x ->
      match x with
      | New_decl _ -> acc @ [x]
      | New_mod (_,inner) ->
        let inner = List.fold_left inner ~f:flatten ~init:[] in
        acc @ inner
      | _ -> acc
    in
    let lst = List.fold_left decls ~f:flatten ~init:[] in
    List.fold_right lst ~init ~f:(
      fun x acc ->
        match x with
        | New_decl (_,binder,expr,attr) -> O.e_a_let_in binder expr acc attr
        | _ -> acc
    )

and compile_declarations ~raise : Data.scope -> Data.path -> I.declaration_loc list -> Data.scope * irepr list =
  fun init_scope path lst ->
    let f : Data.scope * irepr list -> I.declaration_loc -> Data.scope * irepr list =
      fun (acc_scope,acc_hic) decl ->
        let return d h = (d , acc_hic @ [h]) in
        match decl.wrap_content with
        | I.Declaration_type _ -> (acc_scope, acc_hic)
        | I.Declaration_constant { binder ; expr ; attr } -> (
          let item = compile_expression ~raise acc_scope [] expr in
          let fresh_name = fresh_name binder path in
          let n = ({ name = binder ; fresh_name ; item } : Data.exp_) in
          let acc_scope = Data.add_exp acc_scope n in
          let hic = New_decl (binder,fresh_name,item,attr) in
          return acc_scope hic
        )
        | I.Declaration_module { module_binder ; module_ ; module_attr = _ } -> (
          let rec merge_inner_outer : irepr list -> Data.scope = fun lst ->
              let f : Data.scope -> irepr -> Data.scope = fun acc hic ->
                match hic with
                | New_decl (name,fresh_name,exp,_) ->
                  let exp_ = ({ name ; fresh_name ; item = exp } : Data.exp_) in
                  Data.add_exp acc exp_
                | New_mod (name, hics) ->
                  let items = merge_inner_outer hics in
                  Data.shadow_module acc name items
                | Alias items -> items
              in
              List.fold lst ~f ~init:Data.empty
          in
          let _,ireprs = compile_declarations ~raise acc_scope (path@[module_binder]) module_ in
          let items = merge_inner_outer ireprs in
          let acc_scope = Data.shadow_module acc_scope module_binder items in
          let hic = New_mod (module_binder, ireprs) in
          return acc_scope hic
        )
        | I.Module_alias { alias ; binders } -> (
          let items = Data.resolve_path acc_scope (List.Ne.to_list binders) in
          let acc_scope = Data.shadow_module acc_scope alias items in
          let hic = Alias acc_scope in
          return acc_scope hic
        )
    in
    List.fold lst ~init:(init_scope,[]) ~f

and compile_type ~raise : I.type_expression -> O.type_expression =
  fun ty ->
    let self = compile_type ~raise in
    let return type_content : O.type_expression = { type_content ; orig_var = ty.orig_var ; location = ty.location ; source_type = Some ty } in
    let map_rows : I.row_element label_map -> O.row_element label_map = fun rows ->
      let f : I.row_element -> O.row_element = fun row -> { row with associated_type = self row.associated_type} in
      LMap.map f rows
    in
    match ty.type_content with
    | T_variable x -> return (T_variable x)
    | T_constant { language ; injection ; parameters } ->
      let parameters = List.map parameters ~f:self in
      return (T_constant { language ; injection ; parameters })
    | T_sum { content ; layout } ->
      let content = map_rows content in
      return (T_sum { content ; layout })
    | T_record { content ; layout } ->
      let content = map_rows content in
      return (T_record { content ; layout })
    | T_arrow { type1 ; type2 } ->
      let type1 = self type1 in
      let type2 = self type2 in
      return (T_arrow { type1 ; type2 })
    | T_module_accessor _ -> failwith "module accessor types should not end up here"
    | T_singleton x -> return (T_singleton x)
    | T_abstraction { ty_binder ; kind ; type_ } ->
      let type_ = self type_ in
      return (T_abstraction { ty_binder ; kind ; type_ })
    | T_for_all { ty_binder ; kind ; type_ } ->
      let type_ = self type_ in
      return (T_for_all { ty_binder ; kind ; type_ })

and compile_expression ~raise : Data.scope -> Data.path -> I.expression -> O.expression =
  fun scope path expr ->
    let self ?(data = scope) = compile_expression ~raise data path in
    let return expression_content : O.expression =
      let type_expression = compile_type ~raise expr.type_expression in
      { expression_content ; type_expression ; location = expr.location } in
    match expr.expression_content with
    | I.E_literal l ->
      return (O.E_literal l)
    | I.E_variable v -> (
      let v = Data.resolve_variable scope v in
      return (O.E_variable v)
    )
    | I.E_raw_code { language ; code } ->
      let code = self code in
      return (O.E_raw_code { language ; code })
    | I.E_matching {matchee=e;cases} -> (
      let e' = self e in
      let cases' = compile_cases ~raise scope path cases in
      return @@ O.E_matching {matchee=e';cases=cases'}
    )
    | I.E_record_accessor {record; path} -> (
      let record = self record in
      return @@ O.E_record_accessor {record; path}
    )
    | I.E_record m -> (
      let m' = O.LMap.map self m in
      return @@ O.E_record m'
    )
    | I.E_record_update {record; path; update} -> (
      let record = self record in
      let update = self update in
      return @@ O.E_record_update {record;path;update}
    )
    | I.E_constructor { constructor ; element } -> (
      let element = self element in
      return @@ O.E_constructor { constructor ; element }
    )
    | I.E_application {lamb; args} -> (
      let ab = (lamb, args) in
      let (a,b) = Pair.map ~f:self ab in
      return @@ O.E_application {lamb=a;args=b}
    )
    | I.E_let_in { let_binder ; rhs ; let_result; attr } -> (
      let rhs = self rhs in
      let data = Data.rm_exp scope let_binder in
      let let_result = self ~data let_result in
      return @@ O.E_let_in { let_binder ; rhs ; let_result; attr }
    )
    | I.E_type_in {type_binder; rhs; let_result} -> (
      let let_result = self let_result in
      let rhs = compile_type ~raise rhs in
      return @@ O.E_type_in {type_binder; rhs; let_result}
    )
    | I.E_lambda { binder ; result } -> (
      let data = Data.rm_exp scope binder in
      let result = self ~data result in
      return @@ O.E_lambda { binder ; result }
    )
    | I.E_type_abstraction {type_binder; result} -> (
      let result = self result in
      return @@ O.E_type_abstraction {type_binder; result}
    )
    | I.E_type_inst { forall ; type_ } -> (
      let forall = self forall in
      let type_ = compile_type ~raise type_ in
      return @@ O.E_type_inst { forall ; type_ }
    )
    | I.E_recursive { fun_name; fun_type; lambda = {binder;result}} -> (
      let data = Data.rm_exp scope binder in
      let data = Data.rm_exp data fun_name in
      let result = self ~data result in
      let fun_type = compile_type ~raise fun_type in
      return @@ O.E_recursive { fun_name; fun_type; lambda = {binder;result}}
    )
    | I.E_constant { cons_name ; arguments } -> (
      let arguments = List.map ~f:self arguments in
      return @@ O.E_constant { cons_name ; arguments }
    )
    | I.E_module_accessor { module_name; element} -> (
      let rec aux : module_variable List.Ne.t -> (O.type_expression * O.type_expression) list -> I.expression -> _ * _ List.Ne.t * (O.type_expression * O.type_expression) list * _ option =
        fun acc_path acc_types exp ->
          match exp.expression_content with
          | E_module_accessor {module_name ; element} ->
            let acc_path = Simple_utils.List.Ne.cons module_name acc_path in
            aux acc_path acc_types element
          | E_type_inst { forall ; type_ } ->
            let type_ = compile_type ~raise type_ in
            let exp_ty = compile_type ~raise exp.type_expression in
            aux acc_path ((type_, exp_ty) :: acc_types) forall
          | E_variable v ->
            v, acc_path, acc_types, None
          | E_record_accessor _ ->
            let rec aux' (e : I.expression) acc_path = match e.expression_content with
              | E_variable v ->
                v, e.type_expression, acc_path
              | E_record_accessor { record ; path } ->
                aux' record ((path, compile_type ~raise e.type_expression) :: acc_path)
              | _ -> failwith "not allowed in the syntax" in
            let v, t, path = aux' exp [] in
            v, acc_path, acc_types, Some (t, path)
          | _ -> failwith "not allowed in the syntax"
      in
      let v, path, types, record_path = aux (List.Ne.of_list [module_name]) [] element in
      let path = List.Ne.rev path in
      let path = List.Ne.to_list path in
      let v = Data.resolve_variable_in_path scope path v in
      match record_path with
      | None ->
        let expr = O.e_a_variable v (compile_type ~raise expr.type_expression) in
        List.fold_right ~f:(fun (t, u) e -> O.e_a_type_inst e t u) ~init:expr (List.rev types)
      | Some (t, record_path) ->
        let expr = O.e_a_variable v (compile_type ~raise t) in
        let expr = List.fold_right ~f:(fun (l, t) r -> O.e_a_record_accessor r l t) ~init:expr record_path in
        List.fold_right ~f:(fun (t, u) e -> O.e_a_type_inst e t u) ~init:expr (List.rev types)
    )
    | I.E_mod_in { module_binder ; rhs ; let_result } -> (
      let local_name = ModuleVar.add_prefix "LOCAL#in" module_binder in
      compile ~raise scope path let_result
        [
          Location.wrap @@ I.Declaration_module { module_binder = local_name ; module_ = rhs ; module_attr = {public = false}} ;
          Location.wrap @@ I.Module_alias { alias = module_binder ; binders = (local_name,[]) }
        ]
    )
    | I.E_mod_alias { alias ; binders ; result } -> (
      compile ~raise scope path result
        [
          Location.wrap @@ I.Module_alias { alias ; binders }
        ]
    )

and compile_cases ~raise : Data.scope -> Data.path -> I.matching_expr -> O.matching_expr =
  fun scope path m ->
    match m with
    | Match_variant {cases;tv} -> (
        let aux { I.constructor ; pattern ; body } =
          let data = Data.rm_exp scope pattern in
          let body = compile_expression ~raise data path body in
          {O.constructor;pattern;body}
        in
        let cases = List.map ~f:aux cases in
        let tv = compile_type ~raise tv in
        Match_variant {cases ; tv}
      )
    | Match_record {fields; body; tv} ->
      let fields = O.LMap.map (fun (v, t) -> (v, compile_type ~raise t)) fields in
      let lst = List.map ~f:fst (O.LMap.values fields) in
      let data = List.fold_right ~f:(fun v data -> Data.rm_exp data v) ~init:scope lst in
      let body = compile_expression ~raise data path body in
      let tv = compile_type ~raise tv in
      Match_record {fields; body; tv}

and fresh_name : I.expression_variable -> Data.path -> O.expression_variable  = fun v path ->
  match path with
  | [] -> v
  | _ ->
    let name,_ = ValueVar.internal_get_name_and_counter v in
    let name = List.fold_right ~f:(fun s r -> ModuleVar.to_name_exn s ^ "#" ^ r) ~init:name path in
    let name = "#" ^ name in
    ValueVar.fresh ~loc:(ValueVar.get_location v) ~name ()
