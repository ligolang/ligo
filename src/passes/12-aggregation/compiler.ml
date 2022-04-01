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

  type scope = { exp : exp_ list ; mod_ : mod_ list ; decls : decl list }
  and decl = Mod of mod_ | Exp of exp_
  and mod_ = { name: module_variable ; in_scope : scope }
  and exp_ = { name: expression_variable ; fresh_name : expression_variable ; item: Ast_aggregated.expression ; attr : Ast_aggregated.known_attributes ; attributes : Ast_aggregated.binder_attributes }
  (* Important note: path is _only_ used for naming of fresh variables, so that debuging a printed AST is easier *)
  and path = module_variable list
  module PP_DEBUG = struct
    open Format
    open Stage_common.PP
    let rec pp ppf {exp ;mod_} =
      let pp_mod_ ppf { name ; in_scope } = fprintf ppf "{ name = %a ; items = @[<v 2>@.%a@] }" module_variable name pp in_scope in
      let pp_exp_ ppf { name ; fresh_name ; item = _ } = fprintf ppf "{ name = %a ; fresh_name = %a ; items = XX }" expression_variable name expression_variable fresh_name (*Ast_aggregated.PP.expression item*) in
      fprintf ppf "{ exp = @[<v>%a@] ; mod_ = @[<v 2>@.%a@] }" Simple_utils.PP_helpers.(list_sep pp_exp_ (tag "@.")) exp Simple_utils.PP_helpers.(list_sep pp_mod_ (tag "@.")) mod_
  end
  let empty = { exp = [] ; mod_ = [] ; decls = [] }
  let resolve_path : scope -> path -> scope =
    fun scope requested_path ->
      let f : scope -> module_variable -> scope = fun acc module_variable ->
        match List.find acc.mod_ ~f:(fun x -> ModuleVar.equal x.name module_variable) with
        | Some x -> x.in_scope
        | _ -> failwith (Format.asprintf "couldnt find %a in: \n %a " ModuleVar.pp module_variable PP_DEBUG.pp scope)
      in
      List.fold requested_path ~init:scope ~f
  let rm_exp : scope -> I.expression_variable -> scope = fun items to_rm ->
    let exp = List.filter items.exp ~f:(fun x -> not @@ ValueVar.equal x.name to_rm) in
    { items with exp }
  let add_exp : scope -> exp_ -> scope =
    fun scope new_exp ->
      let exp = List.filter scope.exp ~f:(fun x -> not (ValueVar.equal x.name new_exp.name)) in
      { scope with
        exp = new_exp :: exp ;
        decls = Exp new_exp :: scope.decls }
  let add_module : scope -> module_variable -> scope -> scope = fun scope mod_var new_scope ->
    let mod_ = List.filter scope.mod_ ~f:(fun x -> not (ModuleVar.equal x.name mod_var)) in
    let mod_ = {name = mod_var ; in_scope = new_scope } :: mod_ in
    let decls = Mod { name = mod_var ; in_scope = new_scope} :: scope.decls in
    { scope with mod_ ; decls }
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

let aggregate_scope : Data.scope -> leaf:O.expression -> O.expression = fun scope ~leaf ->
  let rec f : O.expression -> Data.decl -> O.expression =
    fun acc_exp d ->
      match d with
      | Exp { name = _ ; fresh_name ; item ; attr ; attributes} ->
        O.e_a_let_in {var=fresh_name;ascr=Some item.type_expression;attributes} item acc_exp attr
      | Mod { in_scope = { decls ; _ } ; _ } ->
        List.fold_left decls ~f ~init:acc_exp
  in
  List.fold_left scope.decls ~f ~init:leaf

let rec compile ~raise : Data.scope -> Data.path -> I.expression -> I.program -> O.expression =
  fun scope path hole module_ ->
    let scope = compile_declarations ~raise scope path module_ in
    let hole = compile_expression ~raise scope [] hole in
    aggregate_scope scope ~leaf:hole

and compile_declarations ~raise : Data.scope -> Data.path -> I.module_ -> Data.scope =
  fun init_scope path lst ->
    let f : Data.scope -> I.declaration -> Data.scope =
      fun acc_scope decl ->
        match decl.wrap_content with
        | I.Declaration_type _ -> acc_scope
        | I.Declaration_constant { binder ; expr ; attr } -> (
          let exp =
            let item = compile_expression ~raise acc_scope [] expr in
            let fresh_name = fresh_name binder.var path in
            ({ name = binder.var ; fresh_name ; item ; attr ; attributes = binder.attributes} : Data.exp_)
          in
          Data.add_exp acc_scope exp
        )
        | I.Declaration_module { module_binder ; module_ ; module_attr = _ } -> (
          let rhs_glob = compile_module_expr ~raise acc_scope(path@[module_binder]) module_ in
          Data.add_module acc_scope module_binder rhs_glob
        )
    in
    List.fold lst ~init:init_scope ~f

and compile_module_expr ~raise : Data.scope -> Data.path -> I.module_expr -> Data.scope =
  fun scope path mexpr ->
    match mexpr.wrap_content with
    | M_struct prg -> (
      compile_declarations ~raise {scope with decls = []} path prg
    )
    | M_variable v -> (
      let res = Data.resolve_path scope [v] in
      { res with decls = [] }
    )
    | M_module_path path -> (
      let res = Data.resolve_path scope (List.Ne.to_list path) in
      { res with decls = [] }
    )

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
    | T_abstraction _ ->
      raise.raise @@ Errors.corner_case "Abstraction type uncaught"
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
      let data = Data.rm_exp scope let_binder.var in
      let let_result = self ~data let_result in
      let let_binder = Stage_common.Maps.binder (compile_type ~raise) let_binder in
      return @@ O.E_let_in { let_binder ; rhs ; let_result; attr }
    )
    | I.E_type_in {type_binder; rhs; let_result} -> (
      let let_result = self let_result in
      let rhs = compile_type ~raise rhs in
      return @@ O.E_type_in {type_binder; rhs; let_result}
    )
    | I.E_lambda { binder ; result } -> (
      let data = Data.rm_exp scope binder.var in
      let binder = Stage_common.Maps.binder (compile_type ~raise) binder in
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
      let data = Data.rm_exp scope binder.var in
      let data = Data.rm_exp data fun_name in
      let result = self ~data result in
      let fun_type = compile_type ~raise fun_type in
      let binder = Stage_common.Maps.binder (compile_type ~raise) binder in
      return @@ O.E_recursive { fun_name; fun_type; lambda = {binder;result}}
    )
    | I.E_constant { cons_name ; arguments } -> (
      let arguments = List.map ~f:self arguments in
      return @@ O.E_constant { cons_name ; arguments }
    )
    | I.E_module_accessor { module_path ; element} -> (
      let v = Data.resolve_variable_in_path scope module_path element in
      return (O.E_variable v)
    )
    | I.E_mod_in { module_binder ; rhs ; let_result } -> (
      let data =
        let rhs_scope = compile_module_expr ~raise scope (ModuleVar.add_prefix "LOCAL#in" module_binder :: path) rhs in
        Data.add_module scope module_binder rhs_scope
      in
      let x = Data.resolve_path data [module_binder] in
      aggregate_scope x ~leaf:(self ~data let_result)
    )
    | I.E_assign {binder;access_path;expression} ->
      let aux_ap = function
        Access_record s -> Access_record s
      | Access_tuple  i -> Access_tuple  i
      | Access_map    e -> Access_map (self e)
      in
      let var = Data.resolve_variable scope binder.var in
      let access_path = List.map ~f:aux_ap access_path in
      let expression = self expression in
      let binder = {binder with var;ascr=Option.map ~f:(compile_type ~raise) binder.ascr} in
      return @@ O.E_assign {binder;access_path;expression}

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
      let fields = O.LMap.map (fun (b : _ O.binder) -> {b with ascr = Option.map ~f:(compile_type ~raise) b.ascr}) fields in
      let lst = List.map ~f:(fun b -> b.var) (O.LMap.values fields) in
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
