module I = Ast_typed
module O = Ast_aggregated
open Ligo_prim

(*
  This pass flattens programs with module declarations into a list of simple top-level declarations.
  Later, this list is morphed into a single expression of chained let-ins.
  
  It works as follow:
  - morph input program I into the Data IR (see doc bellow)
  - morph the IR into a list of top-level declaration
*)

module Data = struct
  (*
    IR of a module (i.e. a program)
    [env]
      Bindings accessible *from* a given module. It is used to resolve variables and module accesses.
      It includes bindings that are declared within the module itself:
      e.g. in the following, module `A` env contains both binding for `y` and `x`
      ```
      let x = ..
      module A = struct
        let y = ..
      end
      ```
    [content]
      Actual module content. It is used to build the final result of this pass (the list of top-level declarations) and 
      also to build expression when encountering the `E_mod_in` node.
      For each declaration it holds the binding and the payload (expression or another module).
  *)
  type t =
    { env : env
    ; content : decl list
    }

  and env =
    { exp : var_binding list
    ; mod_ : mod_ list
    }

  and decl =
    | Mod of mod_
    | Exp of exp_
    | Pat of pat_

  and mod_ =
    { name : Module_var.t
    ; in_scope : t
    }

  and pat_ =
    { binding : pat_binding
    ; item : (O.expression[@sexp.opaque])
    ; attr : (O.ValueAttr.t[@sexp.opaque])
    }

  and exp_ =
    { binding : var_binding
    ; item : (O.expression[@sexp.opaque])
    ; attr : (O.ValueAttr.t[@sexp.opaque])
    }

  (* [binding_] link [old] identifiers to freshly generated ones [fresh] *)
  and 'a binding_ =
    { old : 'a
    ; fresh : 'a
    }

  and var_binding = Value_var.t binding_
  and pat_binding = (O.ty_expr[@sexp.opaque]) O.Pattern.t binding_ [@@deriving sexp_of]

  let pp ppf data = Format.fprintf ppf "%a" Sexp.pp_hum (sexp_of_t data)
  let empty = { env = { exp = []; mod_ = [] }; content = [] }

  (*
    Important note: path is _ONLY_ used for naming of fresh variables, so that debuging a printed AST is easier.  
    e.g. module access `A.B.x` will become variable `#A#B#x`
    IT SHOULD NEVER BE USED FOR INTERNAL COMPUTATION OR RESOLVING
  *)
  type path = Module_var.t list

  let fresh_var : Value_var.t -> path -> Value_var.t =
   fun v path ->
    match path with
    | [] -> v
    | _ ->
      let name, _ = Value_var.internal_get_name_and_counter v in
      let name =
        List.fold_right ~f:(fun s r -> Module_var.to_name_exn s ^ "#" ^ r) ~init:name path
      in
      let name = "#" ^ name in
      Value_var.fresh ~loc:(Value_var.get_location v) ~name ()


  let fresh_pattern : O.ty_expr O.Pattern.t -> path -> O.ty_expr O.Pattern.t =
   fun pattern path ->
    O.Pattern.map_pattern
      (Location.map (function
          | Linear_pattern.P_var x ->
            O.Pattern.P_var (Binder.set_var x (fresh_var (Binder.get_var x) path))
          | x -> x))
      pattern


  let resolve_path : t -> path -> t =
   fun data requested_path ->
    let f : t -> Module_var.t -> t =
     fun acc module_variable ->
      match
        List.find acc.env.mod_ ~f:(fun x -> Module_var.equal x.name module_variable)
      with
      | Some x -> x.in_scope
      | _ ->
        failwith
          (Format.asprintf
             "couldnt find %a in: \n %a "
             Module_var.pp
             module_variable
             pp
             data)
    in
    List.fold requested_path ~init:data ~f


  let rm_exp : t -> I.expression_variable -> t =
   fun { env = { exp; mod_ }; content } to_rm ->
    { env =
        { exp = List.filter exp ~f:(fun x -> not @@ Value_var.equal x.old to_rm); mod_ }
    ; content
    }


  let add_exp : t -> exp_ -> t =
   fun { env = { exp; mod_ }; content } new_exp ->
    let exp =
      List.filter exp ~f:(fun x -> not (Value_var.equal x.old new_exp.binding.old))
    in
    { env = { exp = new_exp.binding :: exp; mod_ }; content = content @ [ Exp new_exp ] }


  let pat_to_var_bindings : pat_binding -> var_binding list =
   fun { old; fresh } ->
    let names = List.map ~f:Binder.get_var @@ I.Pattern.binders old in
    let freshs = List.map ~f:Binder.get_var @@ I.Pattern.binders fresh in
    List.map ~f:(fun (old, fresh) -> { old; fresh }) (List.zip_exn names freshs)


  let add_exp_pat : t -> pat_ -> t =
   fun { env = { exp; mod_ }; content } new_pat ->
    let new_bound = pat_to_var_bindings new_pat.binding in
    let exp =
      List.filter exp ~f:(fun x ->
          not @@ List.exists new_bound ~f:(fun new_ -> Value_var.equal x.old new_.old))
    in
    { env = { exp = new_bound @ exp; mod_ }; content = content @ [ Pat new_pat ] }


  let add_module : t -> Module_var.t -> t -> t =
   fun { env = { exp; mod_ }; content } mod_var new_scope ->
    let mod_ =
      { name = mod_var; in_scope = new_scope }
      :: List.filter mod_ ~f:(fun x -> not (Module_var.equal x.name mod_var))
    in
    let content = content @ [ Mod { name = mod_var; in_scope = new_scope } ] in
    { env = { exp; mod_ }; content }


  let resolve_variable : t -> Value_var.t -> Value_var.t =
   fun data v ->
    match List.find data.env.exp ~f:(fun x -> Value_var.equal v x.old) with
    | Some x -> x.fresh
    | None -> v


  let resolve_variable_in_path : t -> path -> Value_var.t -> Value_var.t =
   fun data path v ->
    let x = resolve_path data path in
    resolve_variable x v


  let rec refresh : t -> path -> t =
   fun data path ->
    (* for all declaration in content data, assign a new fresh binding and update the environements *)
    let refreshed_content =
      List.map
        ~f:(function
          | Mod { name; in_scope } ->
            Mod { name; in_scope = refresh in_scope (path @ [ name ]) }
          | Exp ({ binding = { old; fresh = _ }; _ } as x) ->
            Exp { x with binding = { old; fresh = fresh_var old path } }
          | Pat ({ binding = { old; fresh = _ }; _ } as x) ->
            Pat { x with binding = { old; fresh = fresh_pattern old path } })
        data.content
    in
    let refreshed_mod_ =
      List.map data.env.mod_ ~f:(function { name; in_scope } ->
          let in_scope_opt =
            (* refresh has been recursively applied when refreshing the content, just copy the result here *)
            List.find_map refreshed_content ~f:(function
                | Mod x -> if Module_var.equal x.name name then Some x.in_scope else None
                | _ -> None)
          in
          { name; in_scope = Option.value ~default:in_scope in_scope_opt })
    in
    let refreshed_exp =
      refreshed_content
      |> List.map ~f:(function
             | Mod _ -> []
             | Exp x -> [ x.binding ]
             | Pat x -> pat_to_var_bindings x.binding)
      |> List.join
      |> List.fold ~init:data.env.exp ~f:(fun exp_env { old; fresh } ->
             List.map
               ~f:(fun ({ old = old'; fresh = _ } as x) ->
                 if Value_var.equal old old' then { x with fresh } else x)
               exp_env)
    in
    { env = { exp = refreshed_exp; mod_ = refreshed_mod_ }; content = refreshed_content }
end

let aggregate_scope : Data.t -> leaf:O.expression -> O.expression =
 fun data ~leaf ->
  let rec f : Data.decl -> O.expression -> O.expression =
   fun d acc_exp ->
    match d with
    | Pat { binding = { old = _; fresh }; item; attr } ->
      O.e_a_let_in ~loc:item.location fresh item acc_exp attr
    | Exp { binding = { old = _; fresh }; item; attr } ->
      let binder =
        O.Pattern.var ~loc:(Value_var.get_location fresh)
        @@ Binder.make fresh item.type_expression
      in
      O.e_a_let_in ~loc:item.location binder item acc_exp attr
    | Mod { in_scope = { content; _ }; _ } -> List.fold_right content ~f ~init:acc_exp
  in
  List.fold_right data.content ~f ~init:leaf


let build_context : Data.t -> O.context =
 fun data ->
  let rec f : Data.decl -> O.declaration list =
   fun d ->
    match d with
    | Pat { binding = { old = _; fresh }; item; attr } ->
      [ Location.wrap
          ~loc:item.location
          (O.D_irrefutable_match { pattern = fresh; expr = item; attr })
      ]
    | Exp { binding = { old = _; fresh }; item; attr } ->
      let binder = Binder.make fresh item.type_expression in
      [ Location.wrap ~loc:item.location (O.D_value { binder; expr = item; attr }) ]
    | Mod { in_scope = { content; _ }; _ } -> List.join (List.map content ~f)
  in
  List.join (List.map ~f data.content)


let rec compile : Data.t -> Data.path -> I.expression -> I.program -> O.program =
 fun data path hole module_ ->
  let data = compile_declarations data path module_ in
  let hole = compile_expression data [] hole in
  build_context data, hole


and compile_declarations : Data.t -> Data.path -> I.module_ -> Data.t =
 fun init_scope path lst ->
  let f : Data.t -> I.declaration -> Data.t =
   fun acc_scope decl ->
    match decl.wrap_content with
    | I.D_type _ -> acc_scope
    | I.D_irrefutable_match { pattern; expr; attr } ->
      let pat =
        let item = compile_expression acc_scope [] expr in
        let pattern = I.Pattern.map compile_type pattern in
        let fresh = Data.fresh_pattern pattern path in
        (Data.{ binding = { old = pattern; fresh }; item; attr } : Data.pat_)
      in
      Data.add_exp_pat acc_scope pat
    | I.D_value { binder; expr; attr } ->
      let exp =
        let item = compile_expression acc_scope [] expr in
        let fresh = Data.fresh_var binder.var path in
        (Data.{ binding = { old = binder.var; fresh }; item; attr } : Data.exp_)
      in
      Data.add_exp acc_scope exp
    | I.D_module { module_binder; module_; module_attr = _ } ->
      let rhs_glob = compile_module_expr acc_scope (path @ [ module_binder ]) module_ in
      Data.add_module acc_scope module_binder rhs_glob
  in
  List.fold lst ~init:init_scope ~f


(*
  [copy_content] let you control if the module content should be entirely copied 
  as a new set of bindings, or if we should just make reference to it
*)
and compile_module_expr ?(copy_content = false)
    : Data.t -> Data.path -> I.module_expr -> Data.t
  =
 fun data path mexpr ->
  match mexpr.module_content with
  | M_struct prg -> compile_declarations { data with content = [] } path prg
  | M_variable v ->
    let res = Data.resolve_path data [ v ] in
    if copy_content then Data.refresh res path else { res with content = [] }
  | M_module_path m_path ->
    let res = Data.resolve_path data (List.Ne.to_list m_path) in
    if copy_content then Data.refresh res path else { res with content = [] }


and compile_type : I.type_expression -> O.type_expression =
 fun ty ->
  let self = compile_type in
  let return type_content : O.type_expression =
    { type_content
    ; orig_var = ty.orig_var
    ; location = ty.location
    ; source_type = Some ty
    }
  in
  match ty.type_content with
  | T_variable x -> return (T_variable x)
  | T_constant { language; injection; parameters } ->
    return (T_constant { language; injection; parameters = List.map parameters ~f:self })
  | T_sum r -> return (T_sum (I.Row.map self r))
  | T_record r -> return (T_record (I.Row.map self r))
  | T_arrow x -> return (T_arrow (Arrow.map self x))
  | T_singleton x -> return (T_singleton x)
  | T_abstraction x -> return (T_for_all (Abstraction.map self x))
  | T_for_all x -> return (T_for_all (Abstraction.map self x))


and compile_expression : Data.t -> Data.path -> I.expression -> O.expression =
 fun data path expr ->
  let self ?(data = data) = compile_expression data path in
  let self_ty = compile_type in
  let return expression_content : O.expression =
    let type_expression = compile_type expr.type_expression in
    { expression_content; type_expression; location = expr.location }
  in
  match expr.expression_content with
  (* resolving variable names *)
  | I.E_variable v ->
    let v = Data.resolve_variable data v in
    return (O.E_variable v)
  | I.E_module_accessor { module_path; element } ->
    let v = Data.resolve_variable_in_path data module_path element in
    return (O.E_variable v)
  (* bounding expressions *)
  | I.E_matching { matchee; cases } ->
    let cases =
      List.map
        ~f:(fun { pattern; body } ->
          let data =
            List.fold
              ~init:data
              ~f:(fun data binder -> Data.rm_exp data (Binder.get_var binder))
              (I.Pattern.binders pattern)
          in
          O.Match_expr.{ pattern = I.Pattern.map self_ty pattern; body = self ~data body })
        cases
    in
    return (O.E_matching { matchee = self matchee; cases })
  | I.E_for { binder; start; final; incr; f_body } ->
    let data = Data.rm_exp data binder in
    return
      (O.E_for
         { binder
         ; start = self start
         ; final = self final
         ; incr = self incr
         ; f_body = self ~data f_body
         })
  | I.E_recursive
      { fun_name; fun_type; lambda = { binder; result; output_type }; force_lambdarec } ->
    let data = Data.rm_exp data (Param.get_var binder) in
    let data = Data.rm_exp data fun_name in
    let result = self ~data result in
    let fun_type = self_ty fun_type in
    let output_type = self_ty output_type in
    let binder = Param.map self_ty binder in
    return
    @@ O.E_recursive
         { fun_name; fun_type; lambda = { binder; result; output_type }; force_lambdarec }
  | I.E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    let data =
      List.fold
        ~init:data
        ~f:(fun data binder -> Data.rm_exp data (Binder.get_var binder))
        (I.Pattern.binders let_binder)
    in
    let rhs = self rhs in
    let let_result = self ~data let_result in
    let let_binder = I.Pattern.map self_ty let_binder in
    return @@ O.E_let_mut_in { let_binder; rhs; let_result; attributes }
  | I.E_let_in { let_binder; rhs; let_result; attributes } ->
    let data =
      List.fold
        ~init:data
        ~f:(fun data binder -> Data.rm_exp data (Binder.get_var binder))
        (I.Pattern.binders let_binder)
    in
    let let_result = self ~data let_result in
    let let_binder = I.Pattern.map self_ty let_binder in
    return @@ O.E_let_in { let_binder; rhs = self rhs; let_result; attributes }
  | I.E_for_each { fe_binder = b, b_opt; collection; collection_type; fe_body } ->
    let data = Data.rm_exp data b in
    let data = Option.value_map b_opt ~default:data ~f:(fun b -> Data.rm_exp data b) in
    return
      (O.E_for_each
         { fe_binder = b, b_opt
         ; collection = self collection
         ; collection_type
         ; fe_body = self ~data fe_body
         })
  (* recursively call aggregation *)
  | I.E_mod_in { module_binder; rhs; let_result } ->
    let data =
      let rhs_scope =
        compile_module_expr
          data
          (Module_var.add_prefix "LOCAL#in" module_binder :: path)
          rhs
      in
      Data.add_module data module_binder rhs_scope
    in
    let x = Data.resolve_path data [ module_binder ] in
    aggregate_scope x ~leaf:(self ~data let_result)
  (* trivials *)
  | I.E_literal l -> return (O.E_literal l)
  | I.E_raw_code x -> return (O.E_raw_code (Raw_code.map self x))
  | I.E_accessor x -> return (O.E_accessor (I.Accessor.map self x))
  | I.E_record m -> return (O.E_record (Record.map m ~f:self))
  | I.E_update x -> return (O.E_update (I.Update.map self x))
  | I.E_constructor x -> return (O.E_constructor (Constructor.map self x))
  | I.E_application x -> return (O.E_application (Application.map self x))
  | I.E_lambda x -> return (O.E_lambda (Lambda.map self self_ty x))
  | I.E_type_abstraction x -> return (O.E_type_abstraction (Type_abs.map self x))
  | I.E_type_inst { forall; type_ } ->
    return (O.E_type_inst { forall = self forall; type_ = self_ty type_ })
  | I.E_constant x -> return (O.E_constant (Constant.map self x))
  | I.E_assign x -> return (O.E_assign (Assign.map self self_ty x))
  | I.E_deref x -> return (O.E_deref x)
  | I.E_while x -> return (O.E_while (While_loop.map self x))
