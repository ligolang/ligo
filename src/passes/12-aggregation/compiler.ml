open Core
open Errors
open Ligo_prim
module Location = Simple_utils.Location
module Trace = Simple_utils.Trace
module I = Ast_typed
module O = Ast_aggregated

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
      For each declaration it holds the binding and the payload (expression or another module)
      together with its own environment.
  *)

  module Var_map = Core.Map.Make (Value_var)

  type var_binding_map = Value_var.t Var_map.t

  type t =
    { env : env
    ; content : int list
    }

  and env =
    { exp : (var_binding_map[@sexp.opaque])
    ; mod_ : mod_ list
    }

  and mod_ =
    { name : Module_var.t
    ; in_ : t
    }

  (* [binding_] link [old] identifiers to freshly generated ones [fresh] *)
  and 'a binding_ =
    { old : 'a
    ; fresh : 'a
    }

  and var_binding = Value_var.t binding_
  and pat_binding = (O.ty_expr[@sexp.opaque]) O.Pattern.t binding_ [@@deriving sexp_of]

  (*
    Important note: path is _ONLY_ used for naming of fresh variables, so that debuging a printed AST is easier.
    e.g. module access `A.B.x` will become variable `#A#B#x`
    IT SHOULD NEVER BE USED FOR INTERNAL COMPUTATION OR RESOLVING
  *)
  type path = string list

  module IMap = Map.Make (struct
    type t = int [@@deriving sexp]

    let compare = Int.compare
  end)

  type decl =
    | Mod of mod_
    | Incl of int list
    | Exp of exp_
    | Pat of pat_

  and pat_ =
    { binding : pat_binding
    ; env : env
    ; item : (I.expression[@sexp.opaque])
    ; attr : (O.ValueAttr.t[@sexp.opaque])
    ; loc : Location.t
    }

  and exp_ =
    { binding : var_binding
    ; env : (env[@sexp.opaque])
    ; item : (I.expression[@sexp.opaque])
    ; attr : (O.ValueAttr.t[@sexp.opaque])
    ; loc : Location.t
    }

  let decl_map : (int * decl IMap.t) ref = ref (0, IMap.empty)

  let new_global_decl : decl -> int =
   fun d ->
    let cur_max_idx, dm = !decl_map in
    decl_map := cur_max_idx + 1, Map.set ~key:cur_max_idx ~data:d dm;
    cur_max_idx


  let get_global_decl : int -> decl =
   fun i ->
    match Map.find (snd @@ !decl_map) i with
    | Some x -> x
    | None -> failwith "corner case: could not find decl in decl_map"


  let pp ppf data = Format.fprintf ppf "%a" Sexp.pp_hum (sexp_of_t data)
  let pp_env ppf data = Format.fprintf ppf "%a" Sexp.pp_hum (sexp_of_env data)
  let empty_env = { exp = Var_map.empty; mod_ = [] }
  let empty = { env = empty_env; content = [] }

  let extend_debug_path : path -> Module_var.t -> path =
   fun path modvar ->
    let name, _ = Module_var.internal_get_name_and_counter modvar in
    path @ [ name ]


  let fresh_var : Value_var.t -> path -> Value_var.t =
   fun v path ->
    match path with
    | [] -> v
    | _ ->
      let name, _ = Value_var.internal_get_name_and_counter v in
      let name = List.fold_right ~f:(fun s r -> s ^ "." ^ r) ~init:name path in
      (* hmm... these vars are kind of generated, but also kind of
         not; they come directly from user declarations after
         all. setting generated:false here so that they show up in env
         info for the debugger *)
      Value_var.fresh ~loc:(Value_var.get_location v) ~generated:false ~name ()


  let fresh_pattern : O.ty_expr O.Pattern.t -> path -> O.ty_expr O.Pattern.t =
   fun pattern path ->
    O.Pattern.map_pattern
      (Location.map (function
          | Linear_pattern.P_var x ->
            O.Pattern.P_var (Binder.set_var x (fresh_var (Binder.get_var x) path))
          | x -> x))
      pattern


  let resolve_path : env -> Module_var.t list -> t =
   fun env requested_path ->
    let f : t -> Module_var.t -> t =
     fun acc module_variable ->
      match
        List.find acc.env.mod_ ~f:(fun x -> Module_var.equal x.name module_variable)
      with
      | Some x -> x.in_
      | None ->
        (* XXX: Suppose the following contract:

           [let x : int = Byte.length]

           If [--typer-error-recovery] is enabled (on for the LSP), printing the env will
           take a very long time and hang the LSP. Because of this, it's not recommended
           to print the environment in production. *)
        failwith (Format.asprintf "Couldn't find %a in env" Module_var.pp module_variable)
    in
    List.fold requested_path ~init:{ content = []; env } ~f


  let rm_exp : env -> I.expression_variable -> env =
   fun env to_rm -> { env with exp = Core.Map.remove env.exp to_rm }


  let add_exp : t -> exp_ -> t =
   fun { env = { exp; mod_ }; content } new_exp ->
    { env =
        { exp = Core.Map.set exp ~key:new_exp.binding.old ~data:new_exp.binding.fresh
        ; mod_
        }
    ; content = new_global_decl (Exp new_exp) :: content
    }


  let pat_to_var_bindings : pat_binding -> var_binding_map =
   fun { old; fresh } ->
    let names = List.map ~f:Binder.get_var @@ I.Pattern.binders old in
    let freshs = List.map ~f:Binder.get_var @@ I.Pattern.binders fresh in
    List.fold_left
      ~init:Var_map.empty
      ~f:(fun mp (key, data) -> Core.Map.set mp ~key ~data)
      (List.zip_exn names freshs)


  let add_exp_pat : t -> pat_ -> t =
   fun { env = { exp; mod_ }; content } new_pat ->
    let new_bound = pat_to_var_bindings new_pat.binding in
    { env =
        { exp = Core.Map.merge_skewed new_bound exp ~combine:(fun ~key:_ v1 _v2 -> v1)
        ; mod_
        }
    ; content = new_global_decl (Pat new_pat) :: content
    }


  let add_module : t -> Module_var.t -> t -> t =
   fun { env = { exp; mod_ }; content } mod_var new_scope ->
    let mod_ =
      { name = mod_var; in_ = new_scope }
      :: List.filter mod_ ~f:(fun x -> not (Module_var.equal x.name mod_var))
    in
    let content = new_global_decl (Mod { name = mod_var; in_ = new_scope }) :: content in
    { env = { exp; mod_ }; content }


  let merge_env : env -> env -> env =
   fun { exp = exp1; mod_ = mod1 } { exp = exp2; mod_ = mod2 } ->
    let mod_ =
      List.filter mod2 ~f:(fun x ->
          not @@ List.exists mod1 ~f:(fun y -> Module_var.equal x.name y.name))
    in
    { exp = Core.Map.merge_skewed exp1 exp2 ~combine:(fun ~key:_ v1 _v2 -> v1)
    ; mod_ = mod1 @ mod_
    }


  let resolve_variable : env -> Value_var.t -> Value_var.t =
   fun env v ->
    match Core.Map.find env.exp v with
    | Some x -> x
    | None -> v


  let resolve_variable_in_path : env -> Module_var.t list -> Value_var.t -> Value_var.t =
   fun env path v ->
    let x = resolve_path env path in
    resolve_variable x.env v


  let memo_rec
      ?(size = 30)
      ?(hash = (Core.Hashtbl.hash_param 1_000 10_000 : 'a -> int))
      (f : ('a -> 'b) -> 'a -> 'b)
    =
    let h = Core.Hashtbl.create (module Int) ~size in
    let rec g x =
      let k = hash x in
      match Core.Hashtbl.find h k with
      | None ->
        let z = f g x in
        (* Warning: The original code called Caml.Hashtbl.add, which
           behaves differently if the key was already present: here,
           instead of hiding any previous binding, we leave the table
           unchanged. *)
        let (`Ok | `Duplicate) = Core.Hashtbl.add h ~key:k ~data:z in
        z
      | Some z -> z
    in
    g


  let rec refresh
      :  new_bindings:(Module_var.t list * var_binding) list -> t -> path
      -> (Module_var.t list * var_binding) list * t
    =
   fun ~new_bindings data path ->
    let refresh_env : _ -> (Module_var.t list * var_binding) list * env -> env =
     fun refresh_env (new_bindings, { exp; mod_ }) ->
      let exp =
        (* select new bindings at top-level only *)
        let news =
          List.filter_map
            ~f:(function
              | [], x -> Some x
              | _ -> None)
            new_bindings
        in
        List.fold_left news ~init:exp ~f:(fun exp { old; fresh } ->
            Core.Map.set exp ~key:old ~data:fresh)
      in
      let mod_ =
        List.map mod_ ~f:(fun { name; in_ } ->
            let news =
              List.filter_map
                ~f:(function
                  | b ->
                    (match b with
                    | [], _ -> Some b
                    | hd :: tl, x when Module_var.equal hd name -> Some (tl, x)
                    | _ -> None))
                new_bindings
            in
            { name; in_ = { in_ with env = refresh_env (news, in_.env) } })
      in
      { exp; mod_ }
    in
    let refresh_env x y = memo_rec refresh_env (x, y) in
    let dedup_new_bindings =
      let compare_new_bindings (ma, va) (mb, vb) =
        Tuple2.compare
          ~cmp1:(List.compare Module_var.compare)
          ~cmp2:Value_var.compare
          (ma, va.old)
          (mb, vb.old)
      in
      List.dedup_and_sort ~compare:compare_new_bindings
    in
    let new_bindings, content =
      List.fold_map data.content ~init:new_bindings ~f:(fun new_bindings idx ->
          let decl = get_global_decl idx in
          match decl with
          | Pat ({ binding = { old; fresh = _ }; env; _ } as x) ->
            let env = refresh_env new_bindings env in
            let new_binding = { old; fresh = fresh_pattern old path } in
            let nb_pat =
              List.map ~f:(fun (old, fresh) -> [], { old; fresh })
              @@ Core.Map.to_alist
              @@ pat_to_var_bindings new_binding
            in
            let new_idx = new_global_decl (Pat { x with binding = new_binding; env }) in
            dedup_new_bindings @@ nb_pat @ new_bindings, new_idx
          | Exp ({ binding = { old; fresh = _ }; env; _ } as x) ->
            let env = refresh_env new_bindings env in
            let new_binding = { old; fresh = fresh_var old path } in
            let new_idx = new_global_decl (Exp { x with binding = new_binding; env }) in
            dedup_new_bindings @@ (([], new_binding) :: new_bindings), new_idx
          | Mod { name; in_ } ->
            let inner_bindings, in_ =
              refresh ~new_bindings in_ (extend_debug_path path name)
            in
            let mod_bindings =
              List.map ~f:(fun (mod_path, b) -> name :: mod_path, b) inner_bindings
            in
            let new_idx = new_global_decl (Mod { name; in_ }) in
            dedup_new_bindings @@ mod_bindings, new_idx
          | Incl x ->
            let incl_bindings, x =
              refresh ~new_bindings { content = x; env = empty_env } path
            in
            let new_idx = new_global_decl (Incl x.content) in
            dedup_new_bindings @@ incl_bindings, new_idx)
    in
    let env = refresh_env new_bindings data.env in
    new_bindings, { content; env }


  let refresh data path = snd @@ refresh ~new_bindings:[] data path

  let decls_to_env : int list -> env =
   fun decls ->
    let rec loop decls acc =
      List.fold_left decls ~init:acc ~f:(fun acc decl ->
          match get_global_decl decl with
          | Pat { binding; _ } ->
            { acc with
              exp =
                Core.Map.merge_skewed
                  (pat_to_var_bindings binding)
                  acc.exp
                  ~combine:(fun ~key:_ v1 _v2 -> v1)
            }
          | Exp { binding; _ } ->
            { acc with exp = Core.Map.set acc.exp ~key:binding.old ~data:binding.fresh }
          | Mod mod_binding -> { acc with mod_ = mod_binding :: acc.mod_ }
          | Incl decls -> loop decls acc)
    in
    loop decls empty_env


  let include_ : t -> int list -> t =
   fun data to_include ->
    let env = decls_to_env to_include in
    { env = merge_env env data.env
    ; content = new_global_decl (Incl to_include) :: data.content
    }
end

let rec aggregate_scope ~(raise : _ Trace.raise)
    : Data.t -> leaf:O.expression -> O.expression
  =
 fun { content; _ } ~leaf ->
  let rec f : O.expression -> int -> O.expression =
   fun acc_exp d ->
    match Data.get_global_decl d with
    | Pat { binding = { old = _; fresh }; item; env; attr; loc } ->
      let item = compile_expression ~raise env item in
      O.e_a_let_in ~loc fresh item acc_exp attr
    | Exp { binding = { old = _; fresh }; item; env; attr; loc } ->
      let item = compile_expression ~raise env item in
      let binder =
        O.Pattern.var ~loc:(Value_var.get_location fresh)
        @@ Binder.make fresh item.type_expression
      in
      O.e_a_let_in ~loc binder item acc_exp attr
    | Mod { in_; _ } -> aggregate_scope ~raise in_ ~leaf:acc_exp
    | Incl content -> List.fold_left content ~f ~init:acc_exp
  in
  List.fold_left content ~f ~init:leaf


and build_context ~(raise : _ Trace.raise) : Data.t -> O.context =
 fun { content; _ } ->
  let rec f : int -> O.declaration list =
   fun d ->
    match Data.get_global_decl d with
    | Pat { binding = { old = _; fresh }; item; env; attr; loc } ->
      let item = compile_expression ~raise env item in
      [ Location.wrap ~loc (O.D_irrefutable_match { pattern = fresh; expr = item; attr })
      ]
    | Exp { binding = { old = _; fresh }; item; env; attr; loc } ->
      let item = compile_expression ~raise env item in
      let binder = Binder.make fresh item.type_expression in
      [ Location.wrap ~loc (O.D_value { binder; expr = item; attr }) ]
    | Mod { in_; _ } -> build_context ~raise in_
    | Incl content -> List.bind ~f @@ List.rev content
  in
  List.bind ~f @@ List.rev content


and compile ~(raise : _ Trace.raise) : Data.t -> I.expression -> I.program -> O.program =
 fun data hole program ->
  let data = compile_declarations ~raise data [] program.pr_module in
  let hole = compile_expression ~raise data.env hole in
  build_context ~raise data, hole


and compile_declarations ~(raise : _ Trace.raise)
    : Data.t -> Data.path -> I.module_ -> Data.t
  =
 fun init_scope path lst ->
  let f : Data.t -> I.declaration -> Data.t =
   fun acc_scope decl ->
    match decl.wrap_content with
    | I.D_type _ -> acc_scope
    | I.D_irrefutable_match { pattern; expr; attr } ->
      let pat =
        let fresh = Data.fresh_pattern pattern path in
        (Data.
           { binding = { old = pattern; fresh }
           ; item = expr
           ; env = acc_scope.env
           ; attr
           ; loc = decl.location
           }
          : Data.pat_)
      in
      Data.add_exp_pat acc_scope pat
    | I.D_value { binder; expr; attr } ->
      let exp =
        let fresh = Data.fresh_var binder.var path in
        (Data.
           { binding = { old = binder.var; fresh }
           ; item = expr
           ; env = acc_scope.env
           ; attr
           ; loc = decl.location
           }
          : Data.exp_)
      in
      Data.add_exp acc_scope exp
    | I.D_module { module_binder; module_; module_attr = _ } ->
      let rhs_glob =
        compile_module_expr
          ~raise
          acc_scope.env
          (Data.extend_debug_path path module_binder)
          module_
      in
      Data.add_module acc_scope module_binder rhs_glob
    | I.D_module_include module_ ->
      let data =
        compile_module_expr
          ~raise
          ~copy_content:true
          acc_scope.env
          ("INCL" :: path)
          module_
      in
      Data.include_ acc_scope data.content
    | I.D_signature _ -> acc_scope
    | I.D_import _ -> failwith "import declarations cannot be aggregated"
  in
  List.fold lst ~init:init_scope ~f


(*
  [copy_content] let you control if the module content should be entirely copied
  as a new set of bindings, or if we should just make reference to it
*)
and compile_module_expr ~(raise : _ Trace.raise) ?(copy_content = false)
    : Data.env -> Data.path -> I.module_expr -> Data.t
  =
 fun env path mexpr ->
  match mexpr.module_content with
  | M_struct prg -> compile_declarations ~raise { env; content = [] } path prg
  | M_variable v ->
    let res = Data.resolve_path env [ v ] in
    if copy_content then Data.refresh res path else { res with content = [] }
  | M_module_path m_path ->
    let res = Data.resolve_path env (Nonempty_list.to_list m_path) in
    if copy_content then Data.refresh res path else { res with content = [] }


and compile_expression ~(raise : _ Trace.raise)
    : Data.env -> ?debug_path:Data.path -> I.expression -> O.expression
  =
 fun env ?(debug_path = []) expr ->
  let self ?(env = env) ?(debug_path = debug_path) =
    compile_expression ~raise ~debug_path env
  in
  let return expression_content : O.expression =
    let type_expression = expr.type_expression in
    { expression_content; type_expression; location = expr.location }
  in
  match expr.expression_content with
  | I.E_error _ -> raise.error @@ cannot_compile_erroneous_expression expr expr.location
  (* resolving variable names *)
  | I.E_contract _ -> assert false (* reduced in self_ast_typed *)
  | I.E_variable v ->
    let v = Data.resolve_variable env v in
    return (O.E_variable v)
  | I.E_module_accessor { module_path; element } ->
    let v = Data.resolve_variable_in_path env module_path element in
    return (O.E_variable v)
  (* bounding expressions *)
  | I.E_matching { matchee; cases } ->
    let cases =
      List.map
        ~f:(fun { pattern; body } ->
          let env =
            List.fold
              ~init:env
              ~f:(fun env binder -> Data.rm_exp env (Binder.get_var binder))
              (I.Pattern.binders pattern)
          in
          O.Match_expr.{ pattern; body = self ~env body })
        cases
    in
    return (O.E_matching { matchee = self matchee; cases })
  | I.E_lambda { binder; output_type; result } ->
    let env = Data.rm_exp env (Param.get_var binder) in
    return (O.E_lambda { binder; output_type; result = self ~env result })
  | I.E_for { binder; start; final; incr; f_body } ->
    let env = Data.rm_exp env binder in
    return
      (O.E_for
         { binder
         ; start = self start
         ; final = self final
         ; incr = self incr
         ; f_body = self ~env f_body
         })
  | I.E_recursive
      { fun_name; fun_type; lambda = { binder; result; output_type }; force_lambdarec } ->
    let env = Data.rm_exp env (Param.get_var binder) in
    let env = Data.rm_exp env fun_name in
    let result = self ~env result in
    return
    @@ O.E_recursive
         { fun_name; fun_type; lambda = { binder; result; output_type }; force_lambdarec }
  | I.E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    let env =
      List.fold
        ~init:env
        ~f:(fun env binder -> Data.rm_exp env (Binder.get_var binder))
        (I.Pattern.binders let_binder)
    in
    let rhs = self rhs in
    let let_result = self ~env let_result in
    return @@ O.E_let_mut_in { let_binder; rhs; let_result; attributes }
  | I.E_let_in { let_binder; rhs; let_result; attributes } ->
    let env =
      List.fold
        ~init:env
        ~f:(fun env binder -> Data.rm_exp env (Binder.get_var binder))
        (I.Pattern.binders let_binder)
    in
    let let_result = self ~env let_result in
    return @@ O.E_let_in { let_binder; rhs = self rhs; let_result; attributes }
  | I.E_for_each { fe_binder = b, b_opt; collection; collection_type; fe_body } ->
    let env = Data.rm_exp env b in
    let env = Option.value_map b_opt ~default:env ~f:(fun b -> Data.rm_exp env b) in
    return
      (O.E_for_each
         { fe_binder = b, b_opt
         ; collection = self collection
         ; collection_type
         ; fe_body = self ~env fe_body
         })
  (* recursively call aggregation *)
  | I.E_mod_in { module_binder; rhs; let_result } ->
    let data =
      let rhs_scope =
        compile_module_expr
          ~raise
          env
          ("LOCAL#in" :: Data.extend_debug_path debug_path module_binder)
          rhs
      in
      Data.add_module { env; content = [] } module_binder rhs_scope
    in
    let x = Data.resolve_path data.env [ module_binder ] in
    aggregate_scope ~raise x ~leaf:(self ~env:data.env let_result)
  (* trivials *)
  | I.E_literal l -> return (O.E_literal l)
  | I.E_raw_code x -> return (O.E_raw_code (Raw_code.map self x))
  | I.E_accessor x -> return (O.E_accessor (I.Accessor.map self x))
  | I.E_record m -> return (O.E_record (Record.map m ~f:self))
  | I.E_update x -> return (O.E_update (I.Update.map self x))
  | I.E_constructor x -> return (O.E_constructor (Constructor.map self x))
  | I.E_application x -> return (O.E_application (Application.map self x))
  | I.E_type_abstraction x -> return (O.E_type_abstraction (Type_abs.map self x))
  | I.E_type_inst { forall; type_ } ->
    return (O.E_type_inst { forall = self forall; type_ })
  | I.E_constant x -> return (O.E_constant (Constant.map self x))
  | I.E_coerce x -> return (O.E_coerce (Ascription.map self Fn.id x))
  | I.E_assign x -> return (O.E_assign (Assign.map self Fn.id x))
  | I.E_deref x -> return (O.E_deref x)
  | I.E_while x -> return (O.E_while (While_loop.map self x))
  | I.E_union_injected _ | I.E_union_match _ | I.E_union_use _ ->
    Ast_aggregated.impossible_because_no_union_in_ast_aggregated ()
