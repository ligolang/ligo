(*
This algorithm flatten the module by prefixing the declared identifier by the list of mudule names in which it is declared.
ie :

module Toto = struct
  module Tata = struct
    module Titi = struct
      let a = 42
    end
  end
end

let a = Toto.Tata.Titi.a

becomes

let <Toto><Tata><Titi>a = 42
let a = <Toto><Tata><Titi>a

There is two difficulties :
- First in case of shadowing :

module Toto = struct
  let a = 1
end
module Toto = struct
  let a = 42
  let b = Toto.a (* 1 *)
end

would become

let <Toto>a = 1
let <Toto>a = 42
let <Toto>b = <Toto>a (* 42 *)

which requires to deplucate module name to avoid shadowing.

let <Toto#1>a = 1
let <Toto#2>a = 42
let <Toto#2>b = <Toto#1>a (* 1 *)

- Second in case of nested module :

let a = 1

module Toto = struct
  let a = 42
  module Tata = struct
    let b = a
  end
end

would become :

let a = 1
let <Toto>a = 42
let <Toto><Tata>b = a (* 1 *)

which is incorrect. -> we need to store the path corresponding to the variable in the scope to retrieve it and do:

let a = 1
let <Toto>a = 42
let <Toto><Tata>b = <Toto>a (* 42 *)

which is correct.

Furthermore for module alias :

module Toto = struct
  module Tata = struct
    let a = 42
  end
end

module Titi = Toto.Tata

let a = Titi.a

Two possibilities :
  - replace all next occurences of Titi by Toto
  - copy all declarations of Toto for Titi

we choose the later one. ie:

let <Toto><Tata>a = 42
let <Titi>a = <Toto><Tata>a
let a = <Titi>a

which requires to store the declarations relative to a module

however we made a preprocessing self-pass to resolve the aliasing, which can be truned on and off.

The algorithm does as follow.
It parse the file and for each declaration it adds the current Path as a prefix.
When we encounter a variable, we get the Path from the scope.

Then the algorithm transfrom the declarations into a chain of let-ins.
Transforming the program (a list of declarations) into a function that takes an
expression and place it at the end of the chain.
*)

module I = Ast_typed
module O = Ast_aggregated
open Ligo_prim

module PathVar (M: Map.OrderedType) = struct
  type t = (Module_var.t list * M.t) [@deriving compare]
  let compare (mvla,va) (mvlb,vb) =
    let c = List.compare Module_var.compare mvla mvlb in
    if c = 0 then M.compare va vb else c
end
module PathVarMap = Map.Make(PathVar(Value_var))

(* Define the notion of Path i.e the list of nested module in which we are.
   Variables are stored with the more nested module at the top *)
module Path : sig
  type t = Module_var.t list
  val empty       : t
  val equal       : t -> t -> bool
  val add_to_path : t -> Module_var.t -> t
  val get_from_module_path : Module_var.t List.t -> t
  val append : t -> t -> t
  val pp          : Format.formatter -> t -> unit
end = struct
  type t = Module_var.t list
  let empty = []
  let equal p1 p2 = List.equal (Module_var.equal) p1 p2
  let add_to_path (p : t) s = s :: p
  let get_from_module_path lst = List.rev lst
  let append a b = a @ b
  let pp ppf path =
    Format.fprintf ppf "[%a]\n%!" (PP_helpers.list_sep_d Module_var.pp) path
end

(* Store LUTs of the path of each identifier, plus the list of declaration in a module *)
(* Note: we could have one module per function but it felt like duplication and more error prone *)
module Scope : sig
  type t
  type decl =
    Value of O.type_expression Binder.t * O.type_expression * O.ValueAttr.t
  | Module of Module_var.t
  val empty : t
  val pp    : Format.formatter -> t -> unit
  val find_value : t -> Value_var.t -> Path.t
  val find_type_ : t -> Type_var.t -> Path.t
  val find_module : t -> Module_var.t -> Path.t * t
  val push_value : t -> O.type_expression Binder.t -> O.type_expression -> O.ValueAttr.t -> Path.t -> t
  val push_func_or_case_binder : t -> Value_var.t -> t
  val push_type_ : t -> Type_var.t -> Path.t -> t
  val push_local_type : t -> Type_var.t -> t
  val push_module : t -> Module_var.t -> Path.t -> t -> t
  val add_path_to_var : t -> Path.t -> Value_var.t -> t * Value_var.t
  val get_declarations : t -> decl list
  val clean_declarations : t -> t
end = struct
  module ValueVMap = Map.Make(Value_var)
  module TypeVMap  = Map.Make(Type_var)
  module ModuleVMap = Map.Make(Module_var)
  type t = {
    value : Path.t ValueVMap.t;
    type_ : Path.t TypeVMap.t ;
    module_ : (Path.t * t) ModuleVMap.t ;
    name_map : Value_var.t PathVarMap.t;
    decl_list : decl list}
  and decl =
    Value of O.type_expression Binder.t * O.type_expression * O.ValueAttr.t
  | Module of Module_var.t
  let empty = { value = ValueVMap.empty; type_ = TypeVMap.empty; module_ = ModuleVMap.empty ; name_map = PathVarMap.empty; decl_list = [] }
  let rec pp ppf scope =
    Format.fprintf ppf "{value : %a; type_ : %a; module_ : %a; name_map :%a}\n%!"
      (PP_helpers.list_sep_d (fun ppf (a,b) -> Format.fprintf ppf "(%a -> %a)" Value_var.pp a Path.pp b)) (ValueVMap.to_kv_list scope.value)
      (PP_helpers.list_sep_d (fun ppf (a,b) -> Format.fprintf ppf "(%a -> %a)" Type_var.pp a Path.pp b)) (TypeVMap.to_kv_list scope.type_)
      (PP_helpers.list_sep_d (fun ppf (a,(b,s)) -> Format.fprintf ppf "(%a -> (%a,%a)" Module_var.pp a Path.pp b pp s)) (ModuleVMap.to_kv_list scope.module_)
      (PP_helpers.list_sep_d (fun ppf ((a,b),s) -> Format.fprintf ppf "(%a,%a) -> %a)" Path.pp a Value_var.pp b Value_var.pp s)) (PathVarMap.to_kv_list scope.name_map)

  let find_value scope v =
    Option.value ~default:Path.empty  (ValueVMap.find_opt v scope.value)
  let find_type_ scope t =
    Option.value ~default:Path.empty  (TypeVMap.find_opt t scope.type_)
  let find_module scope m =
    Option.value ~default:(Path.empty,empty)  (ModuleVMap.find_opt m scope.module_)
  let push_value scope (b : _ Binder.t) ty attr path =
    let value = ValueVMap.add (Binder.get_var b) path scope.value in
    let decl_list = Value (b, ty, attr) :: scope.decl_list in
    { scope with value ; decl_list}
  let push_func_or_case_binder scope (v : Value_var.t) =
    let value = ValueVMap.add v Path.empty scope.value in
    { scope with value }
  let push_type_ scope t path =
    let type_ = TypeVMap.add t path scope.type_ in
    { scope with type_ }
  let push_local_type scope (v : Type_var.t) =
    let type_ = TypeVMap.add v Path.empty scope.type_ in
    { scope with type_ }
  let push_module scope m path mod_scope =
    let module_ = ModuleVMap.add m (path,mod_scope) scope.module_ in
    let name_map = mod_scope.name_map in
    let decl_list = Module (m) :: scope.decl_list in
    { scope with module_ ; decl_list ; name_map = PathVarMap.union (fun _ f _ -> Some f) name_map scope.name_map }
  let get_declarations scope = scope.decl_list
  let clean_declarations scope = { scope with decl_list = [] }

  let add_path_to_var scope p v =
    if List.is_empty p then scope,v else
    match PathVarMap.find_opt (p,v) scope.name_map with
      Some (v) -> scope,v
    | None ->
        let var = Value_var.fresh_like v in
        {scope with name_map=PathVarMap.add (p,v) var scope.name_map},var
end


let compile_value_attr : I.ValueAttr.t -> O.ValueAttr.t =
  fun {inline;no_mutation;view;public;hidden;thunk} -> {inline;no_mutation;view;public;hidden;thunk}

let rec compile_type_expression ~raise path scope (type_expression : I.type_expression) : O.type_expression =
  let self ?(path=path) ?(scope=scope) = compile_type_expression ~raise path scope in
  let return type_content = O.{type_content;location=type_expression.location;orig_var=type_expression.orig_var;source_type=Some type_expression } in
  match type_expression.type_content with
    T_variable type_variable ->
    (* Look up the path in the scope *)
    return @@ T_variable type_variable
  | T_constant {language;injection;parameters} ->
    let parameters = List.map ~f:self parameters in
    return @@ T_constant {language;injection;parameters}
  | T_sum      {fields;layout} ->
    let fields = Record.map (Rows.map_row_element_mini_c self) fields in
    return @@ T_sum      {fields;layout}
  | T_record   {fields;layout} ->
    let fields = Record.map (Rows.map_row_element_mini_c self) fields in
    return @@ T_record   {fields;layout}
  | T_arrow    {type1;type2} ->
    let type1 = self type1 in
    let type2 = self type2 in
    return @@ T_arrow  {type1;type2}
  | T_singleton literal ->
    return @@ T_singleton literal
  | T_abstraction {ty_binder; kind; type_} ->
    let type_ = self type_ in
    return @@ T_for_all {ty_binder; kind; type_}
  | T_for_all {ty_binder; kind; type_} ->
    let type_ = self type_ in
    return @@ T_for_all {ty_binder; kind; type_}

let rec compile_expression ~raise path scope (expr : I.expression) =
  let self ?(path = path) ?(scope=scope) = compile_expression ~raise path scope in
  let self_type ?(path = path) ?(scope=scope) = compile_type_expression ~raise path scope in
  let self_cases ?(path = path) ?(scope=scope) = compile_cases ~raise ~loc:expr.location path scope in
  let return expression_content =
    let type_expression = self_type expr.type_expression in
    O.{expression_content;location=expr.location;type_expression}
  in
  match expr.expression_content with
    E_literal literal -> return @@ E_literal literal
  | E_constant {cons_name;arguments} ->
    let arguments = List.map ~f:self arguments in
    return @@ E_constant {cons_name; arguments}
  | E_variable expression_variable ->
    (* Look up the path in the scope *)
    let path = Scope.find_value scope expression_variable in
    let _,expression_variable = Scope.add_path_to_var scope path expression_variable in
    return @@ E_variable expression_variable
  | E_application {lamb;args} ->
    let lamb = self lamb in
    let args = self args in
    return @@ E_application {lamb;args}
  | E_lambda {binder;output_type;result} ->
    let binder = Param.map self_type binder in
    let scope = Scope.push_func_or_case_binder scope @@ Param.get_var binder in
    let output_type = self_type output_type in
    let result = self ~scope result in
    return @@ E_lambda {binder;output_type;result}
  | E_type_abstraction {type_binder;result} ->
    let scope = Scope.push_local_type scope type_binder in
    let result = self ~scope result in
    return @@ E_type_abstraction {type_binder;result}
  | E_recursive {fun_name;fun_type;lambda={binder;output_type;result}} ->
    let fun_type = self_type fun_type in
    let binder   = Param.map self_type binder in
    let scope = Scope.push_func_or_case_binder scope @@ Param.get_var binder in
    let scope = Scope.push_func_or_case_binder scope fun_name in
    let output_type = self_type output_type in
    let result   = self ~scope result in
    return @@ E_recursive {fun_name;fun_type;lambda={binder;output_type;result}}
  | E_let_in {let_binder;rhs;let_result;attr} ->
    let let_binder   = Binder.map self_type let_binder in
    let rhs = self rhs in
    let attr = compile_value_attr attr in
    let scope = Scope.push_value scope let_binder rhs.type_expression attr Path.empty  in
    let let_result = self ~scope let_result in
    return @@ E_let_in {let_binder;rhs;let_result;attr}
  | E_raw_code {language;code} ->
    let code = self code in
    return @@ E_raw_code {language;code}
  | E_type_inst {forall;type_} ->
    let forall = self forall in
    let type_  = self_type type_ in
    return @@ E_type_inst {forall;type_}
  (* Variant *)
  | E_constructor {constructor;element} ->
    let element = self element in
    return @@ E_constructor {constructor;element}
  | E_matching {matchee;cases} ->
    let matchee = self matchee in
    let cases   = self_cases matchee cases in
    return @@ cases
  (* Record *)
  | E_record record ->
    let record = Record.map self record in
    return @@ E_record record
  | E_accessor {struct_;path} ->
    let struct_ = self struct_ in
    return @@ E_accessor {struct_;path}
  | E_update   {struct_;path;update} ->
    let struct_ = self struct_ in
    let update = self update in
    return @@ E_update   {struct_;path;update}
  | E_mod_in  {module_binder; rhs; let_result} ->
    let path' = Path.add_to_path path @@ module_binder in
    let mod_scope,rhs = compile_module_expr ~raise path' scope rhs in
    let scope    = Scope.push_module scope module_binder path' mod_scope in
    let let_result = self ~scope let_result in
    O.context_apply rhs let_result
  | E_module_accessor {module_path;element} ->
    (match module_path with
    | [] -> failwith "Corner case : E_module_accessor with empty module_path"
    | hd :: tl ->
    (* Look up the path in the scope for the first module and add the following ones *)
    let path,_scope = Scope.find_module scope hd in
    let path2   = Path.get_from_module_path tl in
    let path    = Path.append path2 path in
    let _,element = Scope.add_path_to_var scope path element in
    return @@ E_variable element)
  | E_let_mut_in { let_binder ; rhs ; let_result ; attr } ->
    let let_binder   = Binder.map self_type let_binder in
    let rhs = self rhs in
    let attr = compile_value_attr attr in
    let scope = Scope.push_value scope let_binder rhs.type_expression attr Path.empty  in
    let let_result = self ~scope let_result in
    return (E_let_mut_in { let_binder ; rhs ; let_result ; attr })
  | E_deref var -> 
    let path = Scope.find_value scope var in
    let _,expression_variable = Scope.add_path_to_var scope path var in
    return @@ E_deref expression_variable
  | E_assign {binder;expression} ->
    let binder = Binder.map self_type binder in
    let expression = self expression in
    return @@ E_assign {binder;expression}
  | E_for { binder; start; final; incr; f_body } ->
    let start = self start
    and final = self final 
    and incr = self incr in
    let scope = Scope.push_func_or_case_binder scope binder in
    let f_body = self ~scope f_body in
    return @@ E_for { binder; start; final; incr; f_body }
  | E_for_each { fe_binder = binder1, binder2 as fe_binder; collection; collection_type; fe_body } ->
    let collection = self collection in
    let scope = 
      List.fold_left (binder1 :: Option.to_list binder2) ~init:scope ~f:Scope.push_func_or_case_binder
    in
    let fe_body = self ~scope fe_body in
    return @@ E_for_each { fe_binder; collection; collection_type; fe_body }
  | E_while while_loop ->
    let while_loop = While_loop.map self while_loop in
    return @@ E_while while_loop

and compile_cases ~raise ~loc path scope matchee cases : O.expression_content =
  let matchee_type = matchee.type_expression in
  let eqs = List.map cases 
    ~f:(fun {pattern ; body} -> 
        let pattern = I.Pattern.map (compile_type_expression ~raise path scope) pattern in
        let body = compile_expression ~raise path scope body in
        pattern, matchee_type, body) in
  match matchee.expression_content with
  | E_variable var ->
    let match_expr =
      Pattern_matching.compile_matching ~raise ~err_loc:loc var eqs
    in
    match_expr.expression_content
  | _ ->
    let var = Value_var.fresh ~loc ~name:"match_" () in
    let match_expr =
      Pattern_matching.compile_matching ~raise ~err_loc:loc var eqs
    in
    O.E_let_in
      { let_binder = Binder.make var matchee.type_expression
      ; rhs = matchee
      ; let_result = { match_expr with location = loc }
      ; attr =
          { inline = false
          ; no_mutation = false
          ; public = true
          ; view = false
          ; hidden = false
          ; thunk = false
          }
      }

and compile_declaration ~raise ~(super_attr : O.ModuleAttr.t) path scope (d : I.declaration) =
  match Location.unwrap d with
    D_value {binder;expr;attr} ->
      let attr = {attr with hidden = attr.hidden || super_attr.hidden; public = attr.public && super_attr.public} in
      let expr = compile_expression ~raise path scope expr in
      let attr = compile_value_attr attr in
      let binder = Binder.map (fun _ -> expr.type_expression) binder in
      let scope  = Scope.push_value scope binder expr.type_expression attr path in
      let scope,var = Scope.add_path_to_var scope path @@ Binder.get_var binder in
      let binder = Binder.set_var binder var in
      scope, O.context_decl binder expr attr
  | D_type _ ->
      scope, O.context_id
  | D_module {module_binder;module_;module_attr} ->
      let module_attr : O.ModuleAttr.t = {hidden = module_attr.hidden || super_attr.hidden; public = module_attr.public && super_attr.public} in
      let path' = Path.add_to_path path module_binder in
      let mod_scope,decl_list = compile_module_expr ~raise ~module_attr path' scope module_ in
      let scope   = Scope.push_module scope module_binder path' mod_scope in
      scope, decl_list

and compile_declaration_list ~raise ~super_attr (path : Path.t) scope (program : I.program) : (Scope.t) * (O.context) =
  let scope, current = List.fold_map ~init:scope ~f:(compile_declaration ~raise ~super_attr path) program in
  let current = List.fold_left ~f:O.context_append ~init:O.context_id current in
  scope, current

and compile_decl ~raise path scope (d : I.decl) =
  compile_declaration ~raise path scope d

and compile_module ~raise ~super_attr (path : Path.t) scope (program : I.module_) : (Scope.t) * (O.context) =
  let scope, current = List.fold_map ~init:scope ~f:(compile_decl ~raise ~super_attr path) program in
  let current = List.fold_left ~f:O.context_append ~init:O.context_id current in
  scope, current

and compile_module_expr ~raise ?(module_attr = {public=true;hidden=false}) : Path.t -> Scope.t -> I.module_expr -> (Scope.t) * (O.context) =
  fun path scope mexpr ->
    let rec get_declarations_from_scope scope new_path old_path =
      let dcls = Scope.get_declarations scope in
      let module_ = List.fold_left ~init:O.context_id dcls ~f:(fun f dcl ->
        match dcl with
          Value (binder,ty,attr) ->
            let variable = O.e_a_variable (snd @@ Scope.add_path_to_var scope old_path @@ Binder.get_var binder) ty in
            let _,var = Scope.add_path_to_var scope new_path @@ Binder.get_var binder in
            O.(context_append (context_decl (Binder.set_var binder var) variable attr) f)
        | Module (var) ->
          let _,scope  = Scope.find_module scope var in
          let old_path = Path.add_to_path old_path var in
          let new_path = Path.add_to_path new_path var in
          let module_ = get_declarations_from_scope scope new_path old_path in
          O.context_append module_ f
      ) in module_
    in
    let super_attr : O.ModuleAttr.t = {public=module_attr.public;hidden=module_attr.hidden} in
    match mexpr.wrap_content with
    | M_struct m -> (
      (* Keep the scope of identifiers be start with an empty list of declaration (corresponding to this module) *)
      let scope = Scope.clean_declarations scope in
      let scope,module_ = compile_module ~raise ~super_attr path scope m in
      scope,module_
    )
    | M_variable v -> (
      let path_b,scope = Scope.find_module scope v in
      let module_ = get_declarations_from_scope scope path path_b in
      scope,module_
    )
    | M_module_path (hd,tl) -> (
      let old_path,scope = Scope.find_module scope hd in
      let old_path,scope = List.fold_left ~f:(fun (_,s) -> Scope.find_module s) ~init:(old_path,scope) tl in
      let module_ = get_declarations_from_scope scope path old_path in
      scope,module_
    )

let preprocess_program program =
  let scope,program = Deduplicate_module_binders.program program in
  let aliases,program = Resolve_module_aliases.program program in
  scope,aliases,program

let preprocess_expression ?scope ?aliases e =
  let e = Deduplicate_module_binders.expression ?scope e in
  let e = Resolve_module_aliases.expression ?aliases e in
  e

let compile_program ~raise : I.program -> Scope.t * Deduplicate_module_binders.Scope.t * Resolve_module_aliases.Aliases.t * O.context =
  fun program ->
    let deduplicate_scope,aliases,program = preprocess_program program in
    let scope, decls = compile_declaration_list ~raise ~super_attr:{public=true;hidden=false} Path.empty Scope.empty program in
    scope, deduplicate_scope, aliases, decls

let compile_expression ~raise ?(scope = Scope.empty) ?deduplicate_scope ?aliases : I.expression -> O.expression =
  fun e ->
    let e = preprocess_expression ?scope:deduplicate_scope ?aliases e in
    let e = compile_expression ~raise Path.empty scope e in
    e
