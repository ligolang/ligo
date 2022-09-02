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
  type t = (ModuleVar.t list * M.t) [@deriving compare]
  let compare (mvla,va) (mvlb,vb) =
    let c = List.compare ModuleVar.compare mvla mvlb in
    if c = 0 then M.compare va vb else c
end
module PathVarMap = Map.Make(PathVar(ValueVar))

(* Define the notion of Path i.e the list of nested module in which we are.
   Variables are stored with the more nested module at the top *)
module Path : sig
  type t = ModuleVar.t list
  val empty       : t
  val equal       : t -> t -> bool
  val add_to_path : t -> ModuleVar.t -> t
  val get_from_module_path : ModuleVar.t List.t -> t
  val append : t -> t -> t
  val pp          : Format.formatter -> t -> unit
end = struct
  type t = ModuleVar.t list
  let empty = []
  let equal p1 p2 = List.equal (ModuleVar.equal) p1 p2
  let add_to_path (p : t) s = s :: p
  let get_from_module_path lst = List.rev lst
  let append a b = a @ b
  let pp ppf path =
    Format.fprintf ppf "[%a]\n%!" (PP_helpers.list_sep_d ModuleVar.pp) path
end

(* Store LUTs of the path of each identifier, plus the list of declaration in a module *)
(* Note: we could have one module per function but it felt like duplication and more error prone *)
module Scope : sig
  type t
  type decl =
    Value of O.type_expression Binder.t * O.type_expression * O.ValueAttr.t
  | Module of ModuleVar.t
  val empty : t
  val pp    : Format.formatter -> t -> unit
  val find_value : t -> ValueVar.t -> Path.t
  val find_type_ : t -> TypeVar.t -> Path.t
  val find_module : t -> ModuleVar.t -> Path.t * t
  val push_value : t -> O.type_expression Binder.t -> O.type_expression -> O.ValueAttr.t -> Path.t -> t
  val remove_value : t -> ValueVar.t -> t
  val push_type_ : t -> TypeVar.t -> Path.t -> t
  val remove_type_ : t -> TypeVar.t -> t
  val push_module : t -> ModuleVar.t -> Path.t -> t -> t
  val add_path_to_var : t -> Path.t -> ValueVar.t -> t * ValueVar.t
  val get_declarations : t -> decl list
  val clean_declarations : t -> t
end = struct
  module ValueVMap = Map.Make(ValueVar)
  module TypeVMap  = Map.Make(TypeVar)
  module ModuleVMap = Map.Make(ModuleVar)
  type t = {
    value : Path.t ValueVMap.t;
    type_ : Path.t TypeVMap.t ;
    module_ : (Path.t * t) ModuleVMap.t ;
    name_map : ValueVar.t PathVarMap.t;
    decl_list : decl list}
  and decl =
    Value of O.type_expression Binder.t * O.type_expression * O.ValueAttr.t
  | Module of ModuleVar.t
  let empty = { value = ValueVMap.empty; type_ = TypeVMap.empty; module_ = ModuleVMap.empty ; name_map = PathVarMap.empty; decl_list = [] }
  let rec pp ppf scope =
    Format.fprintf ppf "{value : %a; type_ : %a; module_ : %a; name_map :%a}\n%!"
      (PP_helpers.list_sep_d (fun ppf (a,b) -> Format.fprintf ppf "(%a -> %a)" ValueVar.pp a Path.pp b)) (ValueVMap.to_kv_list scope.value)
      (PP_helpers.list_sep_d (fun ppf (a,b) -> Format.fprintf ppf "(%a -> %a)" TypeVar.pp a Path.pp b)) (TypeVMap.to_kv_list scope.type_)
      (PP_helpers.list_sep_d (fun ppf (a,(b,s)) -> Format.fprintf ppf "(%a -> (%a,%a)" ModuleVar.pp a Path.pp b pp s)) (ModuleVMap.to_kv_list scope.module_)
      (PP_helpers.list_sep_d (fun ppf ((a,b),s) -> Format.fprintf ppf "(%a,%a) -> %a)" Path.pp a ValueVar.pp b ValueVar.pp s)) (PathVarMap.to_kv_list scope.name_map)

  let find_value scope v =
    Option.value ~default:Path.empty  (ValueVMap.find_opt v scope.value)
  let find_type_ scope t =
    Option.value ~default:Path.empty  (TypeVMap.find_opt t scope.type_)
  let find_module scope m =
    Option.value ~default:(Path.empty,empty)  (ModuleVMap.find_opt m scope.module_)
  let push_value scope (v : _ Binder.t) ty attr path =
    let value = ValueVMap.add v.var path scope.value in
    let decl_list = Value (v, ty, attr) :: scope.decl_list in
    { scope with value ; decl_list}
  let remove_value scope (v : ValueVar.t) =
    let value = ValueVMap.remove v scope.value in
    { scope with value }
  let push_type_ scope t path =
    let type_ = TypeVMap.add t path scope.type_ in
    { scope with type_ }
  let remove_type_ scope (v : TypeVar.t) =
    let type_ = TypeVMap.remove v scope.type_ in
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
        let var = ValueVar.fresh_like v in
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
  let self_cases ?(path = path) ?(scope=scope) = compile_cases ~raise path scope in
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
    let binder = Binder.map self_type binder in
    let scope = Scope.remove_value scope binder.var in
    let output_type = self_type output_type in
    let result = self ~scope result in
    return @@ E_lambda {binder;output_type;result}
  | E_type_abstraction {type_binder;result} ->
    let scope = Scope.remove_type_ scope type_binder in
    let result = self ~scope result in
    return @@ E_type_abstraction {type_binder;result}
  | E_recursive {fun_name;fun_type;lambda={binder;output_type;result}} ->
    let fun_type = self_type fun_type in
    let binder   = Binder.map self_type binder in
    let scope = Scope.remove_value scope binder.var in
    let scope = Scope.remove_value scope fun_name in
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
    let cases   = self_cases cases in
    return @@ E_matching {matchee;cases}
  (* Record *)
  | E_record record ->
    let record = Record.map self record in
    return @@ E_record record
  | E_accessor {record;path} ->
    let record = self record in
    return @@ E_accessor {record;path}
  | E_update   {record;path;update} ->
    let record = self record in
    let update = self update in
    return @@ E_update   {record;path;update}
  | E_mod_in  {module_binder; rhs; let_result} ->
    let path' = Path.add_to_path path @@ module_binder in
    let mod_scope,rhs = compile_module_expr ~raise path' scope rhs in
    let scope    = Scope.push_module scope module_binder path' mod_scope in
    let let_result = self ~scope let_result in
    rhs let_result
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
  | E_assign {binder;expression} ->
    let binder = Binder.map self_type binder in
    let expression = self expression in
    return @@ E_assign {binder;expression}

and compile_cases ~raise path scope cases : O.matching_expr =
  match cases with
    Match_variant {cases;tv} ->
    let cases = List.map cases ~f:(fun I.{constructor;pattern;body} ->
      let scope = Scope.remove_value scope pattern in
      let body = compile_expression ~raise path scope body in
      O.{constructor;pattern;body})
    in
    let tv = compile_type_expression ~raise path scope tv in
    Match_variant {cases;tv}
  | Match_record {fields;body;tv} ->
    let scope, fields = Record.fold_map (fun scope' binder ->
        Scope.remove_value scope' binder.Binder.var,
        Binder.map (compile_type_expression ~raise path scope) binder) scope fields in
    let body   = compile_expression ~raise path scope body in
    let tv     = compile_type_expression ~raise path scope tv in
    Match_record {fields;body;tv}



and compile_declaration ~raise path scope (d : I.declaration) =
  match Location.unwrap d with
    D_value {binder;expr;attr} ->
      let expr   = compile_expression ~raise path scope expr in
      let attr = compile_value_attr attr in
      let binder = Binder.map (fun _ -> expr.type_expression) binder in
      let scope  = Scope.push_value scope binder expr.type_expression attr path in
      let scope,var = Scope.add_path_to_var scope path binder.var in
      let binder = { binder with var} in
      scope, fun e ->
        O.e_a_let_in binder expr e attr
  | D_type _ ->
      scope, fun e -> e
  | D_module {module_binder;module_;module_attr=_} ->
      let path' = Path.add_to_path path module_binder in
      let mod_scope,decl_list = compile_module_expr ~raise path' scope module_ in
      let scope   = Scope.push_module scope module_binder path' mod_scope in
      scope, decl_list

and compile_declaration_list ~raise (path : Path.t) scope (program : I.program) : (Scope.t) * (O.expression -> O.expression) =
  let scope, current = List.fold_map ~init:scope ~f:(compile_declaration ~raise path) program in
  let current = List.fold_left ~f:Function.compose ~init:(fun e -> e) current in
  scope, current

and compile_decl ~raise path scope (Decl d : I.decl) =
  compile_declaration ~raise path scope d

and compile_module ~raise (path : Path.t) scope (program : I.module_) : (Scope.t) * (O.expression -> O.expression) =
  let scope, current = List.fold_map ~init:scope ~f:(compile_decl ~raise path) program in
  let current = List.fold_left ~f:Function.compose ~init:(fun e -> e) current in
  scope, current

and compile_module_expr ~raise : Path.t -> Scope.t -> I.module_expr -> (Scope.t) * (O.expression -> O.expression) =
  fun path scope mexpr ->
    let rec get_declarations_from_scope scope new_path old_path =
      let dcls = Scope.get_declarations scope in
      let module_ = List.fold_left ~init:(fun e -> e) dcls ~f:(fun f dcl ->
        match dcl with
          Value (binder,ty,attr) ->
            let variable = O.e_a_variable (snd @@ Scope.add_path_to_var scope old_path binder.var) ty in
            let _,var = Scope.add_path_to_var scope new_path binder.var in
            fun e -> O.e_a_let_in {binder with var} variable (f e) attr
        | Module (var) ->
          let _,scope  = Scope.find_module scope var in
          let old_path = Path.add_to_path old_path var in
          let new_path = Path.add_to_path new_path var in
          let module_ = get_declarations_from_scope scope new_path old_path in
          fun e -> module_ (f e)
      ) in module_
    in
    match mexpr.wrap_content with
    | M_struct m -> (
      (* Keep the scope of identifiers be start with an empty list of declaration (corresponding to this module) *)
      let scope = Scope.clean_declarations scope in
      compile_module ~raise path scope m
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

let compile ~raise : I.expression -> I.program -> O.expression =
  fun hole program ->
    let scope,aliases,program = preprocess_program program in
    let hole = preprocess_expression ~scope ~aliases hole in
    let (scope), decls = compile_declaration_list ~raise Path.empty Scope.empty program in
    let init = compile_expression ~raise Path.empty scope hole in
    decls init

let compile_expression ~raise : I.expression -> O.expression =
  fun e ->
    let e = preprocess_expression e in
    let e = compile_expression ~raise Path.empty Scope.empty e in
    e

