open Mini_c
module AST = Ligo_parser.Typed.O
module SMap = Ligo_parser.Typed.SMap

module Rename = struct
  open! AST

  let rec rename_expr_case (src:string) (dst:string) : expr_case -> expr_case = function
    | App {operator;arguments} -> App {operator = rename_operator src dst operator ; arguments = rename_exprs src dst arguments}
    | Var n when n.name.name = src -> Var {n with name = {n.name with name = dst}}
    | Var n -> Var n
    | Constant c -> Constant c
    | Record r -> Record (List.map (fun (key, expr) -> key, rename_expr src dst expr) r)
    | Lambda {parameter} as l when parameter.name.name = src -> l
    | Lambda ({instructions;declarations} as l) ->
        Lambda {l with instructions = rename_instrs src dst instructions ; declarations = rename_declarations src dst declarations}

  and rename_expr (src:string) (dst:string) (e : expr) : expr =
    { e with expr = rename_expr_case src dst e.expr }

  and rename_exprs src dst exprs = List.map (rename_expr src dst) exprs

  and rename_operator_case (src:string) (dst:string) : operator_case -> operator_case = function
    | Function n when n.name = src -> Function {n with name = dst}
    | x -> x

  and rename_operator src dst (o:operator) : operator = {o with operator = rename_operator_case src dst o.operator}

  and rename_var src dst (v:var_name) : var_name =
    if v.name = src
    then {v with name = dst}
    else v

  and rename_instr (src:string) (dst:string) : instr -> instr = function
    | Assignment {name;value;orig} when name.name = src -> Assignment {name = {name with name = dst};value;orig}
    | Assignment {name;value;orig} -> Assignment {value = rename_expr src dst value;name;orig}
    | While {condition;body;orig} -> While {condition = rename_expr src dst condition;body=rename_instrs src dst body;orig}
    | ForCollection {list;var;body;orig} -> ForCollection {list = rename_expr src dst list;var = rename_var src dst var;
                                                           body = rename_instrs src dst body;orig}
    | Match ({expr;cases} as a) -> Match {a with expr = rename_expr src dst expr ; cases = rename_match_cases src dst cases}
    | ProcedureCall {expr;orig} -> ProcedureCall {expr = rename_expr src dst expr;orig}
    | Fail {expr;orig} -> Fail {expr = rename_expr src dst expr;orig}

  and rename_instrs src dst : instr list -> instr list = List.map (rename_instr src dst)

  and rename_match_cases (src:string) (dst:string) (m:(_ * instr list) list) =
    List.map (fun (x, y) -> x, rename_instrs src dst y) m

  and rename_declaration (src:string) (dst:string) ({var} as d: decl) : decl =
    if var.name.name = src
    then {d with var = {var with name = {var.name with name = dst}}}
    else d

  and rename_declarations (src:string) (dst:string) (decls:decl list) =
    List.map (rename_declaration src dst) decls
end

let list_of_map m = List.rev @@ SMap.fold (fun _ v prev -> v :: prev) m []

let rec translate_type : AST.type_expr -> type_value result = fun {type_expr}  ->
  match type_expr with
  | Unit -> ok (`Base Unit)
  | Int -> ok (`Base Int)
  | String -> ok (`Base String)
  | Bool -> ok (`Base Bool)
  | Sum m ->
      let node = Append_tree.of_list @@ List.map snd @@ list_of_map m in
      let aux a b : type_value result =
        let%bind a = a in
        let%bind b = b in
        ok (`Or (a, b))
      in
      Append_tree.fold_ne translate_type aux node
  | Record r ->
      let node = Append_tree.of_list @@ List.map snd @@ list_of_map r in
      let aux a b : type_value result =
        let%bind a = a in
        let%bind b = b in
        ok (`Pair (a, b))
      in
      Append_tree.fold_ne translate_type aux node
  | Ref t -> translate_type t
  | Function {arg;ret} ->
      let%bind arg = translate_type arg in
      let%bind ret = translate_type ret in
      ok (`Function(arg, ret))
  | TypeApp _ -> simple_fail "No type application"

let translate_constant : AST.constant -> value result = function
  | Unit -> ok `Unit
  | String s -> ok (`String s)
  | Int n -> ok (`Int (Z.to_int n))
  | False -> ok (`Bool false)
  | True -> ok (`Bool true)
  | _ -> simple_fail ""

let rec translate_lambda : AST.lambda -> anon_function result =
  fun {declarations;parameter;instructions;result} ->
  let ({name;ty}:AST.typed_var) = parameter in
  let%bind input_ty = translate_type ty in
  let%bind output_ty = translate_type result.ty in
  let%bind result = translate_expr result in
  let%bind (declaration_statements : statement list) = translate_declarations declarations in
  let%bind (instruction_statements : statement list) = translate_instructions instructions in
  let body = declaration_statements @ instruction_statements in
  ok {content={binder=name.name;input=input_ty;output=output_ty;body;result} ; capture = No_capture}

and translate_expr' : AST.expr_case -> expression' result = function
  | Var {name} -> ok (Var name.name)
  | Constant cst ->
      let%bind value = translate_constant cst in
      ok (Literal value)
  | Lambda _ -> simple_fail "Mini_c doesn't deal with lambda in expressions yet"
  | _ -> simple_fail ""

and translate_expr env : AST.expr -> expression result = fun {expr;ty} ->
  let%bind expr = translate_expr' expr in
  let%bind ty = translate_type ty in
  ok (expr, ty, env)

and translate_declaration : AST.decl -> statement result = fun {var;value} ->
  let%bind expr = translate_expr value in
  ok (Assignment(Variable(var.name.name, expr)))

and translate_declarations : AST.decl list -> statement list result = fun declarations ->
  bind_list @@ List.map translate_declaration declarations

and translate_match (expr:AST.expr) (cases: (AST.pattern * AST.instr list) list) : statement result =
  match cases with
  | [(AST.PTrue, instrs_true) ; (AST.PFalse, instrs_false) ] ->
      let%bind cond = translate_expr expr in
      let%bind b_true = translate_instructions instrs_true in
      let%bind b_false = translate_instructions instrs_false in
      ok (Cond (cond, b_true, b_false))
  | [(AST.PFalse, instrs_false) ; (AST.PTrue, instrs_true) ] ->
      let%bind cond = translate_expr expr in
      let%bind b_true = translate_instructions instrs_true in
      let%bind b_false = translate_instructions instrs_false in
      ok (Cond (cond, b_true, b_false))
  | _ -> simple_fail "unrecognized pattern"

and translate_instruction : AST.instr -> statement result = function
  | Assignment {name ; value} ->
      let%bind expr = translate_expr value in
      ok (Assignment (Variable(name.name, expr)))
  | While {condition ; body} ->
      let%bind block = translate_instructions body in
      let%bind cond = translate_expr condition in
      ok (While (cond, block))
  | ForCollection _ -> simple_fail "We don't deal with for collection yet"
  | Match {expr;cases} -> translate_match expr cases
  | Fail _ -> simple_fail "Fail have to be added in Mini_C"
  | ProcedureCall _ -> simple_fail "Drop Unit have to be added in Mini_C"

and translate_instructions : AST.instr list -> statement list result = fun instrs ->
  bind_list @@ List.map translate_instruction instrs

let translate_program : AST.ast -> block result = fun {declarations} ->
  translate_declarations declarations

let rec to_mini_c_value' : (AST.expr_case * AST.type_expr) -> value result = function
  | Constant c, _ -> translate_constant c
  | App {arguments;operator = {operator = Constructor c ; ty = {type_expr = Sum lst}}}, _ ->
      let node = Append_tree.of_list @@ List.map fst @@ list_of_map lst in
      let%bind lst =
        trace_option (simple_error "Not constructor of variant type") @@
        Append_tree.exists_path (fun (x:AST.name_and_region) -> x.name = c.name) node in
      let arg = List.hd arguments in
      let%bind arg = to_mini_c_value arg in
      let ors = List.fold_left (fun b a -> if a then `Right b else `Left b) arg (List.rev lst) in
      ok ors
  | App _, _ -> simple_fail "Applications aren't value"
  | Record lst, _ ->
      let node = Append_tree.of_list @@ List.map snd lst in
      let aux a b =
        let%bind a = a in
        let%bind b = b in
        ok (`Pair (a, b))
      in
      Append_tree.fold_ne to_mini_c_value aux node
  | Lambda _, _-> simple_fail "Lambda aren't value yet"
  | Var _, _-> simple_fail "Var aren't value yet"

and to_mini_c_value : AST.expr -> value result = fun {expr;ty} ->
  to_mini_c_value' (expr, ty)

let ghost expr ty : AST.expr = {expr;ty;orig=`TODO}

let of_mini_c_value ({type_expr} as ty, v : AST.type_expr * value) : AST.expr result = match (type_expr, v) with
  | String, `String s -> ok @@ ghost (Constant (String s)) ty
  | Bool, `Bool b -> ok @@ ghost (Constant (if b then True else False)) ty
  | Unit, `Unit -> ok @@ ghost (Constant (Unit)) ty
  | Int, `Int n -> ok @@ ghost (Constant (Int (Z.of_int n))) ty
  | Function _, _ -> simple_fail "Functions aren't retrieved from Mini_C yet"
  | _ -> simple_fail "of_mini_c_value error"

