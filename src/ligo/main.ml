(* -*- compile-command: "cd .. ; dune build -p ligo" -*- *)

open Ligo_helpers
open Trace

module Untyped = struct
  module WrapLocation = Wrap.Location
  let wrap = Wrap.Location.make

  module Type = struct
    type name = string

    type base = [
      | `Unit
      | `Bool
      | `Int
      | `Nat
    ]

    let unit : base = `Unit
    let bool : base = `Bool
    let int : base = `Int
    let nat : base = `Nat

    type 'a node = [
      | `Pair of 'a * 'a
      | `Or of 'a * 'a
    ]

    type expression_ast = [
      | expression node
      | base
      | `Name of name
    ]

    and expression = expression_ast WrapLocation.t

    let pair ~loc a b : expression = wrap ~loc (`Pair(a,b))
    let union ~loc a b : expression = wrap ~loc (`Or(a,b))
    let name ~loc s : expression =
      wrap ~loc (match s with
          | "Unit" -> (unit :> expression_ast)
          | "Bool" -> (bool :> expression_ast)
          | "Int" -> (int :> expression_ast)
          | "Nat" -> (nat :> expression_ast)
          | s -> `Name s)
  end

  module Value = struct
    type name = string
    type function_name = string

    type constant = [
      | `Int of int
    ]

    type expression = [
      | `Variable of name
      | `Pair of expression * expression
      | `Application of expression * expression
      | `Constant of constant
    ] WrapLocation.t

    type assignment = [
      | `Let of name * expression
      | `Type of Type.name * Type.expression
      | `Function of function_name * Type.expression * block
    ] WrapLocation.t

    and statement = [
      | `Assignment of assignment
      | `ForEach of name * expression * block
      | `While of expression * block
      | `Condition of expression * block * (expression * block) list * block option
    ] WrapLocation.t

    and block = statement list WrapLocation.t

    and program = assignment list WrapLocation.t

    type 'a wrapper = loc:Location.t -> 'a -> 'a WrapLocation.t
    let int = (WrapLocation.make_f (fun a -> `Constant (`Int a)) : loc:_ -> _ -> expression)
    let constatn = (WrapLocation.make_f (fun a -> `Constant a) : loc:_ -> _ -> expression)

    let variable = (WrapLocation.make_f (fun a -> `Variable a) : loc:_ -> _ -> expression)

    let pair = (WrapLocation.make_f (fun a -> `Pair a) : loc:_ -> _ -> expression)
    let application = (WrapLocation.make_f (fun a -> `Application a) : loc:_ -> _ -> expression)

    let let_ = (WrapLocation.make_f (fun a -> `Let a) : loc:_ -> _ -> assignment)
    let type_ = (WrapLocation.make_f (fun a -> `Type a) : loc:_ -> _ -> assignment)
    let fun_ = (WrapLocation.make_f (fun a -> `Function a) : loc:_ -> _ -> assignment)
    let assignment = (WrapLocation.make_f (fun a -> `Assignment a) : loc:_ -> _ -> statement)

    let foreach = (WrapLocation.make_f (fun a -> `ForEach a) : loc:_ -> _ -> statement)
    let while_ = (WrapLocation.make_f (fun a -> `While a) : loc:_ -> _ -> statement)

    let elseif x : (expression * block) = x
    let else_ x : block = x
    let if_ = (WrapLocation.make_f (fun a -> `Condition a) : loc:_ -> _ -> statement)

    let block = (WrapLocation.make : loc:_ -> _ -> block)
    let program = (WrapLocation.make : loc:_ -> _ -> program)
  end
end

module Typed = struct

  module Type = struct
    module WrapLocation = Wrap.Location
    let wrap = WrapLocation.make

    type name = string

    type base = [
      | `Unit
      | `Bool
      | `Int
      | `Nat
    ]

    let unit : base = `Unit
    let bool : base = `Bool
    let int : base = `Int
    let nat : base = `Nat

    type 'a node = [
      | `Pair of 'a * 'a
      | `Or of 'a * 'a
    ]

    type value = [
      | value node
      | base
    ]

    type expression_ast = [
      | expression node
      | base
      | `Name of name
    ]

    and expression = expression_ast

    let rec of_untyped (x:Untyped.Type.expression) : expression = match x.value with
      | `Pair(a, b) -> `Pair(of_untyped a, of_untyped b)
      | `Or(a, b) -> `Or(of_untyped a, of_untyped b)
      | `Int as s -> s
      | `Unit as s -> s
      | `Nat as s -> s
      | `Bool as s -> s
      | `Name _ as s -> s

    let pair_v a b : value = `Pair(a,b)
    let union_v a b : value = `Or(a,b)

    let pair_e a b : expression = `Pair(a,b)
    let union_e a b : expression = `Or(a,b)

    let name : string -> expression = function
      | "Unit" -> (unit :> expression_ast)
      | "Bool" -> (bool :> expression_ast)
      | "Int" -> (int :> expression_ast)
      | "Nat" -> (nat :> expression_ast)
      | s -> `Name s

  module Environment = Environment.Make(val (
      Environment.parameter () :
        (module Environment.PARAMETER
          with type key = name
           and type value = value)))

  let rec eval (env:Environment.t) : expression -> value result = function
    | `Name x -> (
        trace_option (simple_error "name doesn't exist in environment") @@
        Environment.get_opt env x
      )
    | `Pair (a, b) -> (
        eval env a >>? fun a ->
        eval env b >>? fun b ->
        ok (`Pair (a, b))
      )
    | `Or (a, b) -> (
        eval env a >>? fun a ->
        eval env b >>? fun b ->
        ok (`Or (a, b))
      )
    | `Bool as x -> ok x
    | `Unit as x -> ok x
    | `Nat as x -> ok x
    | `Int as x -> ok x
  end

  module Value = struct
    module WrapLocation = Wrap.Location
    let wrap = WrapLocation.make
    module WrapTypeLocation = Wrap.Make(struct type meta = (Type.value * Location.t) end)
    let wrap_tl = WrapTypeLocation.make
    let type_of (x:'a WrapTypeLocation.t) : Type.value = fst x.meta

    type name = string
    type function_name = string

    type constant = [
      | `Int of int
    ]

    type 'a node = [
      | `Constant of constant
      | `Pair of 'a * 'a
    ]
    let int n = `Constant (`Int n)

    type value = value node
    type expression = [
      | expression node
      | `Variable of name
    ] WrapTypeLocation.t

    let variable n = `Variable n
    let pair a b = `Pair (a, b)

    type assignment = [
      | `Let of name * expression
      | `Type of Type.name * Type.value
      | `Function of function_name * Type.value * block * Type.value
    ] WrapLocation.t

    and statement = assignment

    and block = statement list

    and toplevel_statement = assignment

    and program = toplevel_statement list

    module Environment = Environment.Make(val (
        Environment.parameter () :
          (module Environment.PARAMETER
            with type key = name
             and type value = Type.value)))
  end

  module Environment = struct
    type type_environment = Type.Environment.t
    type value_environment = Value.Environment.t

    type t = {
      type_environment : type_environment ;
      value_environment : value_environment ;
    }

    let empty = {
      type_environment = Type.Environment.empty ;
      value_environment = Value.Environment.empty ;
    }

    let add_type env
        name type_value =
      { env with
        type_environment =
          Type.Environment.set env.type_environment name type_value }

    let add_variable env
        name type_value =
      { env with
        value_environment =
          Value.Environment.set env.value_environment name type_value }
  end

end


module Typecheck = struct
  module UV = Untyped.Value
  module UT = Untyped.Type
  module TV = Typed.Value
  module TT = Typed.Type

  type env = Typed.Environment.t
  type ty = Typed.Type.value

  let typecheck_constant (constant:UV.constant) : _ = match constant with
    | `Int n -> (`Int, `Int n)

  let rec typecheck_expression (env:env) (e:UV.expression) : (TV.expression) result =
    match e.value with
    | `Constant c -> (
        let (ty, value) = typecheck_constant c in
        ok (TV.wrap_tl (ty, e.meta) (`Constant value))
      )
    | `Variable n -> (
        trace_option (simple_error "variable doesn't exist in env")
        @@ TV.Environment.get_opt env.value_environment n >>? fun ty ->
        ok (TV.wrap_tl (ty, e.meta) (TV.variable n))
      )
    | `Pair(a, b) -> (
        typecheck_expression env a >>? fun a ->
        typecheck_expression env b >>? fun b ->
        let ty = TT.pair_v (TV.type_of a) (TV.type_of b) in
        ok (TV.wrap_tl (ty, e.meta) (TV.pair a b))
      )
    | `Application _ -> simple_fail "Application isn't supported yet"

  let rec typecheck_assignment (env:env) (u:UV.assignment) : (env * TV.assignment) result =
    match u.value with
    | `Let(name, expression) -> (
        typecheck_expression env expression >>? fun expression ->
        let ass : TV.assignment = TV.wrap ~loc:u.meta (`Let(name, expression)) in
        let env = Typed.Environment.add_variable env name (TV.type_of expression) in
        ok (env, ass)
      )
    | `Type(name, expression) -> (
        TT.eval env.type_environment (TT.of_untyped expression) >>? fun value ->
        let env = Typed.Environment.add_type env name value in
        let ass : TV.assignment = TV.wrap ~loc:u.meta (`Type(name, value)) in
        ok (env, ass)
      )
    | `Function(name, type_expression, block) -> (
        TT.eval env.type_environment (TT.of_untyped type_expression) >>? fun type_value ->
        let env = Typed.Environment.add_variable env "input" type_value in
        typecheck_block env block >>? fun (env, block) ->
        let ty =
          match TV.Environment.get_opt env.value_environment "output" with
          | None -> `Unit
          | Some x -> x in
        let ass : TV.assignment = TV.wrap ~loc:u.meta (`Function(name, type_value, block, ty)) in
        ok (env, ass)
      )

  and typecheck_statement (env:env) (s:Untyped.Value.statement) : (env * Typed.Value.statement) result =
    match s.value with
    | `Assignment a -> typecheck_assignment env a
    | `Condition (_bool_expr, _block, _elseifs, _else_opt) -> simple_fail "conditions aren't supported yet"
    | `ForEach _ -> simple_fail "foreach is not supported yet"
    | `While _ -> simple_fail "while is not supported yet"

  and typecheck_block (env:env) (b:Untyped.Value.block) : (env * Typed.Value.block) result =
    let rec aux env = function
      | [] -> ok (env, [])
      | hd :: tl -> (
          typecheck_statement env hd >>? fun (env, hd) ->
          aux env tl >>? fun (env, tl) ->
          ok (env, hd :: tl)
        ) in
    aux env b.value

  let typecheck_program ?(env=Typed.Environment.empty) (u:Untyped.Value.program) : Typed.Value.program result =
    let rec aux env = function
      | [] -> ok []
      | hd :: tl -> (
          typecheck_assignment env hd >>? fun (env, hd) ->
          aux env tl >>? fun tl ->
          ok (hd :: tl)
        ) in
    aux env u.value
end

module Transpile = struct
  open Mini_c
  open Typed

  let rec translate_type : Type.value -> Mini_c.type_value result = function
    | `Bool -> ok (`Base Bool)
    | `Int -> ok (`Base Int)
    | `Nat -> ok (`Base Nat)
    | `Unit -> ok (`Base Unit)
    | `Pair(a, b) -> (
        translate_type a >>? fun a ->
        translate_type b >>? fun b ->
        ok (`Pair(a, b))
      )
    | `Or(a, b) -> (
        translate_type a >>? fun a ->
        translate_type b >>? fun b ->
        ok (`Or(a, b))
      )

  let rec translate_expression (e:Value.expression) : Mini_c.expression result =
    let%bind (e' : Mini_c.expression') = match e.value with
      | `Constant (`Int n) -> ok (Literal (`Int n))
      | `Variable n -> ok (Var n)
      | `Pair (a, b) -> (
          translate_expression a >>? fun a ->
          translate_expression b >>? fun b ->
          ok (Predicate("Pair", [a ; b]))
        ) in
    let%bind (t : Mini_c.type_value) = translate_type @@ fst e.meta in
    ok (e', t)

  let rec translate_assignment (ass:Value.assignment)
    : Mini_c.assignment option result = match ass.value with
    | `Let(x, expr) -> (
        translate_expression expr >>? fun expr ->
        ok (Some (Variable(x, expr)))
      )
    | `Function(name, input_ty, body, output_ty) -> (
        translate_type input_ty >>? fun input ->
        translate_type output_ty >>? fun output ->
        block body >>? fun body ->
        let ass = Fun(name, {input ; output ; body}) in
        ok (Some ass)
      )
    | `Type _ -> ok None

  and statement (st:Value.statement)
    : Mini_c.statement option result =
    translate_assignment st >>? fun a ->
    let ass = match a with
      | Some a -> Some (Assignment a)
      | None -> None in
    ok ass

  and block : Value.block -> Mini_c.block result = function
    | [] -> ok []
    | hd :: tl -> (
        statement hd >>? fun st_opt ->
        let sts = match st_opt with
          | Some x -> [x]
          | None -> [] in
        block tl >>? fun (new_sts) ->
        ok (sts @ new_sts)
      )

  let translate_toplevel_statement = translate_assignment

  let rec program : Value.program -> Mini_c.program result = function
    | [] -> ok []
    | hd :: tl -> (
        translate_assignment hd >>? fun ass_opt ->
        let asss = match ass_opt with
          | Some x -> [x]
          | None -> [] in
        program tl >>? fun (new_asss) ->
        ok (asss @ new_asss)
      )

  let of_mini_c : Mini_c.value -> Value.value result = function
    | `Int n -> ok (Value.int n)
    | _ -> simple_fail "unknown value"

  let to_mini_c : Value.value -> Mini_c.value result = function
    | `Constant (`Int n) -> ok (`Int n)
    | _ -> simple_fail "unknown value"

  let program_to_michelson (p:Value.program) =
    let%bind program_mini_c = program p in
    let%bind program = Mini_c.Translate_program.translate program_mini_c in
    ok program.body
end

module Run = struct
  open Typed.Value
  let run (program : program) (input : value) : value result =
    Transpile.program program >>? fun program_mini_c ->
    Transpile.to_mini_c input >>? fun input_mini_c ->
    (*    Format.printf "%a\n" Mini_c.PP.program program_mini_c ; *)
    Mini_c.Run.run program_mini_c input_mini_c >>? fun output_mini_c ->
    Transpile.of_mini_c output_mini_c >>? fun output ->
    ok output
end
