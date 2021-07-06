[@@@warning "-27"]
[@@@warning "-39"]

open Errors
open Trace
open Function

module CST = Cst.Jsligo
module AST = Ast_imperative

open AST

(* type nonrec 'a result = ('a , abs_error) result *)

type nested_match_repr = (*TODO  , move that in AST. (see !909) *)
  | PatternVar of AST.ty_expr binder
  | TupleVar of AST.ty_expr binder * nested_match_repr list
  | RecordVar of AST.ty_expr binder * AST.label list * nested_match_repr list

let nseq_to_list (hd, tl) = hd :: tl

let npseq_to_list (hd, tl) = hd :: (List.map ~f:snd tl)

let npseq_to_ne_list (hd, tl) = hd, (List.map ~f:snd tl)

let pseq_to_list = function
  | None -> []
  | Some lst -> npseq_to_list lst

let get_value : 'a Raw.reg -> 'a = fun x -> x.value

let build_ins = ["Test";"Tezos";"Crypto";"Bytes";"List";"Set";"Map";"Big_map";"Bitwise";"String";"Layout"]
  @ ["Michelson"]

open Predefined.Tree_abstraction.Jsligo

let r_split = Location.r_split

let compile_variable var = Location.map Var.of_name @@ Location.lift_region var
let compile_attributes attributes : string list =
  List.map ~f:(fst <@ r_split) attributes

module Compile_type = struct


  (*
    `type_compiler_opt` represents an abstractor for a single pattern.
    For instance, you could have a `type_compiler_opt` that is supposed to compile
    only `List(42)`.

    The goal of defining those auxiliary typers is to have a clean compiler function.
    If things are not done this way, then the function will be a big pattern-match with
    a lot of edge cases. Like, "compile type applications this way, except when it's `List`,
    or `sapling_state`, etc.".

    Instead, one can define a `type_compiler_opt` matching their pattern, and then use
    `try_type_compilers`.
  *)

  type type_compiler_opt = CST.type_expr -> (AST.type_expression option, abs_error) result

  let rec type_expression_to_constructor : CST.type_expr -> (string * AST.type_expression * attributes, _) result = function
    | TString s -> ok (s.value, t_unit () ~loc:(Location.lift s.region), [])
    | TProd {inside = {value = {inside = (TString s, []); _}; region}; attributes} ->
      let attributes = compile_attributes attributes in
      ok (s.value, t_unit () ~loc:(Location.lift region), attributes)
    | TProd {inside = {value = {inside = (TString s, rest); _}; region}; attributes} -> 
      let attributes = compile_attributes attributes in
      let lst = List.map ~f:snd rest in
      let* lst = bind_map_list compile_type_expression lst in
      (match lst with 
        [a] -> ok @@ (s.value, a, attributes)
      | lst -> 
        let t = t_tuple lst in
        ok @@ (s.value, t, attributes))
    | _ as t -> fail @@ invalid_constructor t

  and get_t_int_singleton_opt = function
  | CST.TInt x -> 
    let (_,z) = x.value in
    Some z
  | _ -> None

  and get_t_string_singleton_opt = function
  | CST.TString s -> Some s.value
  | _ -> None

  (*
    This chains the application of multiple `type_compiler_opt`. If the first returns `None`, use
    the next one, etc.
  *)
  and type_compiler_opt_list : type_compiler_opt list -> type_compiler_opt = fun compilers te ->
    match compilers with
    | [] -> ok None
    | hd :: tl -> (
      let* x = hd te in
      match x with
      | Some x -> ok (Some x)
      | None -> type_compiler_opt_list tl te
    )

  (*
    `try_type_compilers compilers type_expression other` will try to run the `compilers` on
    `type_expression`. If they all return `None`, it will run `other` instead.
  *)
  and try_type_compilers :
    type_compiler_opt list -> CST.type_expr ->
    (unit -> (AST.type_expression, _) result) ->
    (AST.type_expression, _) result =
  fun compilers te other ->
    let* x = type_compiler_opt_list compilers te in
    match x with
    | Some x -> ok x
    | None -> other ()

  and compile_type_function_args : CST.fun_type_args -> (type_expression, _) result = fun args ->
    let unpar = args.inside in
    let (hd , tl_sep) = unpar in
    let tl = List.map ~f:snd tl_sep in
    let aux : CST.fun_type_arg -> (type_expression, _) result = fun x -> compile_type_expression x.type_expr in
    let* lst = Trace.bind_map_list aux (hd :: tl) in
    match lst with 
      [a] -> ok @@ a
    | lst -> ok @@ t_tuple lst

  and compile_sapling : type_compiler_opt = fun te ->
    match te with
    | TApp app -> (
      let ((operator,args), loc) = r_split app in
      match operator.value with
      | "sapling_state" -> (
        let lst = npseq_to_list args.value.inside in
        (match lst with
        | [(a : CST.type_expr)] -> (
          let sloc = Location.lift @@ Raw.type_expr_to_region a in
          let* a' =
            trace_option (michelson_type_wrong te operator.value) @@
              get_t_int_singleton_opt a in
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          ok @@ Some (t_sapling_state ~loc singleton)
          )
        | _ -> fail @@ michelson_type_wrong_arity loc operator.value)
      )
      | "sapling_transaction" ->
           let lst = npseq_to_list args.value.inside in
           (match lst with
           | [(a : CST.type_expr)] -> (
             let sloc = Location.lift @@ Raw.type_expr_to_region a in
             let* a' =
               trace_option (michelson_type_wrong te operator.value) @@
                 get_t_int_singleton_opt a in
             let singleton = t_singleton ~loc:sloc (Literal_int a') in
             ok @@ Some (t_sapling_transaction ~loc singleton)
             )
           | _ -> fail @@ michelson_type_wrong_arity loc operator.value)
      | _ -> ok None
    )
    | _ -> ok None

  (* this is a bad design, michelson_or and pair should be an operator
  see AnnotType *)
  and compile_michelson_pair_or : type_compiler_opt = fun te ->
    match te with
    | TApp app -> (
      let ((operator,args), loc) = r_split app in
      match operator.value with
      | "michelson_or" ->
        let lst = npseq_to_list args.value.inside in
        let* lst = match lst with
        | [TProd a] -> ok @@ npseq_to_list a.inside.value.inside
        | _ -> fail @@ michelson_type_wrong_arity loc operator.value
        in
        (match lst with
        | [a ; b ; c ; d ] -> (
          let* b' =
            trace_option (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt b in
          let* d' =
            trace_option (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt d in
          let* a' = compile_type_expression a in
          let* c' = compile_type_expression c in
          ok @@ Some (t_michelson_or ~loc a' b' c' d')
          )
        | _ -> fail @@ michelson_type_wrong_arity loc operator.value)
      | "michelson_pair" ->
        let lst = npseq_to_list args.value.inside in
        let* lst = match lst with
        | [TProd a] -> ok @@ npseq_to_list a.inside.value.inside
        | _ -> fail @@ michelson_type_wrong_arity loc operator.value
        in
        (match lst with
        | [a ; b ; c ; d ] -> (
          let* b' =
            trace_option (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt b in
          let* d' =
            trace_option (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt d in
          let* a' = compile_type_expression a in
          let* c' = compile_type_expression c in
          ok @@ Some (t_michelson_pair ~loc a' b' c' d')
          )
        | _ -> fail @@ michelson_type_wrong_arity loc operator.value)
      | _ -> ok None
    )
    | _ -> ok None

  and compile_type_expression : CST.type_expr -> (type_expression, _) result =
    fun te ->
    let self = compile_type_expression in
    let return te = ok @@ te in
    (* This is not efficient. It would make more sense to split each type_compiler in their own match branch. *)
    try_type_compilers [
      compile_sapling ;
      compile_michelson_pair_or ;
    ] te @@ fun () ->
    match te with
    | TSum sum ->
        let sum_type, loc = r_split sum in
        let {variants; attributes; _} : CST.sum_type = sum_type in
        let lst = npseq_to_list variants in
        let attr = compile_attributes attributes in
        let aux (v : CST.type_expr) : (string * type_expression * string list, _) result =
          let* (constructor, type_expr, variant_attr) = type_expression_to_constructor v in
          ok @@ (constructor, type_expr, variant_attr) in
        let* sum = bind_map_list aux lst
        in return @@ t_sum_ez_attr ~loc ~attr sum
    | TObject record ->
      let injection, loc = r_split record in   
      let attributes = compile_attributes injection.attributes in
      let lst = npseq_to_list injection.ne_elements in
      let aux (field : CST.field_decl CST.reg) =
        let f, _ = r_split field in
        let* type_expr =
          self f.field_type in
        let field_attr = compile_attributes f.attributes in
        return @@ (f.field_name.value, type_expr, field_attr) in
      let* fields = bind_map_list aux lst in
      return @@ t_record_ez_attr ~loc ~attr:attributes fields
    | TProd prod  ->
      let (nsepseq, loc) = r_split prod.inside in
      let lst = npseq_to_list nsepseq.inside in
      let* lst = bind_map_list self lst in
      return @@ t_tuple ~loc lst
    | TApp app ->
      let ((operator,args), loc) = r_split app in
      let operators = Var.of_name operator.value in
      let lst = npseq_to_list args.value.inside in
      let* lst = bind_map_list self lst in
      return @@ t_app ~loc operators lst
    | TFun func ->
      let ((input_type,_,output_type), loc) = r_split func in
      let* input_type = compile_type_function_args input_type in
      let* output_type = self output_type in
      return @@ t_function ~loc input_type output_type
    | TPar par ->
      let (par, _) = r_split par in
      let type_expr = par.inside in
      self type_expr
    | TVar var ->
      let (name,loc) = r_split var in
      let v = Var.of_name name in
      return @@ t_variable ~loc v
    | TWild _reg -> fail @@ unsupported_twild te
    | TString _s -> fail @@ unsupported_string_singleton te
    | TInt _s -> fail @@ unsupported_string_singleton te
    | TModA ma ->
      let (ma, loc) = r_split ma in
      let (module_name, _) = r_split ma.module_name in
      let* element = self ma.field in
      return @@ t_module_accessor ~loc module_name element
end

open Compile_type

let expression_to_variable : CST.expr -> (CST.variable, _) result = function
  | EVar var -> ok var
  | _ as e -> fail @@ expected_a_variable e

let selection_to_variable : CST.selection -> (CST.variable, _) result = function
  | FieldName sfn -> (
      let (sfn , _) = r_split sfn in
      ok sfn.value
    )
  | _ as f -> fail @@ expected_a_field_name f

let compile_expression_to_int : CST.expr -> (z, _) result = function
  | EArith (Int i) -> ok (snd (i.value))
  | _ as e -> fail @@ expected_an_int e

let compile_selection : CST.selection -> (_ access * location, _) result = fun selection ->
  match selection with
    FieldName name ->
    let (name, loc) = r_split name in
    ok (Access_record name.value.value, loc)
  | Component comp ->
    let (index_expr, loc) = r_split comp in
    let* index = compile_expression_to_int index_expr.inside in
    ok (Access_tuple index, loc)

let array_item_to_expression : CST.array_item -> (CST.expr, _) result = function
  | Expr_entry expr -> ok expr
  | (Empty_entry _
  | Rest_entry _) as r ->
    fail @@ expected_an_expression r

let get_t_string_singleton_opt = function
| CST.TString s -> Some s.value
| _ -> None

type statement_result =
  Binding of (AST.expression -> AST.expression)
| Expr of AST.expression
| Break of AST.expression
| Return of AST.expression

type constr_types = 
  Match_nil of AST.expression
| Match_cons of expression_ Var.t location_wrap * expression_ Var.t location_wrap

let rec compile_tuple_expression ?loc tuple_expr =
  let* lst = bind_map_list (fun e -> compile_expression e) @@ nseq_to_list tuple_expr in
  match lst with
    hd::[] -> ok hd
  | lst -> ok @@ e_tuple ?loc lst

and compile_bin_op (op_type : AST.constant') (op : _ CST.bin_op CST.reg) =
  let self = compile_expression in
  let return e = ok @@ e in
  let (op, loc) = r_split op in
  let* a = self op.arg1 in
  let* b = self op.arg2 in
  return @@ e_constant ~loc (Const op_type) [a; b]

and compile_un_op (op_type : AST.constant') (op : _ CST.un_op CST.reg) =
  let self = compile_expression in
  let return e = ok @@ e in
  let (op, loc) = r_split op in
  let* arg = self op.arg in
  return @@ e_constant ~loc (Const op_type) [arg]

and compile_expression : CST.expr -> (AST.expr, _) result = fun e ->
  let self: CST.expr -> (AST.expr, _) result = compile_expression in
  let return e = ok @@ e in
  match e with
    EVar var ->
    let (var, loc) = r_split var in
    (match constants var with
      Some const -> return @@ e_constant ~loc const []
    | None -> return @@ e_variable_ez ~loc var
    )
  | EPar par -> self par.value.inside
  | EUnit the_unit ->
    let loc = Location.lift the_unit.region in
    return @@ e_unit ~loc ()
  | EBytes bytes ->
    let (bytes, loc) = r_split bytes in
    let (_s,b) = bytes in
    return @@ e_bytes_hex ~loc b
  | EString str ->(
    match str with
    | String str ->
      let (str, loc) = r_split str in
      return @@ e_string ~loc str
    | Verbatim str ->
      let (str, loc) = r_split str in
      return @@ e_verbatim ~loc str
  )
  | EArith arth ->
    ( match arth with
      Add plus   -> compile_bin_op C_POLYMORPHIC_ADD plus
    | Sub minus  -> compile_bin_op C_SUB minus
    | Mult times -> compile_bin_op C_MUL times
    | Div slash  -> compile_bin_op C_DIV slash
    | Mod mod_   -> compile_bin_op C_MOD mod_ 
    | Neg minus  -> compile_un_op C_NEG minus
    | Int i ->
      let ((_,i), loc) = r_split i in
      return @@ e_int_z ~loc i
    )
  | ELogic logic -> (
    match logic with
      BoolExpr be -> (
      match be with
        Or or_   -> compile_bin_op C_OR  or_
      | And and_ -> compile_bin_op C_AND and_
      | Not not_ -> compile_un_op  C_NOT not_
      | True  reg -> let loc = Location.lift reg in return @@ e_true  ~loc ()
      | False reg -> let loc = Location.lift reg in return @@ e_false ~loc ()
    )
    | CompExpr ce -> (
      match ce with
        Lt lt    -> compile_bin_op C_LT  lt
      | Leq le   -> compile_bin_op C_LE  le
      | Gt gt    -> compile_bin_op C_GT  gt
      | Geq ge   -> compile_bin_op C_GE  ge
      | Equal eq -> compile_bin_op C_EQ  eq
      | Neq ne   -> compile_bin_op C_NEQ ne
    )
  )
  | ECall {value = EProj {value = {expr = EVar {value = module_name; _}; selection = FieldName {value = {value = {value = fun_name; _}; _}; _}}; _}, arguments; region} when 
    List.mem ~equal:Caml.(=) build_ins module_name ->
      let var = module_name ^ "." ^ fun_name in
      let loc = Location.lift region in
      let argsx = match arguments with 
        Unit e -> CST.EUnit e, []
      | Multiple xs ->
        let hd,tl = xs.value.inside in
        hd,List.map ~f:snd tl
      in
      (match constants var with
      Some const ->
      let* args = bind_map_list (fun e -> self e) @@ nseq_to_list argsx in
      return @@ e_constant ~loc const args
    | None ->
      fail @@ unknown_constant var loc
      )
  | ECall {value=(EVar {value = "list"; _}, Multiple {value = {inside = (EArray {value = {inside = (Expr_entry e, [(_, Rest_entry {value = {expr; _}; _})]); _}; _}, []); _}; _}); region } -> 
    let loc = Location.lift region in
    let* a = self e in
    let* b = self expr in
    return @@ e_constant ~loc (Const C_CONS) [a; b]
  | ECall {value=(EVar {value = "list"; _}, Multiple {value = {inside = (EArray {value = {inside = items}}, []); _}; _}); region } -> 
    let loc = Location.lift region in
    let items = Utils.nsepseq_to_list items in
    (match items with 
      [CST.Empty_entry _] -> 
        return @@ e_list ~loc []
    | _ -> (
      let* lst = bind_map_list (fun e -> 
        match e with 
          CST.Expr_entry e -> compile_expression e
        | Empty_entry _ -> ok @@ e_unit ()
        | Rest_entry _ -> fail @@ array_rest_not_supported e
      ) items in
      return @@ e_list ~loc lst
    )
    )
  | ECall {value=(EVar {value = "match"; _}, Multiple {value = {inside = (input, [(_, EObject {value = {inside = fields; _}; _})]); _}; _}); region} ->
    (* Pattern matching for JsLIGO is implemented as a 'built-in function' as
       JavaScript and TypeScript don't have native pattern matching. *)
    let fields' = Utils.nsepseq_to_list fields in
    let compile_simple_pattern p =
      let rec aux = function
        CST.EVar v -> ok @@ Some (Var.of_name v.value, Location.lift v.region)
      | EPar par -> aux par.value.inside
      | ESeq {value = (hd, []); _} -> aux hd
      | EAnnot {value = (a, _, _); _} -> aux a
      | EUnit _ -> ok @@ None
      | _ as e -> fail @@ unsupported_match_pattern e
      in 
      aux p
    in
    let compile_constr_pattern = function 
      CST.Property {value = {name = EVar {value = constr; _}; value; _}; _} -> (
        match value with 
          EFun {value = {parameters; body; _}; _} -> 
            let* parameters_opt = compile_simple_pattern parameters in
            let* expr = compile_function_body_to_expression body in
            ok ((Label constr, parameters_opt), expr)
        | _ as e -> fail @@ invalid_case constr e (* TODO: improve error message *)
      ) 
    | _ as f -> fail @@ unsupported_match_object_property f
    in
    let loc = Location.lift region in
    let* matchee = compile_expression input in
    let* constrs = bind_map_list compile_constr_pattern fields' in
    let cases = List.map
      ~f:(fun ((constructor,p_opt),body) ->
        (* TODO: location should be fetch*)
        let param_loc = Location.generated in
        let whole_pattern_loc = Location.generated in
        let pvar = match p_opt with
          | Some (p, param_loc) ->
            let parameters = Location.wrap ~loc:param_loc p in
            Location.wrap ~loc:param_loc @@ P_var ({var = parameters ; ascr = None ; attributes = Stage_common.Helpers.const_attribute}:_ AST.binder)
          | None -> Location.wrap ~loc:param_loc P_unit
        in 
        let pattern = Location.wrap ~loc:whole_pattern_loc @@ P_variant (constructor, pvar) in
        ({body ; pattern} : _ AST.match_case)
      )
      constrs
    in
    ok @@ e_matching ~loc matchee cases
  | ECall {value=(EVar {value = "match"; _}, Multiple {value = {inside = (input, [(_, ECall {value = EVar {value="list"; _}, Multiple { value = {inside = (CST.EArray args, _); _} ;_} ;_})]); _}; _}); region} ->
    let args = Utils.nsepseq_to_list args.value.inside in
    let compile_simple_pattern p =
      let rec aux = function
        CST.EVar v -> ok @@ Var.of_name v.value
      | EPar par -> aux par.value.inside
      | ESeq {value = (hd, []); _} -> aux hd
      | EAnnot {value = (a, _, _); _} -> aux a
      | EUnit _ -> ok @@ Var.of_name "_"
      | _ as e -> fail @@ unsupported_match_pattern e
      in 
      aux p
    in
    let rec compile_parameter = function
      CST.EPar p -> compile_parameter p.value.inside
    | ESeq {value = (EAnnot {value = (EArray {value = {inside = (Empty_entry _, []); _}; _}, _, _); _}, _); _} ->
      ok @@ Match_nil (e_unit ())
    | ESeq {value = (EAnnot {value = (EArray {value = {inside = (Expr_entry hd, [(_, Rest_entry {value = {expr = tl; _}; _})]); _}; _}, _, _); _}, _); _} ->
      let hd_loc = Location.lift @@ Raw.expr_to_region hd in
      let tl_loc = Location.lift @@ Raw.expr_to_region tl in
      let* hd = compile_simple_pattern hd in
      let* tl = compile_simple_pattern tl in
      let hd = Location.wrap ~loc:hd_loc hd in
      let tl = Location.wrap ~loc:tl_loc tl in
      ok @@ Match_cons (hd, tl)
    | _ as e -> fail @@ not_a_valid_parameter e
    in
    let compile_case = function
      CST.EFun {value = {parameters; body; _}; _} ->
        let* args = compile_parameter parameters in
        let* b    = compile_function_body_to_expression body in
        ok (args, b)
    | _ as e -> fail @@ expected_a_function e
    in
    (match args with
      [CST.Expr_entry a; CST.Expr_entry b]
    | [CST.Expr_entry a; CST.Expr_entry b; CST.Rest_entry _] ->
        let* (params_a, body_a) = compile_case a in
        let* (params_b, body_b) = compile_case b in
        (match params_a, params_b, body_a, body_b with 
          Match_nil match_nil,  Match_cons (a,b), body_nil, body
        | Match_cons (a,b), Match_nil match_nil, body, body_nil ->
          let* matchee = compile_expression input in
          let loc = Location.lift region in
          let nil_case =
            (* TODO: improve locations here *)
            let pattern = Location.wrap @@ P_list (List []) in
            {pattern ; body = body_nil}
          in
          let cons_case =
            (* TODO: improve locations here *)
            let a = Location.wrap @@ P_var {var = a ; ascr = None ; attributes = Stage_common.Helpers.const_attribute} in
            let b = Location.wrap @@ P_var {var = b ; ascr = None ; attributes = Stage_common.Helpers.const_attribute} in
            let pattern = Location.wrap @@ P_list (Cons (a,b)) in
            {pattern ; body = body}
          in
          ok @@ e_matching ~loc matchee [nil_case;cons_case]
        | _ -> fail @@ invalid_list_pattern_match args
        )
    | _ -> fail @@ invalid_list_pattern_match args)
  
  (* This case is due to a bad besign of our constant it as to change
    with the new typer so LIGO-684 on Jira *)
  | ECall {value=(EVar var,args);region} ->
    let args = match args with
      | Unit the_unit -> CST.EUnit the_unit,[]
      | Multiple xs ->
         let hd,tl = xs.value.inside in
         hd,List.map ~f:snd tl in
    let loc = Location.lift region in
    let (var, loc_var) = r_split var in
    (match constants var with
      Some const ->
      let* args = bind_map_list (fun e -> self e) @@ nseq_to_list args in
      return @@ e_constant ~loc const args
    | None ->
      let func = e_variable_ez ~loc:loc_var var in
      let* args = compile_tuple_expression args in
      return @@ e_application ~loc func args
    )
  | EConstr (ESomeApp some) ->
    let ((_, arg), loc) = r_split some in
    let* args = compile_tuple_expression @@ List.Ne.singleton arg in
    return @@ e_some ~loc args
  | EConstr (ENone reg) ->
    let loc = Location.lift reg in
    return @@ e_none ~loc ()
  | EConstr (EConstrApp constr) ->
    let ((constr,args_o), loc) = r_split constr in
    let* args_o = bind_map_option (compile_tuple_expression <@ List.Ne.singleton) args_o in
    let args = Option.value ~default:(e_unit ~loc:(Location.lift constr.region) ()) args_o in
    return @@ e_constructor ~loc constr.value args
  | ECall {value=(EModA {value={module_name;field};region=_},args);region} when
    List.mem ~equal:Caml.(=) build_ins module_name.value ->
      let args = match args with
      | Unit the_unit -> CST.EUnit the_unit,[]
      | Multiple xs ->
         let hd,tl = xs.value.inside in
         hd,List.map ~f:snd tl in
      let loc = Location.lift region in
      let* fun_name = match field with
          EVar v -> ok @@ v.value
        | EConstr _ -> fail @@ unknown_constructor module_name.value loc
        | EModA ma ->
           let (ma, loc) = r_split ma in
           let (module_name, _) = r_split ma.module_name in
           fail @@ unknown_constant module_name loc
        | _ -> failwith "Corner case : This couldn't be produce by the parser"
      in
      let var = module_name.value ^ "." ^ fun_name in
      (match constants var with
        Some const ->
        let* args = bind_map_list self @@ nseq_to_list args in
        return @@ e_constant ~loc const args
      | None ->
        fail @@ unknown_constant var loc
        )
  | ECall call ->
    let ((func, args), loc) = r_split call in
    let args = match args with
      | Unit the_unit -> CST.EUnit the_unit,[]
      | Multiple xs ->
         let hd,tl = xs.value.inside in
         hd,List.map ~f:snd tl in
    let* func = self func in
    let* args = compile_tuple_expression args in
    return @@ e_application ~loc func args
  | EArray items ->
    let (items, loc) = r_split items in
    let items = npseq_to_list items.inside in
    (match items with
      [Empty_entry _] -> return @@ e_tuple ~loc []
    | _ ->
        let* exprs = bind_map_list array_item_to_expression items in
        let* exprs' = bind_map_list compile_expression exprs in
        return @@ e_tuple ~loc exprs')
  | EObject {value = {inside = (Property_rest {value = {expr; _}; _}, rest); _}; _} -> 
    let* record = compile_expression expr in
    let aux up =
      let (_, p) = up in
      match p with
        CST.Punned_property {value = EVar v as evar; region} ->
          let* expr = compile_expression evar in
          ok ([Access_record v.value], expr, Location.lift region)
      | Property {value = {name = EVar name; value; _}; region} ->
          let* expr = compile_expression value in
          ok ([Access_record name.value], expr, Location.lift region)
      | Property_rest _ -> fail @@ rest_not_supported_here p
      | _ -> fail @@ property_not_supported p
    in
    let* updates = bind_map_list aux rest in
    let aux e (path, update, loc) = e_update ~loc e path update in
    return @@ List.fold_left ~f:aux ~init:record updates
  | EObject obj ->
    let (obj, loc) = r_split obj in
    let aux : CST.property -> (string * expression, _) result = fun fa ->
      match fa with
      | Punned_property prop -> (
          let (prop, loc) = r_split prop in
          let* var = expression_to_variable prop in
          ok (var.value , e_variable ~loc (Location.wrap ~loc @@ Var.of_name var.value))
        )
      | Property prop2 -> (
          let (prop2 , _) = r_split prop2 in
          let* var = expression_to_variable prop2.name in
          let* expr = compile_expression prop2.value in
          ok (var.value , expr)
        )
      | Property_rest _ -> (
          fail @@ rest_not_supported_here fa
        )
    in
    let* obj = bind_map_list aux @@ npseq_to_list obj.inside in
    return @@ e_record_ez ~loc obj
  | EProj proj ->
    let (proj, loc) = r_split proj in
    let* var = compile_expression proj.expr in
    let* (sels , _) = compile_selection proj.selection in
    return @@ e_accessor ~loc var [sels]
  | EModA ma ->
    let (ma, loc) = r_split ma in
    let (module_name, _) = r_split ma.module_name in
    let* element = self ma.field in
    (*TODO: move to proper module*)
    if List.mem ~equal:Caml.(=) build_ins module_name then
      let* fun_name = match ma.field with
        EVar v -> ok @@ v.value
      | EConstr _ -> fail @@ unknown_constructor module_name loc
      | EModA ma ->
         let (ma, loc) = r_split ma in
         let (module_name, _) = r_split ma.module_name in
         fail @@ unknown_constant module_name loc
      | _ -> failwith "Corner case : This couldn't be produce by the parser"
      in
      let var = module_name ^ "." ^ fun_name in
      (match constants var with
        Some const -> return @@ e_constant ~loc const []
      | None -> return @@ e_variable_ez ~loc var
      )
    else
      return @@ e_module_accessor ~loc module_name element
  | EFun func ->
    let (func, loc) = r_split func in
    let ({parameters; lhs_type; body} : CST.fun_expr) = func in
    let* lhs_type = bind_map_option (compile_type_expression <@ snd) lhs_type in
    let* (binder,exprs) = compile_parameter parameters in
    let* body = compile_function_body_to_expression body in
    let aux (binder,attr,rhs) expr = e_let_in binder attr rhs expr in
    let expr = List.fold_right ~f:aux exprs ~init:body  in
    return @@ e_lambda ~loc binder lhs_type expr
  | EAnnot {value = (EArith(Int i), _, TVar {value = "nat"; _}); region } -> 
    let ((_,i), loc) = r_split i in
    return @@ e_nat_z ~loc i
  | EAnnot {value = (EArith(Int i), _, TVar {value = "tez"; _}); region } -> 
    let ((_,i), loc) = r_split i in
    let mutez = Z.mul (Z.of_int 1_000_000) i in
    return @@ e_mutez_z ~loc mutez
  | EAnnot {value = (EArith(Int i), _, TVar {value = "mutez"; _}); region } -> 
    let ((_,i), loc) = r_split i in
    return @@ e_mutez_z ~loc i
  | EAnnot {value = (ECodeInj {value = {language; code};_ }, kwd_as, type_expr); region} -> 
    let value: CST.code_inj = {
      language = language;
      code = EAnnot {
        value = code, kwd_as, type_expr;
        region
      }
    } in
    let e = CST.ECodeInj { value; region } in
    compile_expression e
  | EAnnot annot ->
    let (annot, loc) = r_split annot in
    let (expr, _ , ty) = annot in
    let* expr = self expr in
    let* ty   = compile_type_expression ty in
    return @@ e_annotation ~loc expr ty    
  | ECodeInj ci ->
    let (ci, loc) = r_split ci in
    let (language, _) = r_split ci.language in
    let* code = self ci.code in
    return @@ e_raw_code ~loc language code
  | ESeq seq -> (
    let (seq, loc) = r_split seq in
    let* seq = bind_map_list self @@ npseq_to_list seq in
    match seq with
      [] -> return @@ e_unit ~loc ()
    | hd :: tl ->
      let rec aux prev = function
       [] ->  return @@ prev
      | hd :: tl -> bind (return <@ e_sequence ~loc prev) @@ aux hd tl
      in
      aux hd @@ tl
  )

  | EAssign (EVar {value; region} as e1, outer_region, e2) -> 
    let* e1 = compile_expression e1 in
    let* e2 = compile_expression e2 in
    let loc = Location.lift region in
    let outer_loc = Location.lift outer_region in
    ok @@ e_assign ~loc:outer_loc (Location.wrap ~loc @@ Var.of_name value) [] e2
  | EAssign _ as e -> fail @@ not_supported_assignment e
  | ENew {value = (_, e); _} -> fail @@ new_not_supported e

and conv : const:bool -> CST.pattern -> (nested_match_repr,_) result =
  fun ~const p ->
  match p with
  | CST.PWild reg ->
    let loc = Location.lift reg in
    let var = Location.wrap ~loc @@ Var.fresh () in
    ok (PatternVar { var ; ascr = None ; attributes = Stage_common.Helpers.empty_attribute })
  | CST.PVar var ->
    let (var,loc) = r_split var in
    let var = Location.wrap ~loc @@ Var.of_name var in
    let attributes = if const then Stage_common.Helpers.const_attribute else Stage_common.Helpers.var_attribute in
    ok (PatternVar { var ; ascr = None ; attributes })
  | CST.PArray tuple ->
    let (tuple, _loc) = r_split tuple in
    let lst = npseq_to_ne_list tuple.inside in
    let patterns = List.Ne.to_list lst in
    let* nested = bind_map_list (conv ~const) patterns in
    let var = Location.wrap @@ Var.fresh () in
    ok (TupleVar ({var ; ascr = None ; attributes = Stage_common.Helpers.empty_attribute} , nested))
  | _ -> 
    fail @@ unsupported_pattern_type p

and get_binder : nested_match_repr -> AST.ty_expr binder =
  fun s ->
  match s with
  | TupleVar (x,_) -> x
  | PatternVar x -> x
  | RecordVar (x,_,_) -> x

and fold_nested_z
  f acc l =
  match l with
  | [] -> acc
  | ( PatternVar _ as z ) :: l ->
    let x  = f acc z in
    fold_nested_z f x l
  | (TupleVar (_,inner) as z)::l ->
    let x = f acc z in
    let x = fold_nested_z f x inner in
    fold_nested_z f x l
  | (RecordVar (_,_,inner) as z)::l ->
    let x = f acc z in
    let x = fold_nested_z f x inner in
    fold_nested_z f x l

and nestrec : AST.expression -> (AST.expression -> AST.expression) -> nested_match_repr list -> AST.expression =
  fun res f lst ->
    let aux :  (AST.expression -> AST.expression) -> nested_match_repr -> (AST.expression -> AST.expression) =
      fun f z ->
        match z with
        | PatternVar _ -> f
        | TupleVar (matchee,nested) ->
          let binders = List.map ~f:(fun x -> let var = get_binder x in Location.wrap @@ P_var var) nested in
          let pattern = Location.wrap @@ P_tuple binders in
          let f' = fun body -> f (e_matching (e_variable matchee.var) [{pattern ; body}]) in
          f'
        | RecordVar (matchee,labels,nested) ->
          let binders = List.map ~f:(fun x -> let var = get_binder x in Location.wrap @@ P_var var) nested in
          let pattern = Location.wrap @@ P_record (labels, binders) in
          let f' = fun body -> f (e_matching (e_variable matchee.var) [{pattern ; body}]) in
          f'
    in
    match lst with
    | PatternVar _ :: tl -> nestrec res f tl
    | TupleVar (matchee,nested) :: tl ->
      let binders = List.map ~f:(fun x -> let var = get_binder x in Location.wrap @@ P_var var) nested in
      let pattern = Location.wrap @@ P_tuple binders in
      let f' = fun body -> f (e_matching (e_variable matchee.var) [{pattern ; body}]) in
      let f'' = fold_nested_z aux f' nested in
      nestrec res f'' tl
    | RecordVar (matchee,labels,nested) :: tl ->
      let binders = List.map ~f:(fun x -> let var = get_binder x in Location.wrap @@ P_var var) nested in
      let pattern = Location.wrap @@ P_record (labels, binders) in
      let f' = fun body -> f (e_matching (e_variable matchee.var) [{pattern ; body}]) in
      let f'' = fold_nested_z aux f' nested in
      nestrec res f'' tl
    | [] -> f res

and compile_array_let_destructuring : const:bool -> AST.expression -> (CST.pattern, Region.t) Utils.nsepseq CST.brackets Region.reg -> (AST.expression -> AST.expression, _) result =
  fun ~const matchee tuple ->
    let (tuple, loc) = r_split tuple in
    let lst = npseq_to_ne_list tuple.inside in
    let patterns = List.Ne.to_list lst in
    let* patterns = bind_map_list (conv ~const) patterns in
    let binders = List.map ~f:(fun x -> let var = get_binder x in Location.wrap @@ P_var var) patterns in
    let pattern = Location.wrap @@ P_tuple binders in
    let f = fun body -> e_matching ~loc matchee [{pattern ; body}] in
    ok (fun let_result -> nestrec let_result f patterns)

and compile_object_let_destructuring : const:bool -> AST.expression -> (CST.pattern, Region.t) Utils.nsepseq CST.braced CST.reg -> (AST.expression -> AST.expression, _) result =
  fun ~const matchee record ->
    let (record, loc) = r_split record in
    let aux : CST.pattern -> (label * CST.pattern, _) result = fun field ->
      match field with
        PDestruct {value = {property; target; _}; _} ->
          ok @@ (AST.Label property.value, target.value.binders)
      | _ -> 
        fail @@ unsupported_pattern_type field
    in
    let* lst = bind_map_list aux @@ Utils.nsepseq_to_list record.inside in
    let (labels,patterns) = List.unzip lst in
    let* patterns = bind_map_list (conv ~const) patterns in
    let binders = List.map ~f:(fun x -> let var = get_binder x in Location.wrap @@ P_var var) patterns in
    let pattern = Location.wrap @@ P_record (labels, binders) in
    let f = fun body -> e_matching ~loc matchee [{pattern ; body}] in
    ok (fun let_result -> nestrec let_result f patterns)

and compile_parameter : CST.expr ->
    (type_expression binder * (type_expression binder * Types.attributes * expression) list, _) result = fun expr ->
  let return ?ascr loc (exprs: (type_expression binder * Types.attributes * expression) list) var =
    ok ({var=Location.wrap ~loc var; ascr; attributes = Stage_common.Helpers.const_attribute}, exprs) in
  match expr with
  | EPar { value = { inside = ESeq { value = arguments; _ }; _ }; region} ->
    let argument = function
      CST.EAnnot ea ->
        let (ea, loc) = r_split ea in
        let (expr, _, type_expr) : CST.annot_expr = ea in
        let* ascr = compile_type_expression type_expr in
        (match expr with
          CST.EVar ev ->
            return ~ascr loc [] @@ Var.of_name ev.value
        | EArray {value = {inside = array_items; _}; _} ->
            let array_item = function
              CST.Empty_entry reg ->
                let loc = Location.lift reg in
                return loc [] @@ Var.fresh ()
            | Expr_entry EVar e ->
                let (var,loc) = r_split e in
                return loc [] @@ Var.of_name var
            | Rest_entry _ as r -> fail @@ array_rest_not_supported r
            | _ -> fail @@ not_a_valid_parameter expr
            in 

            let* lst = bind_map_ne_list array_item @@ npseq_to_ne_list array_items in
            let (lst,exprs) = List.Ne.unzip lst in
            let var, expr = match lst with
              {var;ascr}, [] ->
              Location.unwrap var, []
            | var, lst ->
              let binder = Var.fresh () in
              let aux (i: Z.t) (b: type_expression binder) =
                Z.add i Z.one,
                (b, [], e_accessor (e_variable @@ Location.wrap ~loc binder) @@ [Access_tuple i])
              in
              binder,
              snd @@ List.fold_map ~f:aux ~init:Z.zero @@ var :: lst
            in
            let exprs = List.concat @@ expr :: List.Ne.to_list exprs in
            return ~ascr loc exprs @@ var
        | _ -> fail @@ not_a_valid_parameter expr
        )
    | _ as e -> fail @@ not_a_valid_parameter e
    in
    let* lst = bind_map_ne_list argument @@ npseq_to_ne_list arguments in
    let (lst,exprs) = List.Ne.unzip lst in
    let loc = Location.lift region in
    let var, ascr, expr = match lst with
      {var;ascr}, [] ->
      Location.unwrap var, ascr, []
    | var, lst ->
      let binder = Var.fresh () in
      let aux (i: Z.t) (b: type_expression binder) =
        Z.add i Z.one,
        (b, [], e_accessor (e_variable @@ Location.wrap ~loc binder) @@ [Access_tuple i])
      in
      binder,
      Option.map ~f:(t_tuple ~loc) @@ Option.all @@ List.map ~f:(fun e -> e.ascr) @@ var::lst,
      snd @@ List.fold_map ~f:aux ~init:Z.zero @@ var :: lst
    in
    let exprs = List.concat @@ expr :: List.Ne.to_list exprs in
    return ?ascr loc exprs @@ var

  | EVar var ->
    let (var,loc) = r_split var in
    return loc [] @@ Var.of_name var
  | EUnit the_unit ->
    let loc = Location.lift the_unit.region in
    return ~ascr:(t_unit ~loc ()) loc [] @@ Var.fresh ()
  | _ -> fail @@ not_a_valid_parameter expr


and compile_function_body_to_expression : CST.fun_expr_body -> (AST.expression, _) result = fun body ->
  match body with
  | FunctionBody statements -> compile_statements_to_expression statements.value.inside
  | ExpressionBody expr -> compile_expression expr

and compile_let_to_declaration : const:bool -> CST.let_binding Region.reg -> (AST.declaration list, _) result = fun ~const let_binding ->
  let ({binders; lhs_type; expr = let_rhs; attributes; _} : CST.let_binding) = let_binding.value in
  let* lst = compile_let_binding ~const attributes let_rhs lhs_type binders let_binding.region in
  let aux (name, binder,attr, expr) =  AST.Declaration_constant {name; binder; attr; expr} in
  ok @@ List.map ~f:aux lst

(*
  JsLIGO has statements. There are two cases when compiling a statement:
  - A `return` statement are easy: the resulting expression is just the
    content of the return
  - `let` and `const` are subtler. There are no expression corresponding to
    `const x = 42 ;`. The result of compiling this statement is actually the
    function that takes `body` as a parameter and returns `let x = 42 in body`
*)

and merge_statement_results : statement_result -> statement_result -> statement_result = fun f s ->
  match f, s with
    Binding a, Binding b -> Binding (a <@ b)
  | Binding a, Expr    b -> Expr (a b)
  | Binding a, Break   b -> Break (a @@ e_unit ())
  | Binding a, Return  b -> Return (a b)
  
  | Expr    a, Binding b -> Binding (e_sequence a <@ b )
  | Expr    a, Expr    b -> Expr (e_sequence a b)
  | Expr    a, Break   b -> Break a
  | Expr    a, Return  b -> Return (e_sequence a b)
  | Break   a, _ ->         Break a
  | Return  a, _ ->         Return a

and is_failwith_call = function
  {expression_content = E_constant {cons_name;_}; _} -> constant_to_string cons_name = "failwith"
| {expression_content = E_ascription {anno_expr; _}; _} -> 
  is_failwith_call anno_expr
| _ -> 
  false

and compile_pattern : const:bool -> CST.pattern -> (type_expression binder * (_ -> _), _) result =
  fun ~const pattern ->
  let return ?ascr loc fun_ var attributes =
    ok ({var=Location.wrap ~loc var; ascr; attributes}, fun_) in
  let return_1 ?ascr loc var = return ?ascr loc (fun e -> e) var in
  match pattern with 
    PVar var -> 
    let (var,loc) = r_split var in
    let attributes = if const then Stage_common.Helpers.const_attribute else Stage_common.Helpers.var_attribute in
    return_1 loc (Var.of_name var) attributes
  | PWild p -> 
    let loc = Location.lift p in
    return_1 loc (Var.fresh ()) Stage_common.Helpers.empty_attribute
  | PArray tuple ->
    let (tuple, loc) = r_split tuple in
    let var = Var.fresh () in
    let aux (binder_lst, fun_) pattern =
      let* (binder, fun_') = compile_pattern ~const pattern in
      ok @@ (binder :: binder_lst, fun_' <@ fun_)
    in
    let* binder_lst, fun_ = bind_fold_right_list aux ([], fun e -> e) @@ Utils.nsepseq_to_list tuple.inside in
    let expr = fun expr -> e_matching_tuple (e_variable @@ Location.wrap var) binder_lst @@ fun_ expr in
    return loc expr var Stage_common.Helpers.empty_attribute
  | _ -> 
    fail @@ unsupported_pattern_type pattern

and compile_let_binding: const:bool -> CST.attributes -> CST.expr -> (Region.t * CST.type_expr) option -> CST.pattern -> Region.t -> (('a * type_expression binder * Ast_imperative__.Types.attributes * expression) list, _) result = 
  fun ~const attributes let_rhs type_expr binders region ->     
  let attributes = compile_attributes attributes in
  let* expr = compile_expression let_rhs in
  let* lhs_type =
      bind_map_option (compile_type_expression <@ snd) type_expr in
  let aux = function
  | CST.PVar name -> (*function or const *)
    let fun_binder = compile_variable name in
    let* expr = (match let_rhs with 
      CST.EFun _ ->
        let* lambda = trace_option (recursion_on_non_function expr.location) @@ get_e_lambda expr.expression_content in
        let lhs_type = (match lhs_type with 
        | Some lhs_type -> Some lhs_type
        | None ->  Option.map ~f:(Utils.uncurry t_function) @@ Option.bind_pair (lambda.binder.ascr, lambda.output_type)) in
        let* fun_type = trace_option (untyped_recursive_fun name.region) @@ lhs_type in
        ok @@ e_recursive ~loc:(Location.lift name.region) fun_binder fun_type lambda
      | _ -> ok @@ expr 
      )
    in
    let var_attributes = if const then Stage_common.Helpers.const_attribute else Stage_common.Helpers.var_attribute in
    ok @@ [(Some name.value, {var=fun_binder;ascr=lhs_type;attributes = var_attributes}, attributes, expr)]
  | CST.PArray a ->  (* tuple destructuring (for top-level only) *)
    let matchee = expr in
    let (tuple, loc) = r_split a in
    let array_items = npseq_to_list tuple.inside in
    let* lst = bind_map_list (compile_pattern ~const) array_items in
    let (lst, exprs) = List.unzip lst in
    let expr = List.fold_right ~f:(@@) exprs ~init:matchee in
    let aux i binder = Z.add i Z.one, (None, binder, attributes, e_accessor expr @@ [Access_tuple i]) in
    let lst = snd @@ List.fold_map ~f:aux ~init:Z.zero @@ lst in
    ok @@ lst
  | _ -> fail @@ unsupported_pattern_type @@ binders
  in 
  aux binders

and compile_statements : CST.statements -> (statement_result, _) result = fun statements ->
  let rec aux result = function
    (_, hd) :: tl ->
      let wrapper = CST.SBlock {
        value = {
          inside = (hd, tl); 
          lbrace = Region.ghost; 
          rbrace = Region.ghost}; 
          region = Region.ghost
      } in
      let* block = compile_statement wrapper in
      aux (merge_statement_results result block) []
  | [] -> ok result
  in
  let hd  = fst statements in
  let snd = snd statements in
  let* init = compile_statement hd in
  aux init snd

and compile_statement : CST.statement -> (statement_result, _) result = fun statement ->
  let self = compile_statement in
  let self_expr = compile_expression in
  let self_statements = compile_statements in
  let binding e = ok @@ Binding (fun f -> e f) in
  let expr e = ok @@ Expr e in
  let return r = ok @@ Return r in
  let compile_initializer ~const ({value = {binders; lhs_type; expr = let_rhs; attributes}; region} : CST.let_binding Region.reg) : (expression -> expression, _) result = 
    match binders with 
      PArray array ->
      let* matchee = compile_expression let_rhs in
      compile_array_let_destructuring ~const matchee array 
    | PObject o ->
      let* matchee = compile_expression let_rhs in
      compile_object_let_destructuring ~const matchee o
    | _ -> 
      let* lst = compile_let_binding ~const attributes let_rhs lhs_type binders region in
      let aux (_name,binder,attr,rhs) expr = e_let_in ~loc: (Location.lift region) binder attr rhs expr in
      ok @@ fun init -> List.fold_right ~f:aux ~init lst
  in
  let rec initializers ~const (result: expression -> expression) (rem: (Region.t * CST.let_binding Region.reg) list) : (expression -> expression, _) result =
    match rem with
    | (_, hd) :: tl -> 
      let* init = compile_initializer ~const hd in
      let new_result = result <@ init in
      initializers ~const new_result tl
    | [] -> ok result
  in
  match statement with
  | SExpr e -> 
    let* e = self_expr e in
    expr e
  | SBlock {value = {inside; _}; region} -> 
    let* statements = self_statements inside in
    ok statements
  | SCond cond ->
    let (cond, loc) = r_split cond in
    let* test         = self_expr cond.test.inside in
    let* then_clause  = self cond.ifso in 
    let* else_clause = bind_map_option (fun (_, s) -> self s) cond.ifnot in
    let compile_clause = function 
      Binding e -> expr, (e @@ e_unit ())
    | Expr e when is_failwith_call e -> return, e
    | Expr e -> expr, (e_sequence e (e_unit ()))    
    | Break b -> return, (e_sequence b (e_unit ()))
    | Return r -> return, r
    in
    let (m, then_clause) = compile_clause then_clause in
    let (m, else_clause) = (match else_clause with 
        Some s -> let a, b = compile_clause s in (a, b)
      | None -> m, e_unit ()
    ) in
    m (e_cond ~loc test then_clause else_clause)
  | SReturn {value = {expr; _}; region} -> (
    match expr with 
      Some v -> 
        let* expr = compile_expression v in
        return expr
    | None -> 
        return (e_unit ~loc:(Location.lift region) ())
    )
  | SLet li ->
    (* TODO: ensure assignment can only happen to let values, not const values. *)
    let (li, loc) = r_split li in
    let {bindings; _} : CST.let_ = li in

    let hd = fst bindings in
    let tl = snd bindings in
    let* init = compile_initializer ~const:false hd in
    let* initializers' = initializers ~const:false init tl in 
    binding initializers'
  | SConst li ->
    let (li, loc) = r_split li in
    let {bindings; _} : CST.const_ = li in
    let hd = fst bindings in
    let tl = snd bindings in
    let* init = compile_initializer ~const:true hd in
    let* initializers' = initializers ~const:true init tl in 
    binding initializers'
  | SSwitch s -> fail @@ switch_not_supported s
  | SBreak b -> fail @@ break_not_implemented b
  | SType ti -> 
    let (ti, loc) = r_split ti in
    let ({name;type_expr;_}: CST.type_decl) = ti in
    let type_binder = Var.of_name name.value in
    let* rhs = compile_type_expression type_expr in
    binding (e_type_in ~loc type_binder rhs)
  | SNamespace n -> 
    let ((m, name, rhs), loc) = r_split n in
    let module_binder = name.value in
    let* rhs = compile_namespace rhs.value.inside in
    binding (e_mod_in ~loc module_binder rhs)
  | SExport e -> 
    let ((_, statement), _) = r_split e in
    compile_statement statement
  | SImport i ->
    let (({alias; module_path; _}: CST.import), loc) = r_split i in
    let start = (fst module_path).value in
    let rest = List.map ~f:(fun (_, (b: _ Region.reg)) -> b.value) (snd module_path) in
    let x = (start, rest) in
    binding (e_mod_alias ~loc alias.value x)
  | SForOf s -> 
    let (v, loc) = r_split s in
    let binder = (
      Location.wrap ~loc @@ Var.of_name v.name.value,
      None
    )
    in
    let* collection  = compile_expression v.expr in
    let* sr = compile_statement v.statement in
    let* body = statement_result_to_expression sr in     
    binding (e_sequence (e_for_each ~loc binder collection Any body))
  | SWhile e -> 
    let (w, loc) = r_split e in
    let* cond = compile_expression w.expr in
    let* statement_result = compile_statement w.statement in
    let* body = statement_result_to_expression statement_result in
    binding (e_sequence (e_while ~loc cond body))

and statement_result_to_expression: statement_result -> (AST.expression, _) result = fun statement_result ->
  ok @@ (match statement_result with 
    Binding b -> b (e_unit ())
  | Expr e -> e_sequence e (e_unit ())
  | Break r
  | Return r -> r)

and compile_statements_to_expression : CST.statements -> (AST.expression, _) result = fun statements ->
  let* statement_result = compile_statements statements in
  statement_result_to_expression statement_result
  
and compile_statement_to_declaration : CST.statement -> (AST.declaration list, _) result = fun statement ->
  match statement with
  | SType {value; _} ->
    let name = value.name.value in
    let* type_expr = compile_type_expression value.type_expr in
    ok @@ [AST.Declaration_type {type_binder = Var.of_name name; type_expr}]
  | SLet {value = {bindings;_ }; _} -> (
    let fst_binding = fst bindings in
    let* fst_binding = compile_let_to_declaration ~const:false fst_binding in
    let bindings = List.map ~f:(fun (_, b) -> b) @@ snd bindings in
    let rec aux result = function
      binding :: remaining -> 
        let* d = compile_let_to_declaration ~const:false binding in
        aux (d @ result) remaining
    | [] -> ok @@ List.rev result
    in 
    aux fst_binding bindings
  )
  | SConst {value = {bindings; _}; _} -> (
    let fst_binding = fst bindings in
    let* fst_binding = compile_let_to_declaration ~const:true fst_binding in
    let bindings = List.map ~f:(fun (_, b) -> b) @@ snd bindings in
    let rec aux result = function
      binding :: remaining -> 
        let* d = compile_let_to_declaration ~const:true binding in
        aux (d @ result) remaining
    | [] -> ok @@ List.rev result
    in 
    aux fst_binding bindings
  )
  | SNamespace {value = (_, ident, {value = {inside = statements; _}; _}); _} ->
    let (name,_) = r_split ident in
    let* module_ = compile_namespace statements in
    ok @@ [AST.Declaration_module  {module_binder=name; module_}]
  | SImport {value = {alias; module_path; _}; _} ->
    let (alias,_)   = r_split alias in
    let binders,_ = List.Ne.unzip @@ List.Ne.map r_split @@ npseq_to_ne_list module_path in
    ok @@ [AST.Module_alias {alias; binders}]
  | SExport {value = (_, s); _} -> compile_statement_to_declaration s
  | _ ->
    fail @@ statement_not_supported_at_toplevel statement

and compile_statements_to_program : CST.ast -> (AST.module_, _) result = fun ast ->
  let aux : CST.toplevel_statement -> (declaration location_wrap list, _) result = fun statement ->
    match statement with 
      TopLevel (statement, _) -> 
        let* declarations = compile_statement_to_declaration statement in  
        ok @@ List.map ~f:(fun d -> 
          let loc = Location.lift @@ CST.statement_to_region statement in
          Location.wrap ~loc d
        ) declarations
    | Directive _ ->
      ok []
  in
  let statements = nseq_to_list ast.statements in
  let* declarations = bind_map_list aux statements in
  let lst = List.concat declarations in
  ok lst


and compile_namespace : CST.statements -> (AST.module_, _) result = fun statements ->
  let aux : CST.statement -> (declaration location_wrap list, _) result = fun statement ->
    let* declarations = compile_statement_to_declaration statement in
    ok @@ List.map ~f:(fun d -> 
      let loc = Location.lift @@ CST.statement_to_region statement in
      Location.wrap ~loc d
    ) declarations
  in
  let statements = Utils.nsepseq_to_list statements in
  let* declarations = bind_map_list aux statements in
  let lst = List.concat declarations in
  ok lst

let compile_module : CST.ast -> _ result =
  fun t -> 
    compile_statements_to_program t
