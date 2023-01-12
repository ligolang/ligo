open Errors
open Simple_utils.Trace
open Simple_utils.Function
module Utils = Simple_utils.Utils
module CST = Cst.Jsligo
module AST = Ast_imperative
module Token = Lexing_jsligo.Token
open Ligo_prim
open AST

let nseq_to_list (hd, tl) = hd :: tl
let npseq_to_list (hd, tl) = hd :: List.map ~f:snd tl
let npseq_to_ne_list (hd, tl) = hd, List.map ~f:snd tl
let r_split = Location.r_split

let compile_variable var =
  let var, loc = r_split var in
  Value_var.of_input_var ~loc var


let compile_type_var var =
  let var, loc = r_split var in
  Type_var.of_input_var ~loc var


let compile_mod_var var =
  let var, loc = r_split var in
  Module_var.of_input_var ~loc var


let compile_attributes attributes : string list =
  let lst = List.map ~f:(fst <@ r_split) attributes in
  List.map lst ~f:(fun attr ->
      (* this shouldnt be necessary *)
      match String.chop_prefix attr ~prefix:"@" with
      | Some s -> s
      | None -> attr)


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

  type type_compiler_opt = CST.type_expr -> AST.type_expression option

  let rec get_t_int_singleton_opt = function
    | CST.TInt x ->
      let _, z = x.value in
      Some z
    | _ -> None


  and get_t_string_singleton_opt = function
    | CST.TString s -> Some s.value
    | _ -> None


  (*
    This chains the application of multiple `type_compiler_opt`. If the first returns `None`, use
    the next one, etc.
  *)
  and type_compiler_opt_list : type_compiler_opt list -> type_compiler_opt =
   fun compilers te ->
    match compilers with
    | [] -> None
    | hd :: tl ->
      let x = hd te in
      (match x with
      | Some x -> Some x
      | None -> type_compiler_opt_list tl te)


  (*
    `try_type_compilers compilers type_expression other` will try to run the `compilers` on
    `type_expression`. If they all return `None`, it will run `other` instead.
  *)
  and try_type_compilers
      :  type_compiler_opt list -> CST.type_expr -> (unit -> AST.type_expression)
      -> AST.type_expression
    =
   fun compilers te other ->
    let x = type_compiler_opt_list compilers te in
    match x with
    | Some x -> x
    | None -> other ()


  and compile_type_function_args ~loc ~raise : CST.fun_type_args -> type_expression =
   fun args ->
    let unpar = args.inside in
    let hd, tl_sep = unpar in
    let tl = List.map ~f:snd tl_sep in
    let aux : CST.fun_type_arg -> type_expression =
     fun x -> compile_type_expression ~raise x.type_expr
    in
    let lst = List.map ~f:aux (hd :: tl) in
    match lst with
    | [ a ] -> a
    | lst -> t_tuple ~loc lst


  and compile_sapling ~raise : type_compiler_opt =
   fun te ->
    match te with
    | TApp app ->
      let (operator, args), loc = r_split app in
      (match operator.value with
      | "sapling_state" ->
        let lst = npseq_to_list args.value.inside in
        (match lst with
        | [ (a : CST.type_expr) ] ->
          let sloc = Location.lift @@ Raw.type_expr_to_region a in
          let a' =
            trace_option ~raise (michelson_type_wrong te operator.value)
            @@ get_t_int_singleton_opt a
          in
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          Some (t_sapling_state ~loc singleton)
        | _ -> raise.error @@ michelson_type_wrong_arity loc operator.value)
      | "sapling_transaction" ->
        let lst = npseq_to_list args.value.inside in
        (match lst with
        | [ (a : CST.type_expr) ] ->
          let sloc = Location.lift @@ Raw.type_expr_to_region a in
          let a' =
            trace_option ~raise (michelson_type_wrong te operator.value)
            @@ get_t_int_singleton_opt a
          in
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          Some (t_sapling_transaction ~loc singleton)
        | _ -> raise.error @@ michelson_type_wrong_arity loc operator.value)
      | _ -> None)
    | _ -> None


  (* this is a bad design, michelson_or and pair should be an operator
  see AnnotType *)
  and compile_michelson_pair_or ~raise : type_compiler_opt =
   fun te ->
    match te with
    | TApp app ->
      let (operator, args), loc = r_split app in
      (match operator.value with
      | "michelson_or" ->
        let lst = npseq_to_list args.value.inside in
        let lst =
          match lst with
          | [ TProd a ] -> npseq_to_list a.inside.value.inside
          | _ -> raise.error @@ michelson_type_wrong_arity loc operator.value
        in
        (match lst with
        | [ a; b; c; d ] ->
          let b' =
            trace_option ~raise (michelson_type_wrong te operator.value)
            @@ get_t_string_singleton_opt b
          in
          let d' =
            trace_option ~raise (michelson_type_wrong te operator.value)
            @@ get_t_string_singleton_opt d
          in
          let a' = compile_type_expression ~raise a in
          let c' = compile_type_expression ~raise c in
          Some (t_michelson_or ~loc a' b' c' d')
        | _ -> raise.error @@ michelson_type_wrong_arity loc operator.value)
      | "michelson_pair" ->
        let lst = npseq_to_list args.value.inside in
        let lst =
          match lst with
          | [ TProd a ] -> npseq_to_list a.inside.value.inside
          | _ -> raise.error @@ michelson_type_wrong_arity loc operator.value
        in
        (match lst with
        | [ a; b; c; d ] ->
          let b' =
            trace_option ~raise (michelson_type_wrong te operator.value)
            @@ get_t_string_singleton_opt b
          in
          let d' =
            trace_option ~raise (michelson_type_wrong te operator.value)
            @@ get_t_string_singleton_opt d
          in
          let a' = compile_type_expression ~raise a in
          let c' = compile_type_expression ~raise c in
          Some (t_michelson_pair ~loc a' b' c' d')
        | _ -> raise.error @@ michelson_type_wrong_arity loc operator.value)
      | _ -> None)
    | _ -> None


  and compile_type_expression ~raise : CST.type_expr -> type_expression =
   fun te ->
    let self = compile_type_expression ~raise in
    let return te = te in
    (* This is not efficient. It would make more sense to split each type_compiler in their own match branch. *)
    try_type_compilers [ compile_sapling ~raise; compile_michelson_pair_or ~raise ] te
    @@ fun () ->
    let region = CST.type_expr_to_region te in
    match te with
    | TSum sum ->
      let sum_type, loc = r_split sum in
      let ({ variants; attributes; _ } : CST.sum_type) = sum_type in
      let lst = npseq_to_list variants.value in
      let attr = compile_attributes attributes in
      let aux (v : CST.variant Region.reg) : string * type_expression * string list =
        let variant = v.value in
        let variant_comp = variant.tuple.value.inside in
        let compile_params_to_type_expr b =
          match b with
          | (_ as f), [] -> self @@ f
          | _ ->
            let cartesian : CST.cartesian =
              { inside =
                  { value =
                      { lbracket = variant.tuple.value.lbracket
                      ; inside = b
                      ; rbracket = variant.tuple.value.rbracket
                      }
                  ; region = Region.ghost
                  }
              ; attributes = variant.attributes
              }
            in
            self @@ TProd cartesian
        in
        let te =
          match variant_comp.params with
          | Some (_, b) -> compile_params_to_type_expr b
          | None -> t_unit ~loc ()
        in
        let attributes = compile_attributes variant.attributes in
        let constructor, type_expr, variant_attr =
          variant_comp.constr.value, te, attributes
        in
        (* type_expression_to_constructor ~raise v in *)
        constructor, type_expr, variant_attr
      in
      let sum = List.map ~f:aux lst in
      return @@ t_sum_ez_attr ~loc ~attr sum
    | TObject record ->
      let injection, loc = r_split record in
      let attributes = compile_attributes injection.attributes in
      let lst = npseq_to_list injection.ne_elements in
      let aux (field : CST.field_decl CST.reg) =
        let f, _ = r_split field in
        let type_expr = self f.field_type in
        let field_attr = compile_attributes f.attributes in
        return @@ (f.field_name.value, type_expr, field_attr)
      in
      let fields = List.map ~f:aux lst in
      return @@ t_record_ez_attr ~loc ~attr:attributes fields
    | TProd prod ->
      let nsepseq, loc = r_split prod.inside in
      let lst = npseq_to_list nsepseq.inside in
      let lst = List.map ~f:self lst in
      return @@ t_tuple ~loc lst
    | TApp app ->
      let (operator, args), loc = r_split app in
      let operator = compile_type_var operator in
      let lst = npseq_to_list args.value.inside in
      let lst = List.map ~f:self lst in
      return @@ t_app ~loc operator lst
    | TFun func ->
      let (input_type, _, output_type), loc = r_split func in
      let input_type = compile_type_function_args ~loc ~raise input_type in
      let output_type = self output_type in
      return @@ t_arrow ~loc input_type output_type
    | TPar par ->
      let par, _ = r_split par in
      let type_expr = par.inside in
      self type_expr
    | TVar var ->
      let name, loc = r_split var in
      let v = Type_var.of_input_var ~loc name in
      return @@ t_variable ~loc v
    | TString _s -> raise.error @@ unsupported_string_singleton te
    | TInt _s -> raise.error @@ unsupported_string_singleton te
    | TModA ma ->
      let ma, loc = r_split ma in
      let module_name = compile_mod_var ma.module_name in
      let rec aux : Module_var.t list -> CST.type_expr -> AST.type_expression =
       fun acc exp ->
        match exp with
        | TVar v ->
          let accessed_el = compile_type_var v in
          t_module_accessor ~loc acc accessed_el
        | TModA ma ->
          aux
            (acc @ [ Module_var.of_input_var ~loc ma.value.module_name.value ])
            ma.value.field
        | _ -> raise.error (expected_a_variable (CST.type_expr_to_region ma.field))
      in
      return @@ aux [ module_name ] ma.field
    | TDisc n ->
      let loc = Location.lift region in
      let shared_field = Discriminated_union.get_shared_field ~raise n in
      let sum_fields =
        Utils.nsepseq_map
          (fun (obj : CST.obj_type) ->
            (* split into constructor and fields *)
            let constructor, fields =
              List.partition_map (npseq_to_list obj.value.ne_elements) ~f:(fun x ->
                  if String.equal x.value.field_name.value shared_field
                  then (
                    let t = x.value.field_type in
                    match t with
                    | TString s -> First s
                    | _ -> raise.error @@ unexpected)
                  else Second x)
            in
            let constructor =
              match constructor with
              | hd :: _ -> hd.value
              | _ -> raise.error @@ no_shared_fields obj.region
            in
            (* create the object type without the constructor field *)
            let type_expr =
              match fields with
              | hd :: tl ->
                let ne_elements =
                  hd, List.map ~f:(fun l -> Token.wrap_semi Region.ghost, l) tl
                in
                let obj =
                  CST.TObject { obj with value = { obj.value with ne_elements } }
                in
                self obj
              | [] -> t_unit ~loc ()
            in
            let fields = List.map ~f:(fun x -> x.value.field_name.value) fields in
            constructor, type_expr, fields)
          n
      in
      let sum_fields = npseq_to_list sum_fields in
      let sum, disc_union =
        List.fold_left
          ~f:(fun (all_sum, all_fields) (constructor, type_expr, fields) ->
            ( (constructor, type_expr, []) :: all_sum
            , Discriminated_union.
                { constructor
                ; constructor_field = shared_field
                ; has_payload = Poly.(type_expr <> AST.t_unit ~loc ())
                ; fields
                }
              :: all_fields ))
          ~init:([], [])
          sum_fields
      in
      Discriminated_union.add disc_union;
      return @@ t_sum_ez_attr ~loc ~attr:[] sum
end

open Compile_type

let is_recursive_lambda : Value_var.t -> _ Lambda.t -> bool =
 fun name lambda ->
  let open AST.Helpers in
  VarSet.mem name (Free_variables.lambda lambda)


let expression_to_variable ~raise : CST.expr -> CST.variable = function
  | EVar var -> var
  | _ as e -> raise.error @@ expected_a_variable (CST.expr_to_region e)


let compile_expression_to_int ~raise : CST.expr -> Z.t = function
  | EArith (Int i) -> snd i.value
  | _ as e -> raise.error @@ expected_an_int e


let compile_selection ~raise : CST.selection -> _ Access_path.access * Location.t =
 fun selection ->
  match selection with
  | FieldName name ->
    let name, loc = r_split name in
    Access_record name.value.value, loc
  | Component comp ->
    let index_expr, loc = r_split comp in
    let index = compile_expression_to_int ~raise index_expr.inside in
    Access_tuple index, loc


let array_item_to_expression ~raise : CST.array_item -> CST.expr = function
  | Expr_entry expr -> expr
  | Rest_entry _ as r -> raise.error @@ expected_an_expression r


let arguments_to_expr_nseq (args : CST.arguments) : CST.expr Utils.nseq * Location.t =
  match args with
  | Unit the_unit -> (CST.EUnit the_unit, []), Location.lift the_unit.region
  | Multiple xs ->
    let hd, tl = xs.value.inside in
    (hd, List.map ~f:snd tl), Location.lift xs.region


type statement_result =
  | Binding of (AST.expression -> AST.expression)
  | Binding_return of (AST.expression -> AST.expression)
  | Expr of AST.expression
  | Break of AST.expression
  | Return of AST.expression

type constr_types =
  | Match_nil of AST.expression
  | Match_cons of Value_var.t * Value_var.t

let rec compile_tuple_expression ~raise ~loc tuple_expr =
  let lst =
    List.map ~f:(fun e -> compile_expression ~raise e) @@ nseq_to_list tuple_expr
  in
  match lst with
  | [ hd ] -> hd
  | lst -> e_tuple ~loc lst


and compile_arguments ~raise (args : CST.arguments) =
  let args, loc = arguments_to_expr_nseq args in
  compile_tuple_expression ~raise ~loc args


and compile_bin_op ~raise (op_type : Constant.constant') (op : _ CST.bin_op CST.reg) =
  let self = compile_expression ~raise in
  let return e = e in
  let op, loc = r_split op in
  let a = self op.arg1 in
  let b = self op.arg2 in
  return @@ e_constant ~loc (Const op_type) [ a; b ]


and compile_un_op ~raise (op_type : Constant.constant') (op : _ CST.un_op CST.reg) =
  let self = compile_expression ~raise in
  let return e = e in
  let op, loc = r_split op in
  let arg = self op.arg in
  return @@ e_constant ~loc (Const op_type) [ arg ]


and compile_expression ~raise : CST.expr -> AST.expr =
 fun e ->
  let self : CST.expr -> AST.expr = compile_expression ~raise in
  let return e = e in
  match e with
  | EVar var ->
    let var, loc = r_split var in
    return @@ e_variable_ez ~loc var
  | EPar par -> self par.value.inside
  | EUnit the_unit ->
    let loc = Location.lift the_unit.region in
    return @@ e_unit ~loc ()
  | EBytes bytes ->
    let bytes, loc = r_split bytes in
    let _s, b = bytes in
    return @@ e_bytes_hex ~loc b
  | EString str ->
    (match str with
    | String str ->
      let str, loc = r_split str in
      return @@ e_string ~loc str
    | Verbatim str ->
      let str, loc = r_split str in
      return @@ e_verbatim ~loc str)
  | EArith arth ->
    (match arth with
    | Add plus -> compile_bin_op ~raise C_POLYMORPHIC_ADD plus
    | Sub minus -> compile_bin_op ~raise C_POLYMORPHIC_SUB minus
    | Mult times -> compile_bin_op ~raise C_MUL times
    | Div slash -> compile_bin_op ~raise C_DIV slash
    | Mod mod_ -> compile_bin_op ~raise C_MOD mod_
    | Neg minus -> compile_un_op ~raise C_NEG minus
    | Int i ->
      let (_, i), loc = r_split i in
      return @@ e_int_z ~loc i)
  | ELogic logic ->
    (match logic with
    | BoolExpr be ->
      (match be with
      | Or or_ -> compile_bin_op ~raise C_OR or_
      | And and_ -> compile_bin_op ~raise C_AND and_
      | Not not_ -> compile_un_op ~raise C_NOT not_)
    | CompExpr ce ->
      (match ce with
      | Lt lt -> compile_bin_op ~raise C_LT lt
      | Leq le -> compile_bin_op ~raise C_LE le
      | Gt gt -> compile_bin_op ~raise C_GT gt
      | Geq ge -> compile_bin_op ~raise C_GE ge
      | Equal eq -> compile_bin_op ~raise C_EQ eq
      | Neq ne -> compile_bin_op ~raise C_NEQ ne))
  | ECall
      { value =
          ( EVar { value = "list"; _ }
          , Multiple
              { value =
                  { inside =
                      ( EArray
                          { value =
                              { inside =
                                  Some
                                    ( Expr_entry e
                                    , [ (_, Rest_entry { value = { expr; _ }; _ }) ] )
                              ; _
                              }
                          ; _
                          }
                      , [] )
                  ; _
                  }
              ; _
              } )
      ; region
      } ->
    let loc = Location.lift region in
    let a = self e in
    let b = self expr in
    return @@ e_constant ~loc (Const C_CONS) [ a; b ]
  | ECall
      { value =
          ( EVar { value = "list"; _ }
          , Multiple
              { value =
                  { inside =
                      ( EArray
                          { value = { inside; lbracket = _; rbracket = _ }; region = _ }
                      , [] )
                  ; _
                  }
              ; _
              } )
      ; region
      } ->
    let loc = Location.lift region in
    let items =
      match inside with
      | Some items -> Utils.nsepseq_to_list items
      | None -> []
    in
    let lst =
      List.map
        ~f:(fun e ->
          match e with
          | CST.Expr_entry e -> self e
          | Rest_entry _ -> raise.error (array_rest_not_supported e))
        items
    in
    return @@ e_list ~loc lst
  | ECall
      { value =
          ( EVar { value = "match"; _ }
          , Multiple
              { value =
                  { inside =
                      ( input
                      , [ (_, EObject { value = { inside = fields; _ }; region = finish })
                        ] )
                  ; _
                  }
              ; _
              } )
      ; region = start
      } ->
    (* Pattern matching for JsLIGO is implemented as a 'built-in function' as
       JavaScript and TypeScript don't have native pattern matching. *)
    let fields' = Utils.nsepseq_to_list fields in
    let compile_simple_pattern p =
      let rec aux = function
        | CST.EVar v -> Some (compile_variable v), v.region
        | EPar par -> aux par.value.inside
        | ESeq { value = hd, []; _ } -> aux hd
        | EAnnot { value = a, _, _; _ } -> aux a
        | EUnit u -> None, u.region
        | _ as e -> raise.error @@ unsupported_match_pattern e
      in
      aux p
    in
    let compile_constr_pattern = function
      | CST.Property { value = { name = EVar { value = constr; region }; value; _ }; _ }
        ->
        (match value with
        | EFun { value = { parameters; body; _ }; _ } ->
          let loc = Location.lift region in
          let parameters_opt, parameters_region = compile_simple_pattern parameters in
          let expr = compile_function_body_to_expression ~loc ~raise body in
          region, (Label.of_string constr, parameters_opt, parameters_region), expr
        | _ as e -> raise.error @@ invalid_case constr e (* TODO: improve error message *))
      | _ as f -> raise.error @@ unsupported_match_object_property f
    in
    let loc1 = Location.lift start in
    let loc2 = Location.lift finish in
    let loc = Location.cover loc1 loc2 in
    let matchee = self input in
    let constrs = List.map ~f:compile_constr_pattern fields' in
    let cases =
      List.map
        ~f:(fun (region, (constructor, p_opt, p_region), body) ->
          let loc = Location.lift region in
          let ploc = Location.lift p_region in
          let pvar =
            match p_opt with
            | Some var -> Pattern.P_var (Binder.make var None)
            | None -> P_unit
          in
          let pattern =
            Location.wrap ~loc
            @@ Pattern.P_variant (constructor, Location.wrap ~loc:ploc pvar)
          in
          ({ body; pattern } : _ Match_expr.match_case))
        constrs
    in
    e_matching ~loc matchee cases
  | ECall
      { value =
          ( EVar { value = "match"; _ }
          , Multiple
              { value =
                  { inside =
                      ( input
                      , [ ( _
                          , ECall
                              { value =
                                  ( EVar { value = "list"; _ }
                                  , Multiple
                                      { value =
                                          { inside =
                                              CST.EArray { value = { inside; _ }; _ }, _
                                          ; _
                                          }
                                      ; _
                                      } )
                              ; _
                              } )
                        ] )
                  ; _
                  }
              ; _
              } )
      ; region
      } ->
    let args =
      match inside with
      | Some inside -> Utils.nsepseq_to_list inside
      | None -> []
    in
    let compile_simple_pattern p =
      let rec aux = function
        | CST.EVar v -> compile_variable v
        | EPar par -> aux par.value.inside
        | ESeq { value = hd, []; _ } -> aux hd
        | EAnnot { value = a, _, _; _ } -> aux a
        | EUnit the_unit ->
          let _, loc = r_split the_unit in
          Value_var.of_input_var ~loc "()"
        | _ as e -> raise.error @@ unsupported_match_pattern e
      in
      aux p
    in
    let rec compile_parameter = function
      | CST.EPar p -> compile_parameter p.value.inside
      | ESeq
          { value =
              EAnnot { value = EArray { value = { inside = None; _ }; _ }, _, _; _ }, _
          ; region
          }
      | ESeq { value = EArray { value = { inside = None; _ }; _ }, _; region } ->
        let loc = Location.lift region in
        Match_nil (e_unit ~loc ())
      | ESeq
          { value =
              ( EAnnot
                  { value =
                      ( EArray
                          { value =
                              { inside =
                                  Some
                                    ( Expr_entry hd
                                    , [ (_, Rest_entry { value = { expr = tl; _ }; _ }) ]
                                    )
                              ; _
                              }
                          ; _
                          }
                      , _
                      , _ )
                  ; _
                  }
              , _ )
          ; _
          }
      | ESeq
          { value =
              ( EArray
                  { value =
                      { inside =
                          Some
                            ( Expr_entry hd
                            , [ (_, Rest_entry { value = { expr = tl; _ }; _ }) ] )
                      ; _
                      }
                  ; _
                  }
              , _ )
          ; _
          } ->
        let hd = compile_simple_pattern hd in
        let tl = compile_simple_pattern tl in
        Match_cons (hd, tl)
      | _ as e -> raise.error @@ not_a_valid_parameter e
    in
    let compile_case = function
      | CST.EFun { value = { parameters; body; _ }; region } ->
        let loc = Location.lift region in
        let args = compile_parameter parameters in
        let b = compile_function_body_to_expression ~loc ~raise body in
        args, b
      | _ as e -> raise.error @@ expected_a_function e
    in
    (match args with
    | [ CST.Expr_entry a; CST.Expr_entry b ]
    | [ CST.Expr_entry a; CST.Expr_entry b; CST.Rest_entry _ ] ->
      let params_a, body_a = compile_case a in
      let params_b, body_b = compile_case b in
      (match params_a, params_b, body_a, body_b with
      | Match_nil _match_nil, Match_cons (a, b), body_nil, body
      | Match_cons (a, b), Match_nil _match_nil, body, body_nil ->
        let matchee = self input in
        let loc = Location.lift region in
        let nil_case =
          (* TODO: improve locations here *)
          let pattern = Location.wrap ~loc @@ Pattern.P_list (List []) in
          ({ pattern; body = body_nil } : _ Match_expr.match_case)
        in
        let cons_case =
          (* TODO: improve locations here *)
          let a = Location.wrap ~loc @@ Pattern.P_var (Binder.make a None) in
          let b = Location.wrap ~loc @@ Pattern.P_var (Binder.make b None) in
          let pattern = Location.wrap ~loc @@ Pattern.P_list (Cons (a, b)) in
          ({ pattern; body } : _ Match_expr.match_case)
        in
        e_matching ~loc matchee [ nil_case; cons_case ]
      | _ -> raise.error @@ invalid_list_pattern_match args)
    | _ -> raise.error @@ invalid_list_pattern_match args)
  | ECall { value = EVar var, args; region } ->
    let loc = Location.lift region in
    let var, loc_var = r_split var in
    let func = e_variable_ez ~loc:loc_var var in
    let args = compile_arguments ~raise args in
    return @@ e_application ~loc func args
  | EConstr constr ->
    let (constr, args_o), loc = r_split constr in
    let args_o =
      Option.map ~f:(compile_tuple_expression ~loc ~raise <@ List.Ne.singleton) args_o
    in
    let args =
      Option.value ~default:(e_unit ~loc:(Location.lift constr.region) ()) args_o
    in
    return @@ e_constructor ~loc constr.value args
  | ECall call ->
    let (func, args), loc = r_split call in
    let func = self func in
    let args = compile_arguments ~raise args in
    return @@ e_application ~loc func args
  | EArray items ->
    let items, loc = r_split items in
    let items =
      match items.inside with
      | Some items -> npseq_to_list items
      | None -> []
    in
    let exprs = List.map ~f:(array_item_to_expression ~raise) items in
    let exprs' = List.map ~f:self exprs in
    return @@ e_tuple ~loc exprs'
  | EObject { value = { inside = Property_rest { value = { expr; _ }; _ }, rest; _ }; _ }
    ->
    let record = self expr in
    let aux up =
      let _, p = up in
      match p with
      | CST.Punned_property { value = EVar v as evar; region } ->
        let expr = self evar in
        [ Access_path.Access_record v.value ], expr, Location.lift region
      | Property { value = { name = EVar name; value; _ }; region } ->
        let expr = self value in
        [ Access_record name.value ], expr, Location.lift region
      | Property_rest _ -> raise.error @@ rest_not_supported_here p
      | _ -> raise.error @@ property_not_supported p
    in
    let updates = List.map ~f:aux rest in
    let aux e (path, update, loc) = e_update ~loc e path update in
    return @@ List.fold_left ~f:aux ~init:record updates
  | EObject obj' ->
    let obj, loc = r_split obj' in
    (match Discriminated_union.find_disc_obj obj' with
    | Some s ->
      let constructor = s.constructor in
      (* recreate the object without the constructor *)
      let filtered_object =
        Utils.nsepseq_foldl
          (fun a i ->
            match i with
            | CST.Property { value = { value = EString (String { value = s; _ }); _ }; _ }
              when String.equal s constructor -> a
            | _ -> (Token.wrap_comma Region.ghost, i) :: a)
          []
          obj.inside
      in
      let e =
        match filtered_object with
        | hd :: rest ->
          let new_inside = snd hd, rest in
          let obj = { obj with inside = new_inside } in
          self @@ EObject { value = obj; region = Region.ghost }
        | [] -> e_unit ~loc ()
      in
      (* turn the object into a constructor *)
      e_constructor ~loc constructor e
    | None ->
      let aux : CST.property -> string * expression =
       fun fa ->
        match fa with
        | Punned_property prop ->
          let prop, loc = r_split prop in
          let var = expression_to_variable ~raise prop in
          var.value, e_variable ~loc (compile_variable var)
        | Property prop2 ->
          let prop2, _ = r_split prop2 in
          let var = expression_to_variable ~raise prop2.name in
          let expr = self prop2.value in
          var.value, expr
        | Property_rest _ -> raise.error @@ rest_not_supported_here fa
      in
      let obj = List.map ~f:aux @@ npseq_to_list obj.inside in
      return @@ e_record_ez ~loc obj)
  | EProj proj ->
    let proj, loc = r_split proj in
    let var = self proj.expr in
    let sels, _ = compile_selection ~raise proj.selection in
    return @@ e_accessor ~loc var [ sels ]
  | EModA ma ->
    let ma, loc = r_split ma in
    let rec aux : Module_var.t list -> CST.expr -> AST.expression =
     fun acc exp ->
      match exp with
      | EVar v ->
        let accessed_el = compile_variable v in
        return @@ e_module_accessor ~loc acc accessed_el
      | EModA ma ->
        aux
          (acc @ [ Module_var.of_input_var ~loc ma.value.module_name.value ])
          ma.value.field
      | _ -> raise.error (expected_a_variable (CST.expr_to_region ma.field))
    in
    aux [ Module_var.of_input_var ~loc ma.module_name.value ] ma.field
  | EFun func ->
    let func, loc = r_split func in
    let ({ parameters; lhs_type; body; arrow = _ } : CST.fun_expr) = func in
    let lhs_type = Option.map ~f:(compile_type_expression ~raise <@ snd) lhs_type in
    let binder, exprs = compile_parameter ~const:true ~raise parameters in
    let body = compile_function_body_to_expression ~loc ~raise body in
    let expr = exprs body in
    return @@ e_lambda ~loc binder lhs_type expr
  | EAnnot { value = EArith (Int i), _, TVar { value = "nat"; _ }; region = _ } ->
    let (_, i), loc = r_split i in
    return @@ e_nat_z ~loc i
  | EAnnot { value = EArith (Int i), _, TVar { value = "tez"; _ }; region = _ } ->
    let (_, i), loc = r_split i in
    let mutez = Z.mul (Z.of_int 1_000_000) i in
    return @@ e_mutez_z ~loc mutez
  | EAnnot { value = EArith (Int i), _, TVar { value = "mutez"; _ }; region = _ } ->
    let (_, i), loc = r_split i in
    return @@ e_mutez_z ~loc i
  | EAnnot
      { value = ECodeInj { value = { language; code }; _ }, kwd_as, type_expr; region } ->
    let value : CST.code_inj =
      { language; code = EAnnot { value = code, kwd_as, type_expr; region } }
    in
    let e = CST.ECodeInj { value; region } in
    self e
  | EAnnot annot ->
    let annot, loc = r_split annot in
    let expr, _, ty = annot in
    let expr = self expr in
    let ty = compile_type_expression ~raise ty in
    return @@ e_annotation ~loc expr ty
  | ECodeInj ci ->
    let ci, loc = r_split ci in
    let language, _ = r_split ci.language in
    let code = self ci.code in
    return @@ e_raw_code ~loc language code
  | ESeq seq ->
    let seq, loc = r_split seq in
    let seq = List.map ~f:self @@ npseq_to_list seq in
    (match seq with
    | [] -> return @@ e_unit ~loc ()
    | hd :: tl ->
      let rec aux prev = function
        | [] -> return @@ prev
        | hd :: tl -> (return <@ e_sequence ~loc prev) @@ aux hd tl
      in
      aux hd @@ tl)
  | EAssign (e1, op, (EAssign ((EVar _ as ev), _, _) as e2)) ->
    let e2 = self e2 in
    let e1 = self (EAssign (e1, op, ev)) in
    let loc = Location.cover e1.location e2.location in
    e_sequence ~loc e2 e1
  | EAssign ((EVar { value; region } as e1), op, e2) ->
    let loc = Location.lift region in
    let outer_loc = Location.lift op.region in
    let e2x =
      match op.value with
      | Eq -> self e2
      | Assignment_operator ao ->
        let lexeme =
          match ao with
          | Times_eq -> "*="
          | Div_eq -> "/="
          | Plus_eq -> "+="
          | Min_eq -> "-="
          | Mod_eq -> "%="
        in
        let ao : Constant.constant' =
          match ao with
          | Times_eq -> C_MUL
          | Div_eq -> C_DIV
          | Plus_eq -> C_POLYMORPHIC_ADD
          | Min_eq -> C_POLYMORPHIC_SUB
          | Mod_eq -> C_MOD
        in
        compile_bin_op
          ~raise
          ao
          { value = { op = Token.wrap lexeme op.region; arg1 = e1; arg2 = e2 }
          ; region = op.region
          }
    in
    (match e2 with
    | EAssign (EProj { value = { expr; selection }; region = _ }, { value = Eq; _ }, _) ->
      let sels, _ = compile_selection ~raise selection in
      let expr = self expr in
      e_sequence
        ~loc
        e2x
        (e_assign
           ~loc:outer_loc
           (Binder.make (Value_var.of_input_var ~loc value) None)
           (e_accessor ~loc expr [ sels ]))
    | _ ->
      e_assign ~loc:outer_loc (Binder.make (Value_var.of_input_var ~loc value) None) e2x)
  | EAssign (e1, ({ value = Eq; _ } as op), (EAssign ((EProj _ as eproj), _, _) as e2)) ->
    let e2 = self e2 in
    let e1 = CST.EAssign (e1, op, eproj) in
    let e1 = self e1 in
    let loc = Location.cover e1.location e2.location in
    e_sequence ~loc e2 e1
  | EAssign
      ( EProj
          { value =
              { expr = EVar { value = evar_value; _ }
              ; selection =
                  Component { value = { inside = EArith (Int _); _ }; _ } as selection
              }
          ; region = _
          }
      , ({ value = Eq; _ } as op)
      , e2 ) ->
    let e2 = self e2 in
    let loc = Location.lift op.region in
    let sels, _ = compile_selection ~raise selection in
    e_assign_ez ~loc evar_value
    @@ e_update ~loc (e_variable_ez ~loc evar_value) [ sels ] e2
  | EAssign _ as e -> raise.error @@ not_supported_assignment e
  | ETernary e ->
    let ternary, loc = r_split e in
    let test = self ternary.condition in
    let truthy = self ternary.truthy in
    let falsy = self ternary.falsy in
    e_cond ~loc test truthy falsy


and compile_pattern ~raise : CST.pattern -> AST.ty_expr option Pattern.t =
 fun p ->
  match p with
  | PVar var ->
    let CST.{ variable; _ }, loc = r_split var in
    let var = compile_variable variable in
    Location.wrap ~loc (Pattern.P_var (Binder.make var None))
  | PArray tuple ->
    let tuple, loc = r_split tuple in
    let lst = npseq_to_ne_list tuple.inside in
    let patterns = List.Ne.to_list lst in
    let nested = List.map ~f:(compile_pattern ~raise) patterns in
    Location.wrap ~loc (Pattern.P_tuple nested)
  | PObject record ->
    let record, loc = r_split record in
    let lps =
      List.map ~f:(fun p ->
          let l =
            match p with
            | CST.PVar var -> Label.of_string var.value.variable.value
            | _ -> raise.error @@ unsupported_pattern_type p
          in
          l, compile_pattern ~raise p)
      @@ Utils.nsepseq_to_list record.inside
    in
    Location.wrap ~loc (Pattern.P_record lps)
  | PRest _ | PAssign _ | PConstr _ | PDestruct _ ->
    raise.error @@ unsupported_pattern_type p


and compile_parameter ~raise
    : const:bool -> CST.expr -> _ Param.t * (expression -> expression)
  =
 fun ~const expr ->
  let mut_flag : Param.mutable_flag = if const then Immutable else Mutable in
  let self = compile_parameter ~raise ~const in
  let return ?ascr fun_ var = Param.make ~mut_flag var ascr, fun_ in
  let return_1 ?ascr var = return ?ascr (fun e -> e) var in
  let matching ~loc binder_lst fun_ =
    match (binder_lst : _ Param.t list) with
    | [ binder ] -> binder, fun_
    | _ ->
      let var = Value_var.fresh ~loc () in
      let expr expr =
        e_param_matching_tuple ~loc (e_variable ~loc var) binder_lst @@ fun_ expr
      in
      let ascr = Option.all @@ List.map ~f:Param.get_ascr binder_lst in
      let ascr = Option.map ~f:(t_tuple ~loc) ascr in
      Param.make var ascr, expr
  in
  match expr with
  | EAnnot ea ->
    let ea, _loc = r_split ea in
    let (expr, _, type_expr) : CST.annot_expr = ea in
    let ascr = compile_type_expression ~raise type_expr in
    let binder, exprs = self expr in
    Param.map (Fn.const @@ Some ascr) binder, exprs
  | EArray array_items ->
    let arguments, loc = r_split array_items in
    let ({ inside = arguments; _ } : _ CST.brackets) = arguments in
    let array_item = function
      | CST.Expr_entry e -> compile_parameter ~raise ~const e
      | Rest_entry _ as r -> raise.error @@ array_rest_not_supported r
    in
    let arguments = Utils.sepseq_to_list arguments in
    let aux (binder, fun_') (binder_lst, fun_) = binder :: binder_lst, fun_' <@ fun_ in
    let binder_lst, fun_ =
      List.fold_right ~f:aux ~init:([], fun e -> e) @@ List.map ~f:array_item arguments
    in
    matching ~loc binder_lst fun_
  | EPar { value = { inside = ESeq { value = arguments; _ }; _ }; region } ->
    let loc = Location.lift region in
    let aux b (binder_lst, fun_) =
      let binder, fun_' = self b in
      binder :: binder_lst, fun_' <@ fun_
    in
    let binder_lst, fun_ =
      List.fold_right ~f:aux ~init:([], fun e -> e) @@ npseq_to_list arguments
    in
    let binder, expr =
      match (binder_lst : _ Param.t list) with
      | [ binder ] -> binder, fun_
      | _ -> matching ~loc binder_lst fun_
    in
    binder, expr
  | EVar var ->
    let var = compile_variable var in
    return_1 var
  | EUnit the_unit ->
    let loc = Location.lift the_unit.region in
    return_1 ~ascr:(t_unit ~loc ()) @@ Value_var.fresh ~loc ~name:"()" ()
  | EObject obj' ->
    let obj, loc = r_split obj' in
    let var = Value_var.fresh ~loc () in
    let aux (p : CST.property) (binder_lst, fun_') =
      match p with
      | Punned_property { value = EVar v as value; _ } ->
        let field_name = v.value in
        let binder, fun_ = compile_parameter ~raise ~const value in
        (field_name, binder) :: binder_lst, fun_ <@ fun_'
      | Property { value = { name = EVar v; value; _ }; _ } ->
        let field_name = v.value in
        let binder, fun_ = compile_parameter ~raise ~const value in
        (field_name, binder) :: binder_lst, fun_ <@ fun_'
      | _ -> raise.error @@ not_a_valid_parameter (CST.EObject obj')
    in
    let binder_lst, fun_ =
      List.fold_right ~f:aux ~init:([], fun e -> e) @@ npseq_to_list obj.inside
    in
    let expr expr =
      e_param_matching_record ~loc (e_variable ~loc var) binder_lst @@ fun_ expr
    in
    let ascr = Option.all @@ List.map ~f:(Fn.compose Param.get_ascr snd) binder_lst in
    let ascr = Option.map ~f:(t_tuple ~loc) ascr in
    return ?ascr expr var
  | _ -> raise.error @@ not_a_valid_parameter expr


and compile_function_body_to_expression ~loc ~raise : CST.body -> AST.expression =
 fun body ->
  match body with
  | FunctionBody statements ->
    compile_statements_to_expression ~loc ~raise statements.value.inside
  | ExpressionBody expr -> compile_expression ~raise expr


(*
  JsLIGO has statements. There are two cases when compiling a statement:
  - A `return` statement are easy: the resulting expression is just the
    content of the return
  - `let` and `const` are subtler. There are no expression corresponding to
    `const x = 42 ;`. The result of compiling this statement is actually the
    function that takes `body` as a parameter and returns `let x = 42 in body`
*)

and merge_statement_results ~raise
    : statement_result -> statement_result -> statement_result
  =
 fun f s ->
  match f, s with
  | Binding a, Binding b -> Binding (a <@ b)
  | Binding a, Expr b -> Expr (a b)
  | Binding a, Break b -> Break (a @@ e_unit ~loc:b.location ())
  | Binding a, Return b -> Return (a b)
  | Binding a, Binding_return b -> Binding_return (a <@ b)
  | Binding_return a, Binding b -> Binding (a <@ b)
  | Binding_return a, Expr b -> Expr (a b)
  | Binding_return a, Break b -> Break (a @@ e_unit ~loc:b.location ())
  | Binding_return a, Return b -> Return (a b)
  | Expr a, Binding b -> Binding (e_sequence ~loc:a.location a <@ b)
  | Expr a, Expr b -> Expr (e_sequence ~loc:b.location a b)
  | Expr a, Break _ -> Break a
  | Expr a, Return b -> Return (e_sequence ~loc:a.location a b)
  | Expr a, Binding_return b -> Binding_return (e_sequence ~loc:a.location a <@ b)
  (* In a block, any statement after a [break] or [return] is considered unreachable *)
  | Break a, Expr b | Break a, Break b | Break a, Return b ->
    raise.warning (`Jsligo_unreachable_code b.location);
    Break a
  | Break a, Binding_return b | Break a, Binding b ->
    let x = b @@ e_unit ~loc:a.location () in
    raise.warning (`Jsligo_unreachable_code x.location);
    Break a
  | Return a, Return x ->
    raise.warning (`Jsligo_unreachable_code x.location);
    Return a
  | Return a, Expr x ->
    raise.warning (`Jsligo_unreachable_code x.location);
    Return a
  | Return a, Binding_return b | Return a, Binding b ->
    let x = b @@ e_unit ~loc:a.location () in
    raise.warning (`Jsligo_unreachable_code x.location);
    Return a
  | Return a, Break b ->
    raise.warning (`Jsligo_unreachable_code b.location);
    Return a
  | Binding_return a, Binding_return b -> Binding_return (a <@ b)


and filter_private (attributes : CST.attributes) =
  List.filter ~f:(fun v -> not @@ String.equal v.value "private") attributes


(* can probably be cleaned up *)
and compile_val_binding ~raise
    :  CST.attributes -> CST.val_binding Region.reg -> Region.t
    -> [ `Fun of type_expression option Binder.t
       | `Val of type_expression option Pattern.t
       ]
       * string list
       * expression
  =
 fun attributes val_binding region ->
  let loc = Location.lift region in
  let CST.{ binders; type_params; lhs_type; expr = let_rhs; _ } = val_binding.value in
  let attr = compile_attributes attributes in
  let lhs_type = Option.map ~f:(compile_type_expression ~raise <@ snd) lhs_type in
  let let_rhs' = compile_expression ~raise let_rhs in
  match binders, let_rhs with
  | CST.PVar name, EFun _ ->
    (* function *)
    let fun_binder : Value_var.t = compile_variable name.value.variable in
    let expr =
      let lambda =
        trace_option ~raise (recursion_on_non_function let_rhs'.location)
        @@ get_e_lambda let_rhs'.expression_content
      in
      let lhs_type =
        match lhs_type with
        | Some lhs_type -> Some lhs_type
        | None ->
          Option.map ~f:(Utils.uncurry (t_arrow ~loc))
          @@ Option.bind_pair (Param.get_ascr lambda.binder, lambda.output_type)
      in
      if is_recursive_lambda fun_binder lambda
      then (
        let fun_type =
          trace_option ~raise (untyped_recursive_fun name.region) @@ lhs_type
        in
        let Lambda.{ binder; result; output_type = _ } = lambda in
        let Arrow.{ type1; type2 } = get_t_arrow_exn fun_type in
        let lambda =
          Lambda.
            { binder = Param.map (Fn.const type1) binder; result; output_type = type2 }
        in
        e_recursive ~loc:(Location.lift name.region) fun_binder fun_type lambda)
      else make_e ~loc:(Location.lift name.region) @@ E_lambda lambda
    in
    (* This handle polymorphic annotation *)
    let map_ascr ascr =
      Option.map ascr ~f:(fun rhs_type ->
          Option.value_map type_params ~default:rhs_type ~f:(fun tp ->
              let tp, loc = r_split tp in
              let type_vars =
                List.Ne.map compile_type_var @@ npseq_to_ne_list tp.inside
              in
              List.Ne.fold_right
                ~f:(fun tvar t -> t_for_all ~loc tvar Type t)
                ~init:rhs_type
                type_vars))
    in
    let expr =
      Option.value_map
        ~default:expr
        ~f:(fun tp ->
          let tp, loc = r_split tp in
          let type_vars = List.Ne.map compile_type_var @@ npseq_to_ne_list tp.inside in
          List.Ne.fold_right ~f:(fun t e -> e_type_abs ~loc t e) ~init:expr type_vars)
        type_params
    in
    let binder = Binder.make fun_binder lhs_type in
    let binder = Binder.map map_ascr binder in
    `Fun binder, attr, expr
  | p, _ ->
    let pattern = compile_pattern ~raise p in
    let expr =
      match let_rhs with
      | EAssign ((EVar _ as v), _, _) ->
        e_sequence ~loc let_rhs' (compile_expression ~raise v)
      | EAssign (EProj { value = { expr = proj_expr; selection }; _ }, _, _) ->
        let var = compile_expression ~raise proj_expr in
        let sels, _ = compile_selection ~raise selection in
        e_sequence ~loc let_rhs' (e_accessor ~loc var [ sels ])
      | _ -> let_rhs'
    in
    let expr =
      Option.value_map lhs_type ~default:expr ~f:(fun ty ->
          AST.e_ascription
            ~loc:expr.location
            { anno_expr = expr; type_annotation = ty }
            ())
    in
    `Val pattern, attr, expr


and compile_let_binding ~raise
    : CST.attributes -> CST.val_binding Region.reg -> Region.t -> AST.declaration
  =
 fun attributes val_binding region ->
  let lhs, attr, expr = compile_val_binding ~raise attributes val_binding region in
  match lhs with
  | `Fun binder -> Location.wrap ~loc:expr.location (AST.D_value { binder; attr; expr })
  | `Val pattern ->
    Location.wrap ~loc:expr.location (AST.D_irrefutable_match { pattern; attr; expr })


and compile_let_in_binding ~raise
    :  const:bool -> CST.attributes -> CST.val_binding Region.reg -> Region.t
    -> AST.expression -> AST.expression
  =
 fun ~const attributes val_binding region ->
  let loc = Location.r_extract val_binding in
  let lhs, attr, rhs = compile_val_binding ~raise attributes val_binding region in
  let binding rhs body =
    let pattern =
      match lhs with
      | `Fun binder -> Location.wrap ~loc:(Binder.get_loc binder) (Pattern.P_var binder)
      | `Val pattern -> pattern
    in
    if const
    then e_let_in ~loc pattern attr rhs body
    else e_let_mut_in ~loc pattern attr rhs body
  in
  fun body -> binding rhs body


and compile_statements ?(wrap = false) ~raise : CST.statements -> statement_result =
 fun statements ->
  let aux result = function
    | (_, hd) :: tl ->
      let wrapper =
        CST.SBlock
          { value =
              { inside = hd, tl
              ; lbrace = Token.ghost_lbrace
              ; rbrace = Token.ghost_rbrace
              }
          ; region = Region.ghost
          }
      in
      let block = compile_statement ~wrap:false ~raise wrapper in
      merge_statement_results ~raise result block
    | [] -> result
  in
  let hd = fst statements in
  let snd_ = snd statements in
  let init = compile_statement ~wrap ~raise hd in
  aux init snd_


and compile_statement ?(wrap = false) ~raise : CST.statement -> statement_result =
 fun statement ->
  let self ?(wrap = false) = compile_statement ~wrap ~raise in
  let self_expr = compile_expression ~raise in
  let self_statements ?(wrap = false) = compile_statements ~wrap ~raise in
  let binding e = Binding (fun f -> e f) in
  let expr e = Expr e in
  let return r : statement_result = Return r in
  match statement with
  | SExpr e ->
    let e = self_expr e in
    expr e
  | SBlock { value = { inside; _ }; region = _ } when not wrap ->
    let statements = self_statements ~wrap:true inside in
    statements
  | SBlock { value = { inside; _ }; region } ->
    let loc = Location.lift region in
    let block_scope_var = Value_var.fresh ~loc () in
    let block_binder = Pattern.var ~loc (Binder.make block_scope_var None) in
    let statements = self_statements ~wrap:true inside in
    let statements_e = statement_result_to_expression ~loc statements in
    let let_in = e_let_in ~loc block_binder [] statements_e in
    let var = e_variable ~loc block_scope_var in
    (match statements with
    | Return _ -> return @@ let_in var
    | Expr _ -> expr @@ let_in var
    | Break _ -> Break (let_in var)
    (* | Binding b -> Binding (fun f -> e_let_in ~loc block_binder [] (b f) var)) *)
    | Binding_return b -> Binding (fun f -> e_let_in ~loc block_binder [] (b f) var)
    | Binding _ -> Binding let_in)
  | SCond cond ->
    let cond, loc = r_split cond in
    let test = self_expr cond.test.inside in
    let then_clause = self ~wrap:false cond.ifso in
    let else_clause = Option.map ~f:(fun (_, s) -> self ~wrap:false s) cond.ifnot in
    let compile_clause = function
      | Binding e | Binding_return e -> expr, e @@ e_unit ~loc ()
      | Expr e -> expr, e_sequence ~loc e (e_unit ~loc ())
      | Break b -> return, e_sequence ~loc b (e_unit ~loc ())
      | Return r -> return, r
    in
    let then_clause_orig = then_clause in
    let _, then_clause = compile_clause then_clause in
    (match else_clause with
    | Some s ->
      let n, else_clause = compile_clause s in
      (match then_clause_orig, s with
      | Binding a, Binding b -> Binding (fun x -> e_cond ~loc test (a x) (b x))
      | Binding a, _ -> Binding (fun x -> e_cond ~loc test (a x) else_clause)
      | _, Binding b -> Binding (fun x -> e_cond ~loc test then_clause (b x))
      | _ -> n (e_cond ~loc test then_clause else_clause))
    | None ->
      (match then_clause_orig with
      | Return _ ->
        Binding_return (fun else_clause -> e_cond ~loc test then_clause else_clause)
      | _ -> Expr (e_cond ~loc test then_clause (e_unit ~loc ()))))
  | SReturn { value = { expr; _ }; region } ->
    (match expr with
    | Some v ->
      let expr = self_expr v in
      return expr
    | None -> return (e_unit ~loc:(Location.lift region) ()))
  | SLet li' ->
    (* TODO: ensure assignment can only happen to let values, not const values. *)
    let li, _loc = r_split li' in
    let ({ bindings; attributes; _ } : CST.let_decl) = li in
    let lst = npseq_to_list bindings in
    let x =
      List.fold
        lst
        ~f:(fun acc x ->
          acc <@ compile_let_in_binding ~const:false ~raise attributes x li'.region)
        ~init:Fun.id
    in
    binding x
  | SConst li' ->
    let li, _loc = r_split li' in
    let ({ bindings; attributes; _ } : CST.const_decl) = li in
    let lst = npseq_to_list bindings in
    let x =
      List.fold
        lst
        ~f:(fun acc x ->
          acc <@ compile_let_in_binding ~const:true ~raise attributes x li'.region)
        ~init:Fun.id
    in
    binding x
  | SSwitch s' ->
    let s, loc = r_split s' in
    (match Discriminated_union.is_discriminated_union s' with
    | Some data ->
      (* here we turn the switch statement into a simple form of pattern matching *)
      let matchee, payload =
        match s'.value.expr with
        | EProj { value = { expr = EVar ev as v; _ }; _ } ->
          compile_expression ~raise v, ev
        | _ -> raise.error @@ wrong_matchee_disc s'.region
      in
      let rec check_return statements =
        match Utils.nsepseq_rev statements with
        | CST.SBreak _, _ -> ()
        | SReturn _, _ -> ()
        | SBlock { value = { inside; _ }; _ }, _ -> check_return inside
        | SCond { value = { ifso; ifnot; _ }; _ }, _ ->
          check_return (ifso, []);
          (match ifnot with
          | Some (_, ifnot) -> check_return (ifnot, [])
          | None -> ())
        | (_ as s), _ -> raise.error @@ case_break_disc (CST.statement_to_region s)
      in
      let st, cases =
        List.fold_left
          ~f:(fun (r, all) f ->
            match f with
            | Switch_case { expr = EString (String v); statements = Some statements; _ }
              ->
              check_return statements;
              let a =
                List.find_exn
                  ~f:(fun { constructor; _ } -> Poly.(constructor = v.value))
                  data
              in
              let ty =
                if a.has_payload
                then (
                  let b = Binder.make (compile_variable payload) None in
                  let arg = Pattern.P_var b in
                  Location.wrap ~loc arg)
                else Location.wrap ~loc Pattern.P_unit
              in
              let pattern = Location.wrap ~loc (Pattern.P_variant (Label v.value, ty)) in
              let statement_result = compile_statements ~raise statements in
              let r =
                match r with
                | Some r -> Some r
                | None -> Some statement_result
              in
              ( r
              , Match_expr.
                  { pattern; body = statement_result_to_expression ~loc statement_result }
                :: all )
            | Switch_case { expr = EString (String _); statements = None; _ } ->
              raise.error @@ case_break_disc s'.region
            | _ -> raise.error unexpected)
          ~init:(None, [])
          (nseq_to_list s.cases)
      in
      (match st with
      | Some (Return _) -> return (e_matching ~loc matchee cases)
      | _ -> expr (e_matching ~loc matchee cases))
    | None ->
      (* a switch statement based on if-else statements *)
      let switch_expr = self_expr s.expr in
      let fallthrough = Value_var.fresh ~loc ~name:"fallthrough" () in
      let found_case = Value_var.fresh ~loc ~name:"found_case" () in
      let binder var = Binder.make var None in
      let fallthrough_binder = binder fallthrough in
      let found_case_binder = binder found_case in
      let dummy_binder = binder (Value_var.fresh ~loc ()) in
      let initial =
        Binding
          (fun x ->
            e_let_mut_in
              ~loc
              (Pattern.var ~loc dummy_binder)
              []
              switch_expr
              (* this is done so that in case of only default we don't the un-used variable warning *)
              (e_let_mut_in
                 ~loc
                 (Pattern.var ~loc fallthrough_binder)
                 []
                 (e_false ~loc ())
                 (e_let_mut_in
                    ~loc
                    (Pattern.var ~loc found_case_binder)
                    []
                    (e_false ~loc ())
                    x)))
      in
      let cases = Utils.nseq_to_list s.cases in
      let fallthrough_assign_false = e_assign ~loc fallthrough_binder (e_false ~loc ()) in
      let fallthrough_assign_true = e_assign ~loc fallthrough_binder (e_true ~loc ()) in
      let found_case_assign_true = e_assign ~loc found_case_binder (e_true ~loc ()) in
      let not_expr e = e_constant ~loc (Const C_NOT) [ e ] in
      let and_expr a b = e_constant ~loc (Const C_AND) [ a; b ] in
      let or_expr a b = e_constant ~loc (Const C_OR) [ a; b ] in
      let eq_expr ~loc a b = e_constant ~loc (Const C_EQ) [ a; b ] in
      let found_case_eq_true =
        eq_expr ~loc (e_variable ~loc found_case) (e_true ~loc ())
      in
      let fallthrough_eq_true =
        eq_expr ~loc (e_variable ~loc fallthrough) (e_true ~loc ())
      in
      let prefix_case_cond case_expr = eq_expr ~loc switch_expr case_expr in
      let case_cond case_expr =
        or_expr
          fallthrough_eq_true
          (and_expr (not_expr found_case_eq_true) (prefix_case_cond case_expr))
      in
      (* __fallthrough || (! __found_case && <cond>) *)
      let process_case case =
        match case with
        | CST.Switch_case { kwd_case; expr; statements = None; colon = _ } ->
          let loc = Location.lift kwd_case#region in
          let case_expr = self_expr expr in
          let test = case_cond case_expr in
          let update_vars =
            e_sequence ~loc fallthrough_assign_true found_case_assign_true
          in
          Binding
            (fun x -> e_sequence ~loc (e_cond ~loc test update_vars (e_unit ~loc ())) x)
        | Switch_case { kwd_case; expr; statements = Some statements; colon = _ } ->
          let loc = Location.lift kwd_case#region in
          let case_expr = self_expr expr in
          let test = case_cond case_expr in
          let update_vars_fallthrough =
            e_sequence ~loc fallthrough_assign_true found_case_assign_true
          in
          let update_vars_break =
            e_sequence ~loc fallthrough_assign_false found_case_assign_true
          in
          let statements = self_statements statements in
          let statements =
            match statements with
            | Binding_return s | Binding s ->
              Binding
                (fun x ->
                  let e = e_sequence ~loc found_case_assign_true (s (e_unit ~loc ())) in
                  let e = e_cond ~loc test e (e_unit ~loc ()) in
                  e_sequence ~loc e x)
            | Expr e ->
              let e = e_sequence ~loc e update_vars_fallthrough in
              Binding (fun x -> e_sequence ~loc (e_cond ~loc test e (e_unit ~loc ())) x)
            | Break e ->
              let e = e_sequence ~loc e update_vars_break in
              Binding (fun x -> e_sequence ~loc (e_cond ~loc test e (e_unit ~loc ())) x)
            | Return e ->
              (* Return e *)
              Binding
                (fun x ->
                  match x with
                  | { expression_content = E_literal Literal_unit; _ } -> e
                  | _ as x -> e_cond ~loc test e x)
          in
          statements
        | Switch_default_case { statements = None; kwd_default = _; colon = _ } ->
          Binding (fun x -> e_sequence ~loc (e_unit ~loc ()) x)
        | Switch_default_case { kwd_default; statements = Some statements; colon = _ } ->
          let loc = Location.lift kwd_default#region in
          let default_cond = or_expr fallthrough_eq_true (not_expr found_case_eq_true) in
          (* __fallthrough || ! __found_case *)
          let statements = self_statements statements in
          let statements =
            match statements with
            | Binding s | Binding_return s ->
              Binding
                (fun x ->
                  let e = e_sequence ~loc found_case_assign_true (s (e_unit ~loc ())) in
                  let e = e_cond ~loc default_cond e (e_unit ~loc ()) in
                  e_sequence ~loc e x)
            | Expr e | Break e ->
              Binding
                (fun x -> e_sequence ~loc (e_cond ~loc default_cond e (e_unit ~loc ())) x)
            | Return e ->
              (* Return e *)
              Binding
                (fun x ->
                  match x with
                  | { expression_content = E_literal Literal_unit; _ } -> e
                  | _ as x -> e_cond ~loc default_cond e x)
          in
          statements
      in
      (* let o = *)
      List.fold_left cases ~init:initial ~f:(fun acc case ->
          merge_statement_results ~raise acc (process_case case))
      (* in
      match o with 
        Binding e -> Binding e
      | _ as e -> e     *))
  | SBreak b -> Break (e_unit ~loc:(Location.lift b#region) ())
  | SType ti ->
    let ti, loc = r_split ti in
    let ({ name; type_expr; _ } : CST.type_decl) = ti in
    let type_binder = compile_type_var name in
    let rhs = compile_type_expression ~raise type_expr in
    binding (e_type_in ~loc type_binder rhs)
  | SNamespace n ->
    let (_, name, rhs, attributes), loc = r_split n in
    ignore attributes;
    let module_binder = compile_mod_var name in
    let rhs =
      let decls = compile_namespace ~raise rhs.value.inside in
      m_struct ~loc decls
    in
    binding (e_mod_in ~loc module_binder rhs)
  | SExport e ->
    let (_, statement), _ = r_split e in
    self statement
  | SImport i' ->
    let i, loc = r_split i' in
    (match i with
    | Import_rename { alias; module_path; _ } ->
      let alias = compile_mod_var alias in
      let module_ =
        let path = List.Ne.map compile_mod_var @@ npseq_to_ne_list module_path in
        m_path ~loc path
      in
      binding (e_mod_in ~loc alias module_)
    | Import_all_as _ -> raise.error @@ not_implemented i'.region
    | Import_selected _ -> raise.error @@ not_implemented i'.region)
  | SForOf s ->
    let forOf, loc = r_split s in
    let binder = compile_variable forOf.index, None in
    let collection = self_expr forOf.expr in
    let sr = self forOf.statement in
    let body = statement_result_to_expression ~loc sr in
    binding @@ e_sequence ~loc (e_for_each ~loc binder collection Any body)
  | SWhile e ->
    let w, loc = r_split e in
    let cond = self_expr w.expr in
    let statement_result = self w.statement in
    let body = statement_result_to_expression ~loc statement_result in
    binding @@ e_sequence ~loc (e_while ~loc cond body)


and statement_result_to_expression ~loc : statement_result -> AST.expression =
 fun statement_result ->
  match statement_result with
  | Binding b | Binding_return b -> b (e_unit ~loc ())
  | Expr e ->
    let pattern =
      Location.wrap ~loc (Pattern.P_var (Binder.make (Value_var.fresh ~loc ()) None))
    in
    e_let_in ~loc pattern [] e (e_unit ~loc ())
  | Break r | Return r -> r


and compile_statements_to_expression ~loc ~raise : CST.statements -> AST.expression =
 fun statements ->
  let statement_result = compile_statements ~raise statements in
  statement_result_to_expression ~loc statement_result


and compile_statement_to_declaration ~raise ~export
    : CST.statement -> AST.declaration list
  =
 fun statement ->
  match statement with
  | SType { value; region } ->
    let name = value.name in
    let attributes =
      if export then filter_private value.attributes else value.attributes
    in
    let attributes = compile_attributes attributes in
    let type_expr =
      let rhs = compile_type_expression ~raise value.type_expr in
      match value.params with
      | None -> rhs
      | Some x ->
        let lst = Utils.nsepseq_to_list x.value.inside in
        let aux : CST.type_var -> AST.type_expression -> AST.type_expression =
         fun param type_ ->
          let ty_binder = compile_type_var param in
          t_abstraction ~loc:(Location.lift region) ty_binder Type type_
        in
        List.fold_right ~f:aux ~init:rhs lst
    in
    let d =
      D_type { type_binder = compile_type_var name; type_expr; type_attr = attributes }
    in
    [ Location.wrap ~loc:(Location.lift region) d ]
  | SLet { value = { bindings; attributes; _ }; region } ->
    (* Add deprecation warning (for implicit conversion to lets) *)
    raise.warning (`Jsligo_deprecated_toplevel_let (Location.lift region));
    let attributes = if export then filter_private attributes else attributes in
    let bindings = npseq_to_list bindings in
    let aux acc binding =
      acc @ [ compile_let_binding ~raise attributes binding region ]
    in
    List.fold ~f:aux ~init:[] bindings
  | SConst { value = { bindings; attributes; _ }; region } ->
    let attributes = if export then filter_private attributes else attributes in
    let bindings = npseq_to_list bindings in
    let aux acc binding =
      acc @ [ compile_let_binding ~raise attributes binding region ]
    in
    List.fold ~f:aux ~init:[] bindings
  | SNamespace
      { value =
          _, ident, { value = { inside = statements; _ }; region = region_in }, attributes
      ; region
      } ->
    let attributes = if export then filter_private attributes else attributes in
    let loc = Location.lift region in
    let module_binder = compile_mod_var ident in
    let attributes = compile_attributes attributes in
    let module_ =
      let _loc = Location.lift region_in in
      Location.wrap ~loc @@ Module_expr.M_struct (compile_namespace ~raise statements)
    in
    let d = D_module { module_binder; module_; module_attr = attributes } in
    [ Location.wrap ~loc d ]
  | SImport { value; region } ->
    let loc = Location.lift region in
    (match value with
    | Import_rename { alias; module_path; _ } ->
      let module_binder = compile_mod_var alias in
      let module_ =
        let path = List.Ne.map compile_mod_var @@ npseq_to_ne_list module_path in
        m_path ~loc path
      in
      let d = D_module { module_binder; module_; module_attr = [] } in
      [ Location.wrap ~loc d ]
    | Import_all_as _ -> raise.error @@ not_implemented region
    | Import_selected _ -> raise.error @@ not_implemented region)
  | SExport { value = _, s; _ } -> compile_statement_to_declaration ~raise ~export:true s
  | _ -> raise.error @@ statement_not_supported_at_toplevel statement


and compile_statements_to_program ~raise : CST.ast -> AST.program =
 fun ast ->
  let aux : CST.toplevel_statement -> declaration list =
   fun statement ->
    match statement with
    | TopLevel (statement, _) ->
      compile_statement_to_declaration ~raise ~export:false statement
    | Directive _ -> []
  in
  let statements = nseq_to_list ast.statements in
  let declarations = List.map ~f:aux statements in
  let lst = List.concat declarations in
  lst


and compile_namespace ~raise : CST.statements -> AST.module_ =
 fun statements ->
  let statements = Utils.nsepseq_to_list statements in
  let declarations =
    List.map ~f:(compile_statement_to_declaration ~raise ~export:false) statements
  in
  let lst = List.concat declarations in
  lst


let compile_module ~raise : CST.ast -> AST.declaration list =
 fun t -> compile_statements_to_program ~raise t


let compile_program ~raise : CST.ast -> AST.program =
 fun ast ->
  Discriminated_union.reset ();
  nseq_to_list ast.statements
  |> List.map ~f:(fun a ~raise ->
         match a with
         | CST.TopLevel (statement, _) ->
           compile_statement_to_declaration ~raise ~export:false statement
         | Directive _ -> [])
  |> Simple_utils.Trace.collect ~raise
  |> List.concat
