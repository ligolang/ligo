(* Decompiler to the CST of PascaLIGO *)

module Utils = Simple_utils.Utils
module AST = Ast_imperative
module CST = Cst.Pascaligo
module Wrap = Lexing_shared.Wrap
module Token = Lexing_pascaligo.Token
module Predefined = Predefined.Tree_abstraction
module Shared_helpers = Tree_abstraction_shared.Helpers
open Simple_utils.Function
module Region = Simple_utils.Region
module List = Simple_utils.List
module Location = Simple_utils.Location
module Pair = Simple_utils.Pair
open Ligo_prim

(* Utils *)

(* TODO: The colon is not a separator of key/value! *)

let list_to_sepseq ~sep lst =
  match lst with
  | [] -> None
  | hd :: tl ->
    let aux e = sep, e in
    Some (hd, List.map ~f:aux tl)


let list_to_nsepseq ~sep lst =
  match list_to_sepseq ~sep lst with
  | Some s -> s
  | None -> failwith "List is not a non_empty list" (* TODO: NO failwith! *)


let nelist_to_npseq ~sep (hd, tl) = hd, List.map ~f:(fun e -> sep, e) tl
let par a = CST.{ lpar = Token.ghost_lpar; inside = a; rpar = Token.ghost_rpar }

let type_vars_of_list : CST.variable list -> CST.variable CST.tuple =
 fun lst ->
  let lst = list_to_nsepseq ~sep:Token.ghost_comma lst in
  Region.wrap_ghost (par lst)


let brackets a =
  CST.{ lbracket = Token.ghost_lbracket; inside = a; rbracket = Token.ghost_rbracket }


let prefix_colon a = Wrap.ghost "", a
let terminator = Some Token.ghost_semi
let lead_vbar = terminator
let enclosing_brackets = Token.ghost_lbracket, Token.ghost_rbracket
let block_enclosing = CST.Braces (None, Token.ghost_lbrace, Token.ghost_rbrace)
let module_enclosing = CST.Braces (None, Token.ghost_lbrace, Token.ghost_rbrace)

let inject : string Wrap.wrap -> ('a, CST.semi) Utils.sepseq -> 'a CST.compound =
 fun kind elements ->
  let opening, closing = enclosing_brackets in
  CST.{ kind; opening; elements; terminator; closing }


let to_block a = CST.{ enclosing = block_enclosing; statements = a; terminator }

let empty_block =
  to_block
    (list_to_nsepseq ~sep:Token.ghost_semi [ CST.S_Instr (I_Skip Token.ghost_skip) ])


let rec t_attr attributes ty =
  match attributes with
  | [] -> ty
  | hd :: tl -> CST.T_Attr (hd, t_attr tl ty)


let fun_decl ~is_rec fun_name parameters ret_type return terminator : CST.fun_decl =
  let kwd_recursive = if is_rec then Some Token.ghost_recursive else None in
  (* functions are all specialized and removed at the moment so we'll never get any type parameter when decompiling*)
  let type_params = None in
  CST.
    { kwd_recursive
    ; kwd_function = Token.ghost_function
    ; fun_name
    ; type_params
    ; parameters
    ; ret_type
    ; kwd_is = Token.ghost_is
    ; return
    ; terminator
    }


(* Decompiler *)

module type X_var = sig
  type t

  val pp : Format.formatter -> t -> unit
end

let decompile_variable_abs (type a) (module X : X_var with type t = a) : a -> CST.variable
  =
 fun var ->
  let var = Format.asprintf "%a" X.pp var in
  if String.contains var '#'
  then (
    let var = String.split ~on:'#' var in
    Wrap.ghost @@ "gen__" ^ String.concat var)
  else if String.length var > 4 && (String.equal "gen__" @@ String.sub var ~pos:0 ~len:5)
  then Wrap.ghost @@ "user__" ^ var
  else Wrap.ghost var


let decompile_variable = decompile_variable_abs (module Value_var)
let decompile_type_var = decompile_variable_abs (module Type_var)
let decompile_mod_var = decompile_variable_abs (module Module_var)

let rec decompile_type_expr : AST.type_expression -> CST.type_expr =
 fun te ->
  let return te = te in
  match te.type_content with
  | T_sum { attributes; fields } ->
    let attributes = Shared_helpers.decompile_attributes attributes in
    let aux
        ( Label.Label c
        , ({ associated_type; row_elem_attributes = row_attr; _ } : _ AST.row_element) )
      =
      let ctor = Wrap.ghost c in
      let arg = decompile_type_expr associated_type in
      let ctor_args = Some (Token.ghost_of, arg) in
      let attributes : CST.attribute list =
        Shared_helpers.decompile_attributes row_attr
      in
      let variant : CST.variant = { ctor; ctor_args; attributes } in
      Region.wrap_ghost variant
    in
    let variants = List.map ~f:aux fields in
    let variants = list_to_nsepseq ~sep:Token.ghost_vbar variants in
    let lead_vbar = Some Token.ghost_vbar in
    let sum : CST.sum_type = { lead_vbar; variants } in
    return @@ t_attr attributes (CST.T_Sum (Region.wrap_ghost sum))
  | T_record { fields; attributes } ->
    let aux
        ( Label.Label c
        , ({ associated_type; row_elem_attributes = field_attr; _ } : _ AST.row_element)
        )
      =
      let field_name = Wrap.ghost c in
      let field_type = decompile_type_expr associated_type in
      let field_attr = Shared_helpers.decompile_attributes field_attr in
      let field : CST.field_decl =
        { field_name
        ; field_type = Some (Token.ghost_colon, field_type)
        ; attributes = field_attr
        }
      in
      Region.wrap_ghost field
    in
    let struct_ = List.map ~f:aux fields in
    let elements = list_to_sepseq ~sep:Token.ghost_semi struct_ in
    let attributes = Shared_helpers.decompile_attributes attributes in
    let compound : CST.field_decl CST.reg CST.compound =
      inject Token.ghost_record elements
    in
    return @@ t_attr attributes (CST.T_Record (Region.wrap_ghost compound))
  | T_tuple tuple ->
    let lst = List.map ~f:decompile_type_expr tuple in
    let lhs, tail = List.Ne.of_list lst in
    let tuple : CST.cartesian =
      Region.wrap_ghost
        (lhs, Token.ghost_times, list_to_nsepseq ~sep:Token.ghost_times tail)
    in
    return @@ CST.T_Cart tuple
  | T_arrow { type1; type2 } ->
    let type1 = decompile_type_expr type1 in
    let type2 = decompile_type_expr type2 in
    let arrow = type1, Wrap.ghost "", type2 in
    return @@ CST.T_Fun (Region.wrap_ghost arrow)
  | T_variable variable ->
    let v = decompile_type_var variable in
    return @@ CST.T_Var v
  | T_app { type_operator; arguments } ->
    let v = CST.T_Var (decompile_type_var (Module_access.get_el @@ type_operator)) in
    let lst = List.map ~f:decompile_type_expr arguments in
    let lst = list_to_nsepseq ~sep:Token.ghost_comma lst in
    let lst : _ CST.par = { lpar = Wrap.ghost ""; inside = lst; rpar = Wrap.ghost "" } in
    return @@ CST.T_App (Region.wrap_ghost (v, Region.wrap_ghost lst))
  | T_annoted _annot -> failwith "TODO: decompile T_annoted"
  | T_module_accessor { module_path; element } ->
    let lst = List.map ~f:decompile_mod_var module_path in
    let module_path = list_to_nsepseq ~sep:Token.ghost_dot lst in
    let field = CST.T_Var (decompile_type_var element) in
    return
    @@ CST.T_ModPath
         (Region.wrap_ghost CST.{ module_path; selector = Token.ghost_dot; field })
  | T_singleton x ->
    (match x with
    | Literal_int i ->
      let z : CST.type_expr = CST.T_Int (Wrap.ghost ("", i)) in
      return z
    | Literal_string s ->
      let z : CST.type_expr =
        CST.T_String (Wrap.ghost (Simple_utils.Ligo_string.extract s))
      in
      return z
    | _ -> failwith "unsupported singleton")
  | T_abstraction x -> decompile_type_expr x.type_
  | T_for_all x -> decompile_type_expr x.type_


let get_e_variable : AST.expression -> _ =
 fun expr ->
  match expr.expression_content with
  | E_variable var -> var
  | _ ->
    failwith
    @@ Format.asprintf "%a should be a variable expression" AST.PP.expression expr


let rec get_e_accessor : AST.expression -> _ =
 fun expr ->
  match expr.expression_content with
  | E_variable var -> var, []
  | E_accessor { struct_; path } ->
    let var, lst = get_e_accessor struct_ in
    var, lst @ path
  | _ ->
    failwith
    @@ Format.asprintf "%a should be a variable expression" AST.PP.expression expr


let get_e_tuple : AST.expression -> AST.expression list =
 fun expr ->
  match expr.expression_content with
  | E_tuple tuple -> tuple
  | E_variable _
  | E_literal _
  | E_constant _
  | E_module_accessor _
  | E_application _
  | E_lambda _ -> [ expr ]
  | _ ->
    failwith @@ Format.asprintf "%a should be a tuple expression" AST.PP.expression expr


type eos =
  | Expression
  | Statements

let rec decompile_expression : AST.expression -> CST.expr =
 fun e ->
  let block, expr = decompile_to_block e in
  match expr with
  | Some expr ->
    (match block with
    | Some block ->
      let block = Region.wrap_ghost block in
      CST.E_Block (Region.wrap_ghost @@ CST.{ block; kwd_with = Wrap.ghost ""; expr })
    | None -> expr)
  | None ->
    failwith
    @@ Format.asprintf
         "An expression was expected, but this was decompiled to statements.@.Expr : %a@ \
          @,\
          Loc : %a"
         AST.PP.expression
         e
         Location.pp
         e.location


and decompile_statements : AST.expression -> _ =
 fun expr ->
  let stat, _ = decompile_eos Statements expr in
  match stat with
  | Some stat -> stat
  | None ->
    failwith
    @@ Format.asprintf
         "Statements was expected, but this was decompiled to expression.@.Expr : %a@ @,\
          Loc : %a"
         AST.PP.expression
         expr
         Location.pp
         expr.location


and decompile_pattern : AST.type_expression option AST.Pattern.t -> CST.pattern =
 fun pattern ->
  let is_unit_pattern (p : AST.type_expression option AST.Pattern.t) =
    match p.wrap_content with
    | P_unit -> true
    | _ -> false
  in
  match pattern.wrap_content with
  | P_unit -> CST.P_Ctor (Wrap.ghost "Unit")
  | P_var v ->
    let name = decompile_variable @@ Binder.get_var v in
    CST.P_Var name
  | P_list pl ->
    (match pl with
    | Cons (pa, pb) ->
      let pa = decompile_pattern pa in
      let pb = decompile_pattern pb in
      let cons = Region.wrap_ghost (pa, Token.ghost_sharp, pb) in
      P_Cons cons
    | List [] -> P_Nil Token.ghost_nil
    | List plst ->
      let plst = List.map ~f:decompile_pattern plst in
      let elements = list_to_sepseq ~sep:Token.ghost_semi plst in
      P_List (Region.wrap_ghost (inject Token.ghost_list elements)))
  | P_variant (constructor, p) ->
    (match constructor with
    | Label constructor ->
      let p =
        if is_unit_pattern p
        then None
        else (
          let p = decompile_pattern p in
          let p = list_to_nsepseq ~sep:Token.ghost_comma [ p ] in
          let p = Region.wrap_ghost (par p) in
          Some p)
      in
      let constr = CST.P_Ctor (Wrap.ghost constructor) in
      CST.P_App (Region.wrap_ghost (constr, p)))
  | P_tuple lst ->
    let pl = List.map ~f:decompile_pattern lst in
    let pl = list_to_nsepseq ~sep:Token.ghost_comma pl in
    CST.P_Tuple (Region.wrap_ghost (par pl))
  | P_record lps ->
    let aux acc (Label.Label label, pattern) =
      let field_rhs = decompile_pattern pattern in
      let full_field =
        CST.Complete
          { field_lhs = CST.P_Var (Wrap.ghost label)
          ; field_lens = Lens_Id Token.ghost_eq
          ; field_rhs
          ; attributes = []
          }
      in
      Region.wrap_ghost full_field :: acc
    in
    let field_patterns = List.fold ~f:aux lps ~init:[] in
    let inj =
      inject Token.ghost_record (list_to_sepseq ~sep:Token.ghost_semi field_patterns)
    in
    CST.P_Record (Region.wrap_ghost inj)


and decompile_to_block : AST.expression -> CST.block option * CST.expr option =
 fun expr ->
  let stats, next = decompile_eos Expression expr in
  let block = Option.map ~f:(to_block <@ nelist_to_npseq ~sep:Token.ghost_semi) stats in
  block, next


and decompile_to_tuple_expr : AST.expression list -> CST.expr CST.tuple =
 fun expr ->
  let tuple_expr = List.map ~f:decompile_expression expr in
  let tuple_expr = list_to_nsepseq ~sep:Token.ghost_comma tuple_expr in
  Region.wrap_ghost @@ par @@ tuple_expr


and decompile_arguments : AST.expression list -> CST.call_args =
 fun e_list ->
  let list = List.map ~f:decompile_expression e_list in
  let sepseq = list_to_sepseq ~sep:Token.ghost_comma list in
  Region.wrap_ghost @@ par @@ sepseq


and decompile_eos
    : eos -> AST.expression -> CST.statement List.Ne.t option * CST.expr option
  =
 fun output expr ->
  let return (a, b) = a, b in
  let return_expr expr = return @@ (None, Some expr) in
  let return_expr_with_par expr =
    return_expr @@ CST.E_Par (Region.wrap_ghost @@ par @@ expr)
  in
  let return_typed expr ty =
    return_expr @@ CST.E_Typed (Region.wrap_ghost @@ par (expr, (Token.ghost_colon, ty)))
  in
  let return_stat stat = return @@ (Some stat, None) in
  let return_stat_ez stat = return_stat @@ (stat, []) in
  let return_inst inst = return_stat_ez @@ CST.S_Instr inst in
  let loc = expr.location in
  match expr.expression_content with
  | E_variable name ->
    let var = decompile_variable name in
    return_expr @@ CST.E_Var var
  | E_constant { cons_name; arguments } ->
    let expr = CST.E_Var (Wrap.ghost (Predefined.constant_to_string cons_name)) in
    (match arguments with
    | [] -> return_expr @@ expr
    | _ ->
      let arguments = decompile_arguments arguments in
      let const : CST.call = Region.wrap_ghost (expr, arguments) in
      (match output with
      | Expression -> return_expr (CST.E_App const)
      | Statements -> return_inst (CST.I_Call const)))
  | E_literal literal ->
    (* TODO: Check these new cases coming from dev *)
    (match literal with
    | Literal_unit ->
      let call_args = Region.wrap_ghost (par None)
      and ctor = CST.E_Ctor (Wrap.ghost "Unit") in
      return_expr @@ CST.E_App (Region.wrap_ghost (ctor, call_args))
    | Literal_int i -> return_expr @@ CST.E_Int (Wrap.ghost (Z.to_string i, i))
    | Literal_nat n -> return_expr @@ CST.E_Nat (Wrap.ghost (Z.to_string n, n))
    | Literal_timestamp time ->
      let time =
        Tezos_utils.Time.Protocol.to_notation
        @@ Tezos_utils.Time.Protocol.of_seconds
        @@ Z.to_int64 time
      in
      (* TODO combinators for CSTs. *)
      let ty = decompile_type_expr @@ AST.t_timestamp ~loc () in
      let time = CST.E_String (Wrap.ghost time) in
      return_typed time ty
    | Literal_mutez mtez ->
      let str = Z.to_string mtez in
      return_expr @@ CST.E_Mutez (Wrap.ghost (str, Z.to_int64 mtez))
    | Literal_string (Standard str) -> return_expr @@ CST.E_String (Wrap.ghost str)
    | Literal_string (Verbatim ver) -> return_expr @@ CST.E_Verbatim (Wrap.ghost ver)
    | Literal_bytes b ->
      let b = Hex.of_bytes b in
      let s = Hex.to_string b in
      return_expr @@ CST.E_Bytes (Wrap.ghost (s, b))
    | Literal_address addr ->
      let addr = CST.E_String (Wrap.ghost addr) in
      let ty = decompile_type_expr @@ AST.t_address ~loc () in
      return_typed addr ty
    | Literal_signature sign ->
      let sign = CST.E_String (Wrap.ghost sign) in
      let ty = decompile_type_expr @@ AST.t_signature ~loc () in
      return_typed sign ty
    | Literal_key k ->
      let k = CST.E_String (Wrap.ghost k) in
      let ty = decompile_type_expr @@ AST.t_key ~loc () in
      return_typed k ty
    | Literal_key_hash kh ->
      let kh = CST.E_String (Wrap.ghost kh) in
      let ty = decompile_type_expr @@ AST.t_key_hash ~loc () in
      return_typed kh ty
    | Literal_chain_id _ | Literal_operation _ ->
      failwith
        "chain_id, operation are not created currently ?" (* TODO : REMOVE THIS!! *)
    | Literal_bls12_381_g1 b ->
      let b = Hex.of_bytes b in
      let s = Hex.to_string b in
      let b = CST.E_Bytes (Wrap.ghost (s, b)) in
      let ty = decompile_type_expr @@ AST.t_bls12_381_g1 ~loc () in
      return_typed b ty
    | Literal_bls12_381_g2 b ->
      let b = Hex.of_bytes b in
      let s = Hex.to_string b in
      let b = CST.E_Bytes (Wrap.ghost (s, b)) in
      let ty = decompile_type_expr @@ AST.t_bls12_381_g2 ~loc () in
      return_typed b ty
    | Literal_bls12_381_fr b ->
      let b = Hex.of_bytes b in
      let s = Hex.to_string b in
      let b = CST.E_Bytes (Wrap.ghost (s, b)) in
      let ty = decompile_type_expr @@ AST.t_bls12_381_fr ~loc () in
      return_typed b ty
    | Literal_chest _ | Literal_chest_key _ ->
      failwith "chest / chest_key not allowed in the syntax (only tests need this type)")
  | E_application { lamb; args } ->
    let lamb = decompile_expression lamb in
    let args = decompile_arguments @@ get_e_tuple args in
    (match output with
    | Expression -> return_expr @@ CST.E_App (Region.wrap_ghost (lamb, args))
    | Statements -> return_inst @@ CST.I_Call (Region.wrap_ghost (lamb, args)))
  | E_lambda lambda ->
    let parameters, ret_type, return = decompile_lambda lambda in
    let fun_expr : CST.fun_expr =
      { kwd_function = Token.ghost_function
      ; type_params = None
      ; parameters
      ; ret_type
      ; kwd_is = Wrap.ghost ""
      ; return
      }
    in
    return_expr_with_par @@ CST.E_Fun (Region.wrap_ghost @@ fun_expr)
  | E_type_abstraction _ -> failwith "type_abstraction not supported yet"
  | E_recursive _ ->
    failwith "corner case : annonymous recursive function" (* TODO : REMOVE THIS!! *)
  | E_let_in { let_binder; rhs; let_result; attributes } ->
    let lin =
      let attributes = Shared_helpers.decompile_attributes attributes in
      let wrap_attr x =
        List.fold
          ~f:(fun acc attr -> CST.D_Attr (Region.wrap_ghost (attr, acc)))
          ~init:x
          attributes
      in
      let pattern = decompile_pattern let_binder in
      let init = decompile_expression rhs in
      let const_decl : CST.const_decl =
        { kwd_const = Token.ghost_const
        ; pattern
        ; type_params = None
        ; const_type = None
        ; equal = Token.ghost_eq
        ; init
        ; terminator
        }
      in
      wrap_attr @@ CST.D_Const (Region.wrap_ghost const_decl)
    in
    let lst, expr = decompile_eos Expression let_result in
    let lst =
      match lst with
      | Some lst -> List.Ne.cons (CST.S_Decl lin) lst
      | None -> CST.S_Decl lin, []
    in
    return @@ (Some lst, expr)
  | E_type_in { type_binder; rhs; let_result } ->
    let kwd_type = Token.ghost_type
    and name = decompile_type_var type_binder
    and kwd_is = Token.ghost_is in
    let type_expr = decompile_type_expr rhs in
    let terminator = terminator in
    let tin =
      Region.wrap_ghost
      @@ CST.{ kwd_type; name; kwd_is; type_expr; terminator; params = None }
    in
    let lst, expr = decompile_eos Expression let_result in
    let lst =
      match lst with
      | Some lst -> List.Ne.cons (CST.S_Decl (CST.D_Type tin)) lst
      | None -> CST.S_Decl (CST.D_Type tin), []
    in
    return @@ (Some lst, expr)
  | E_mod_in { module_binder; rhs; let_result } ->
    let module_expr = decompile_module_expression rhs in
    let module_decl : CST.module_decl =
      { kwd_module = Token.ghost_module
      ; name = Wrap.ghost (Module_var.to_name_exn module_binder)
      ; kwd_is = Token.ghost_is
      ; module_expr
      ; terminator
      }
    in
    let lst, expr = decompile_eos Expression let_result in
    let d = CST.S_Decl (CST.D_Module (Region.wrap_ghost module_decl)) in
    let lst =
      match lst with
      | Some lst -> List.Ne.cons d lst
      | None -> d, []
    in
    return @@ (Some lst, expr)
  | E_raw_code { language; code } ->
    let language : CST.language = Wrap.ghost @@ Region.wrap_ghost @@ language in
    let code = decompile_expression code in
    let ci : CST.code_inj = { language; code; rbracket = Token.ghost_rbracket } in
    return_expr @@ CST.E_CodeInj (Region.wrap_ghost ci)
  | E_constructor { constructor; element } ->
    let (Label constr) = constructor in
    let constr = Wrap.ghost constr in
    let element = decompile_arguments @@ get_e_tuple element in
    return_expr_with_par @@ CST.E_App (Region.wrap_ghost (CST.E_Ctor constr, element))
  | E_matching { matchee; cases } ->
    let expr = decompile_expression matchee in
    let opening, closing = enclosing_brackets in
    let aux
        decompile_f
        ({ pattern; body } :
          (AST.expression, AST.type_expression option) AST.Match_expr.match_case)
      =
      let pattern = decompile_pattern pattern in
      let rhs = decompile_f body in
      let clause : _ CST.case_clause = { pattern; arrow = Token.ghost_arrow; rhs } in
      Region.wrap_ghost clause
    in
    (match output with
    | Expression ->
      let cases = List.map ~f:(aux decompile_expression) cases in
      let cases = list_to_nsepseq ~sep:Token.ghost_vbar cases in
      let cases : _ CST.case =
        { kwd_case = Token.ghost_case
        ; expr
        ; kwd_of = Token.ghost_of
        ; opening
        ; lead_vbar
        ; cases
        ; closing
        }
      in
      return_expr @@ CST.E_Case (Region.wrap_ghost cases)
    | Statements ->
      let cases = List.map ~f:(aux decompile_if_clause) cases in
      let cases : (CST.test_clause CST.case_clause CST.reg, CST.vbar) Utils.nsepseq =
        list_to_nsepseq ~sep:Token.ghost_vbar cases
      in
      let cases : CST.test_clause CST.case =
        { kwd_case = Token.ghost_case
        ; expr
        ; kwd_of = Token.ghost_of
        ; opening
        ; lead_vbar
        ; cases
        ; closing
        }
      in
      return_inst @@ CST.I_Case (Region.wrap_ghost cases))
  | E_record struct_ ->
    let aux (Label.Label str, expr) =
      let field_name = Wrap.ghost str in
      let field_rhs = decompile_expression expr in
      let field : (CST.expr, CST.expr) CST.field =
        Complete
          { field_lhs = E_Var field_name
          ; field_lens = Lens_Id Token.ghost_ass
          ; field_rhs
          ; attributes = []
          }
      in
      Region.wrap_ghost field
    in
    let struct_ = List.map ~f:aux struct_ in
    let struct_ = list_to_sepseq ~sep:Token.ghost_semi struct_ in
    let struct_ = inject Token.ghost_record struct_ in
    return_expr @@ CST.E_Record (Region.wrap_ghost struct_)
  | E_accessor { struct_; path } ->
    let rec aux
        :  AST.expression -> AST.expression Access_path.t
        -> AST.expression * AST.expression Access_path.t
      =
     fun e acc_path ->
      match e.expression_content with
      | E_accessor { struct_; path } -> aux struct_ (path @ acc_path)
      | _ -> e, acc_path
    in
    let struct_, path = aux struct_ path in
    (match List.rev path with
    | [ Access_map e ] ->
      let var, lst = get_e_accessor @@ struct_ in
      let map = decompile_to_path var lst in
      let e = decompile_expression e in
      let keys = Region.wrap_ghost @@ brackets @@ e, [] in
      let mlu : CST.map_lookup = { map; keys } in
      return_expr @@ CST.E_MapLookup (Region.wrap_ghost @@ mlu)
    | Access_map e :: lst ->
      let path = List.rev lst in
      let field_path =
        list_to_nsepseq ~sep:Token.ghost_dot @@ List.map ~f:decompile_to_selection path
      in
      let struct_name = CST.E_Var (decompile_variable @@ get_e_variable struct_) in
      let proj =
        CST.{ record_or_tuple = struct_name; selector = Token.ghost_dot; field_path }
      in
      let map = CST.E_Proj (Region.wrap_ghost proj) in
      let e = decompile_expression e in
      let keys = Region.wrap_ghost @@ brackets @@ e, [] in
      let mlu : CST.map_lookup = { map; keys } in
      return_expr @@ CST.E_MapLookup (Region.wrap_ghost @@ mlu)
    | _ ->
      let field_path =
        list_to_nsepseq ~sep:Token.ghost_dot @@ List.map ~f:decompile_to_selection path
      in
      let record_or_tuple = CST.E_Var (decompile_variable @@ get_e_variable struct_) in
      let proj = CST.{ record_or_tuple; selector = Token.ghost_dot; field_path } in
      return_expr @@ CST.E_Proj (Region.wrap_ghost proj))
  | E_update { struct_; path; update } ->
    let struct_ = decompile_expression struct_ in
    let update = decompile_expression update in
    let structure =
      let aux (access : AST.expression Access_path.access) =
        match access with
        | Access_record field -> CST.FieldName (Wrap.ghost field)
        | Access_tuple z -> CST.Component (Wrap.ghost (Z.to_string z, z))
        | Access_map _ -> failwith "map access in struct_ update"
      in
      let field_path = list_to_nsepseq ~sep:Token.ghost_dot (List.map ~f:aux path) in
      CST.(
        E_Proj
          (Region.wrap_ghost
             { record_or_tuple = struct_; field_path; selector = Token.ghost_dot }))
    in
    let upd = CST.{ structure; kwd_with = Token.ghost_with; update } in
    return_expr @@ CST.E_Update (Region.wrap_ghost upd)
  | E_ascription { anno_expr; type_annotation } ->
    let expr = decompile_expression anno_expr in
    let ty = decompile_type_expr type_annotation in
    let ascr = expr, (Token.ghost_colon, ty) in
    return_expr @@ CST.E_Typed (Region.wrap_ghost @@ par ascr)
  | E_module_accessor { module_path; element } ->
    let lst = List.map ~f:decompile_mod_var module_path in
    let module_path = list_to_nsepseq ~sep:Token.ghost_dot lst in
    let field = CST.E_Var (decompile_variable element) in
    return_expr
    @@ CST.E_ModPath
         (Region.wrap_ghost CST.{ module_path; selector = Token.ghost_dot; field })
  | E_cond { condition; then_clause; else_clause } ->
    let test = decompile_expression condition in
    let decompile_conditional : type a. a -> a -> a CST.conditional =
     fun if_so if_not ->
      let if_not = Some (Token.ghost_else, if_not) in
      CST.{ kwd_if = Token.ghost_if; test; kwd_then = Token.ghost_then; if_so; if_not }
    in
    (match output with
    | Expression ->
      let if_so = decompile_expression then_clause in
      let if_not = decompile_expression else_clause in
      let cond = decompile_conditional if_so if_not in
      return_expr @@ CST.E_Cond (Region.wrap_ghost cond)
    | Statements ->
      let if_so = decompile_if_clause then_clause in
      let if_not = decompile_if_clause else_clause in
      let cond = decompile_conditional if_so if_not in
      return_inst @@ CST.I_Cond (Region.wrap_ghost cond))
  | E_sequence { expr1; expr2 } ->
    let expr1 = decompile_statements expr1 in
    let expr2, next = decompile_eos output expr2 in
    let expr1 =
      Option.value ~default:expr1 @@ Option.map ~f:(List.Ne.append expr1) expr2
    in
    return @@ (Some expr1, next)
  | E_skip _ -> return_inst @@ CST.I_Skip Token.ghost_skip
  | E_tuple tuple ->
    let tuple = List.map ~f:decompile_expression tuple in
    let tuple = list_to_nsepseq ~sep:Token.ghost_comma tuple in
    return_expr @@ CST.E_Tuple (Region.wrap_ghost @@ par tuple)
  | E_map map ->
    let map = List.map ~f:(Pair.map ~f:decompile_expression) map in
    let aux (key, value) =
      let binding : CST.binding = { key; arrow = Token.ghost_arrow; value } in
      Region.wrap_ghost @@ binding
    in
    let map = list_to_sepseq ~sep:Token.ghost_semi @@ List.map ~f:aux map in
    let compound : CST.binding CST.reg CST.compound = inject Token.ghost_map map in
    return_expr @@ CST.E_Map (Region.wrap_ghost compound)
  | E_big_map big_map ->
    let big_map = List.map ~f:(Pair.map ~f:decompile_expression) big_map in
    let aux (key, value) =
      let binding : CST.binding = { key; arrow = Token.ghost_arrow; value } in
      Region.wrap_ghost @@ binding
    in
    let big_map = list_to_sepseq ~sep:Token.ghost_semi @@ List.map ~f:aux big_map in
    let compound : CST.binding CST.reg CST.compound =
      inject Token.ghost_big_map big_map
    in
    return_expr @@ CST.E_BigMap (Region.wrap_ghost @@ compound)
  | E_list lst ->
    let lst = List.map ~f:decompile_expression lst in
    let lst = list_to_sepseq ~sep:Token.ghost_semi lst in
    let compound : CST.expr CST.compound = inject Token.ghost_list lst in
    return_expr @@ CST.E_List (Region.wrap_ghost @@ compound)
  | E_set set ->
    let set = List.map ~f:decompile_expression set in
    let set = list_to_sepseq ~sep:Token.ghost_semi set in
    let compound : CST.expr CST.compound = inject Token.ghost_set set in
    return_expr @@ CST.E_Set (Region.wrap_ghost @@ compound)
  | E_assign { binder; expression } ->
    let lhs = decompile_to_lhs (Binder.get_var binder) [] in
    let rhs = decompile_expression expression in
    let assign : CST.assignment = { lhs; assign = Token.ghost_ass; rhs } in
    return_inst @@ I_Assign (Region.wrap_ghost assign)
  | E_for { binder; start; final; incr; f_body } ->
    let index = decompile_variable binder in
    let init = decompile_expression start in
    let bound = decompile_expression final in
    let step = decompile_expression incr in
    let step = Some (Wrap.ghost "", step) in
    let block, _next = decompile_to_block f_body in
    let block = Region.wrap_ghost @@ Option.value ~default:empty_block block in
    let for_int : CST.for_int =
      { kwd_for = Token.ghost_for
      ; index
      ; assign = Token.ghost_ass
      ; init
      ; kwd_to = Token.ghost_to
      ; bound
      ; step
      ; block
      }
    in
    return_inst @@ CST.I_For (Region.wrap_ghost for_int)
  | E_for_each { fe_binder; collection; fe_body; collection_type } ->
    let var = decompile_variable @@ fst fe_binder in
    let collection = decompile_expression collection in
    let block, _next = decompile_to_block fe_body in
    let block = Region.wrap_ghost @@ Option.value ~default:empty_block block in
    let for_in : CST.for_in =
      let open CST in
      match collection_type with
      | Map ->
        let binding =
          match snd fe_binder with
          | Some x -> var, Token.ghost_arrow, decompile_variable x
          | None -> failwith "Wrong for each"
        in
        ForMap
          (Region.wrap_ghost
             { kwd_for = Token.ghost_for
             ; binding
             ; kwd_in = Token.ghost_in
             ; kwd_map = Token.ghost_map
             ; collection
             ; block
             })
      | Set ->
        ForSetOrList
          (Region.wrap_ghost
             { kwd_for = Token.ghost_for
             ; var
             ; kwd_in = Token.ghost_in
             ; for_kind = `Set Token.ghost_set
             ; collection
             ; block
             })
      | List ->
        ForSetOrList
          (Region.wrap_ghost
             { kwd_for = Token.ghost_for
             ; var
             ; kwd_in = Token.ghost_in
             ; for_kind = `List Token.ghost_list
             ; collection
             ; block
             })
      | Any -> failwith "TODO : have the type of the collection propagated from AST_typed"
    in
    return_inst @@ CST.I_ForIn for_in
  | E_while { cond; body } ->
    let cond = decompile_expression cond in
    let block, _next = decompile_to_block body in
    let block = Region.wrap_ghost @@ Option.value ~default:empty_block block in
    let while_ : CST.while_loop = { kwd_while = Token.ghost_while; cond; block } in
    return_inst @@ CST.I_While (Region.wrap_ghost while_)
  | E_let_mut_in { let_binder; rhs; let_result; _ } ->
    let pattern =
      match let_binder.wrap_content with
      | P_var x -> Binder.get_ascr x
      | P_unit | P_list _ | P_variant (_, _) | P_tuple _ | P_record _ -> None
    in
    let var_type = Option.map ~f:(prefix_colon <@ decompile_type_expr) @@ pattern in
    let rhs = decompile_expression rhs in
    let var_decl =
      Region.wrap_ghost
      @@ { CST.kwd_var = Token.ghost_var
         ; pattern = decompile_pattern let_binder
         ; type_params = None
         ; var_type
         ; assign = Token.ghost_ass
         ; init = rhs
         ; terminator = Some Token.ghost_semi
         }
    in
    let lst, expr = decompile_eos Expression let_result in
    let lst =
      match lst with
      | Some lst -> List.Ne.cons (CST.S_VarDecl var_decl) lst
      | None -> CST.S_VarDecl var_decl, []
    in
    return @@ (Some lst, expr)


and decompile_if_clause : AST.expression -> CST.test_clause =
 fun e ->
  let clause = decompile_statements e in
  match clause with
  | CST.S_Instr instr, [] -> CST.ClauseInstr instr
  | _ ->
    let clause = List.Ne.to_list clause in
    let statements = list_to_nsepseq ~sep:Token.ghost_semi clause in
    CST.ClauseBlock (Region.wrap_ghost @@ to_block statements)


and decompile_to_data_decl
    : _ Binder.t -> AST.expression -> AST.attributes -> CST.declaration
  =
 fun binder expr attributes ->
  let name = decompile_variable @@ Binder.get_var binder in
  let const_type =
    Option.map ~f:(prefix_colon <@ decompile_type_expr) @@ Binder.get_ascr binder
  in
  let attributes = Shared_helpers.decompile_attributes attributes in
  let fun_name = name in
  let wrap_attr x =
    List.fold
      ~f:(fun acc attr -> CST.D_Attr (Region.wrap_ghost (attr, acc)))
      ~init:x
      attributes
  in
  match expr.expression_content with
  | E_lambda lambda ->
    let parameters, ret_type, return = decompile_lambda lambda in
    let fun_decl =
      fun_decl ~is_rec:false fun_name parameters ret_type return terminator
    in
    wrap_attr @@ CST.D_Fun (Region.wrap_ghost fun_decl)
  | E_recursive { lambda; _ } ->
    let lambda = Lambda.map Fun.id Option.return lambda in
    let parameters, ret_type, return = decompile_lambda lambda in
    let fun_decl = fun_decl ~is_rec:true fun_name parameters ret_type return terminator in
    wrap_attr @@ CST.D_Fun (Region.wrap_ghost fun_decl)
  | _ ->
    let init = decompile_expression expr in
    let pattern = CST.P_Var name in
    let const_decl : CST.const_decl =
      { kwd_const = Token.ghost_const
      ; pattern
      ; type_params = None
      ; const_type
      ; equal = Token.ghost_eq
      ; init
      ; terminator
      }
    in
    wrap_attr @@ CST.D_Const (Region.wrap_ghost const_decl)


and decompile_to_lhs : Value_var.t -> _ Access_path.t -> CST.expr =
 fun var access ->
  match List.rev access with
  | [] -> E_Var (decompile_variable var)
  | hd :: tl ->
    (match hd with
    | Access_map e ->
      let map = decompile_to_path var @@ List.rev tl in
      let keys = (Region.wrap_ghost <@ brackets) @@ decompile_expression e, [] in
      let mlu : CST.map_lookup = { map; keys } in
      CST.E_MapLookup (Region.wrap_ghost @@ mlu)
    | Access_tuple _ | Access_record _ -> decompile_to_path var @@ access)


and decompile_to_path : Value_var.t -> _ Access_path.t -> CST.expr =
 fun var access ->
  let struct_name = decompile_variable var in
  match access with
  | [] -> E_Var struct_name
  | lst ->
    let field_path =
      list_to_nsepseq ~sep:Token.ghost_dot @@ List.map ~f:decompile_to_selection lst
    in
    let projection : CST.projection =
      { record_or_tuple = E_Var struct_name; selector = Token.ghost_dot; field_path }
    in
    CST.E_Proj (Region.wrap_ghost @@ projection)


and decompile_to_selection : _ Access_path.access -> CST.selection =
 fun access ->
  match access with
  | Access_tuple index -> CST.Component (Wrap.ghost (Z.to_string index, index))
  | Access_record str -> CST.FieldName (Wrap.ghost str)
  | Access_map _ ->
    failwith "Can't decompile access_map to selection" (* TODO : REMOVE THIS!! *)


and decompile_lambda
    :  (AST.expr, AST.ty_expr option) Lambda.t
    -> CST.parameters * CST.type_annotation option * CST.expr
  =
 fun { binder; result; output_type = _ } ->
  let var = decompile_variable @@ Param.get_var binder in
  let param_type =
    Option.map ~f:(prefix_colon <@ decompile_type_expr) @@ Param.get_ascr binder
  in
  let param_const : CST.param_decl =
    { param_kind = `Const Token.ghost_const; pattern = CST.P_Var var; param_type }
  in
  let parameters : CST.parameters =
    Region.wrap_ghost
    @@ par (list_to_sepseq ~sep:Token.ghost_comma [ Region.wrap_ghost param_const ])
  in
  let result, ret_type =
    match result.expression_content with
    | AST.E_ascription { anno_expr; type_annotation } ->
      let ret_type = prefix_colon @@ decompile_type_expr type_annotation in
      anno_expr, Some ret_type
    | _ -> result, None
  in
  let return = decompile_expression result in
  parameters, ret_type, return


and decompile_declaration : AST.declaration -> CST.declaration =
 fun decl ->
  let decl = Location.unwrap decl in
  let wrap_attr attr x =
    List.fold ~f:(fun acc attr -> CST.D_Attr (Region.wrap_ghost (attr, acc))) ~init:x attr
  in
  match decl with
  | D_type { type_binder; type_expr; type_attr = _ } ->
    let kwd_type = Token.ghost_type
    and name = decompile_type_var type_binder
    and kwd_is = Token.ghost_is in
    let (params : CST.variable CST.tuple option) =
      match type_expr.type_content with
      | T_abstraction _ ->
        let rec aux : AST.type_expression -> _ list -> _ list =
         fun t lst ->
          match t.type_content with
          | T_abstraction x -> aux x.type_ (x.ty_binder :: lst)
          | _ -> lst
        in
        let vars = aux type_expr [] in
        let params = type_vars_of_list @@ List.map ~f:decompile_type_var vars in
        Some params
      | _ -> None
    in
    let type_expr = decompile_type_expr type_expr in
    CST.D_Type
      (Region.wrap_ghost CST.{ kwd_type; name; kwd_is; type_expr; terminator; params })
  | D_value { binder; attr; expr } ->
    let attributes = Shared_helpers.decompile_attributes attr in
    let name = decompile_variable @@ Binder.get_var binder in
    let fun_name = name in
    (match expr.expression_content with
    | E_lambda lambda ->
      let parameters, ret_type, return = decompile_lambda lambda in
      let fun_decl =
        fun_decl ~is_rec:false fun_name parameters ret_type return terminator
      in
      wrap_attr attributes @@ CST.D_Fun (Region.wrap_ghost fun_decl)
    | E_recursive { lambda; _ } ->
      let lambda = Lambda.map Fun.id Option.return lambda in
      let parameters, ret_type, return = decompile_lambda lambda in
      let fun_decl =
        fun_decl ~is_rec:true fun_name parameters ret_type return terminator
      in
      wrap_attr attributes @@ CST.D_Fun (Region.wrap_ghost fun_decl)
    | _ ->
      let const_type =
        Option.map ~f:(prefix_colon <@ decompile_type_expr) @@ Binder.get_ascr binder
      in
      let init = decompile_expression expr in
      let const_decl : CST.const_decl =
        { kwd_const = Token.ghost_const
        ; pattern = CST.P_Var name
        ; type_params = None
        ; const_type
        ; equal = Token.ghost_eq
        ; init
        ; terminator
        }
      in
      wrap_attr attributes @@ CST.D_Const (Region.wrap_ghost const_decl))
  | D_irrefutable_match { pattern; attr; expr } ->
    let attributes = Shared_helpers.decompile_attributes attr in
    let pattern = decompile_pattern pattern in
    let init = decompile_expression expr in
    let const_decl : CST.const_decl =
      { kwd_const = Token.ghost_const
      ; pattern
      ; type_params = None
      ; const_type = None
      ; equal = Token.ghost_eq
      ; init
      ; terminator
      }
    in
    wrap_attr attributes @@ CST.D_Const (Region.wrap_ghost const_decl)
  | D_module { module_binder; module_; module_attr } ->
    let module_attr = Shared_helpers.decompile_attributes module_attr in
    let module_decl : CST.module_decl =
      { kwd_module = Token.ghost_module
      ; name = Wrap.ghost (Module_var.to_name_exn module_binder)
      ; kwd_is = Token.ghost_is
      ; module_expr = decompile_module_expression module_
      ; terminator
      }
    in
    wrap_attr module_attr @@ CST.D_Module (Region.wrap_ghost module_decl)


and decompile_decl : AST.decl -> CST.declaration = fun d -> decompile_declaration d

and decompile_module : AST.module_ -> CST.declaration Utils.nseq =
 fun m ->
  let decl = List.map ~f:decompile_decl m in
  List.Ne.of_list decl


and decompile_module_expression : AST.module_expr -> CST.module_expr =
 fun me ->
  match me.wrap_content with
  | M_struct decls ->
    let declarations = decompile_module decls in
    let module_body =
      CST.{ enclosing = BeginEnd (Token.ghost_begin, Token.ghost_end); declarations }
    in
    CST.M_Body (Region.wrap_ghost module_body)
  | M_module_path path ->
    let hd, tl = path in
    let field, tl' =
      match List.rev tl with
      | [] -> failwith "impossible"
      | field :: tl' -> field, tl'
    in
    let module_path =
      nelist_to_npseq ~sep:Token.ghost_dot
      @@ List.Ne.map
           (fun (x : Module_var.t) -> Wrap.ghost (Module_var.to_name_exn x))
           (hd, tl')
    in
    let field = Wrap.ghost (Module_var.to_name_exn field) in
    CST.(M_Path (Region.wrap_ghost { module_path; field; selector = Token.ghost_dot }))
  | M_variable v -> CST.M_Var (Wrap.ghost (Module_var.to_name_exn v))


and decompile_program : AST.program -> CST.ast =
 fun prg ->
  let decl = List.Ne.of_list @@ List.map ~f:decompile_declaration prg in
  { decl; eof = Token.ghost_eof }
