(* A pretty printer for JsLIGO *)

[@@@warning "-42"]

(* Jane Street dependency *)

module List = Core.List

(* Vendored dependencies *)

module Utils  = Simple_utils.Utils
module Region = Simple_utils.Region
module Option = Simple_utils.Option

(* Local dependencies *)

module CST = Cst_jsligo.CST
module PrettyComb = Parsing_shared.PrettyComb

(* Global openings *)

open CST
open! Region
open! PPrint


(* Utilities and local shadowings *)

type state = PrettyComb.state

let prefix = PrettyComb.prefix
let (^/^)  = PrettyComb.(^/^)

(* Placement *)

let default_state : state =
  object
    method indent       = 2
    method leading_vbar = PrettyComb.Only_on_new_line
  end

(* Comments *)

let print_line_comment comment = string "//" ^^ string comment.value

let print_block_comment comment =
  string "/*" ^^ string comment.value ^^ string "*/"

let print_line_comment_opt ?(sep=empty) prefix = function
  Some comment -> prefix ^^ space ^^ print_line_comment comment ^^ hardline
| None -> prefix ^^ sep

let print_comment = function
  Wrap.Block comment -> print_block_comment comment
| Wrap.Line  comment -> print_line_comment  comment

let print_comments = function
  [] -> empty
| comments -> separate_map hardline print_comment comments ^^ hardline

(* Tokens *)

let token ?(sep=empty) (t : string Wrap.t) : document =
  let prefix = print_comments t#comments ^/^ string t#payload
  in print_line_comment_opt ~sep prefix t#line_comment

let print_variable ?(sep=empty) = function
  Var t -> token ~sep t
| Esc t ->
    let prefix = print_comments t#comments ^/^ string ("@" ^ t#payload)
    in print_line_comment_opt ~sep prefix t#line_comment

(* Enclosed documents *)

let print_enclosed_document
    state ?(force_hardline : bool option) (thread : document)
    break_size left right =
  let left, right = token left, token right in
  group (
    match force_hardline with
      None | Some false ->
        nest state#indent (left ^^ break break_size ^^ thread)
        ^^ break break_size ^^ right
    | Some true ->
        nest state#indent (left ^^ hardline ^^ thread)
        ^^ hardline ^^ right)

(* HIGHER-ORDER PRINTERS *)

let print_braces_like_document
  state inside ?(force_hardline : bool option) left right =
  print_enclosed_document state ?force_hardline inside 1 left right

let print_braces state print
  ?(force_hardline : bool option) (node : 'a braces) =
  let {lbrace; inside; rbrace} = node.value in
  print_braces_like_document
    state ?force_hardline (print inside) lbrace rbrace

let print_brackets_like_document
  state inside ?(force_hardline : bool option) left right =
  print_enclosed_document state ?force_hardline inside 0 left right

let print_brackets state print (node : 'a brackets) =
  let {lbracket; inside; rbracket} = node.value in
  print_brackets_like_document
    state ~force_hardline:false (print inside) lbracket rbracket

let print_chevrons_like_document
  state inside ?(force_hardline : bool option) left right =
  print_enclosed_document state ?force_hardline inside 0 left right

let print_chevrons state print (node : 'a chevrons) =
  let {lchevron; inside; rchevron} = node.value in
  print_chevrons_like_document
    state ~force_hardline:false (print inside) lchevron rchevron

let print_par_like_document
  state inside ?(force_hardline : bool option) left right =
  print_enclosed_document state ?force_hardline inside 0 left right

let print_par state print (node : 'a par) =
  let {lpar; inside; rpar} = node.value in
  print_par_like_document
    state ~force_hardline:false (print inside) lpar rpar

(* The separator [sep] here represents some extra spacing (like spaces
   or newlines) that will be printed after every separator in a
   sequence of type [Utils.nsepseq]. *)

let print_nsepseq :
  'a.document ->
  ('a -> document) -> ('a, lexeme Wrap.t) Utils.nsepseq -> document =
  fun sep print elements ->
    let hd, tl = elements in
    let rec separate_map = function
      []            -> empty
    | (sep', x)::xs -> token ~sep sep' ^^ print x ^^ separate_map xs
    in print hd ^^ separate_map tl

(*
let print_sepseq :
  'a.document -> ('a -> document) ->
  ('a, lexeme wrap) Utils.sepseq -> document =
  fun sep print -> function
    None     -> empty
  | Some seq -> print_nsepseq sep print seq
*)

let print_nseq : 'a.document -> ('a -> document) -> 'a Utils.nseq -> document =
  fun sep print (head, tail) -> separate_map sep print (head::tail)

let print_nsep_or_term :
  'a.document -> ('a -> document) ->
  ('a, lexeme wrap) Utils.nsep_or_term -> document =
  fun sep print -> function
    `Sep  seq -> print_nsepseq sep print seq
  | `Term seq -> let print (item, term) = print item ^^ token term
                 in print_nseq sep print seq

let print_sep_or_term :
  'a.document -> ('a -> document) ->
  ('a, lexeme wrap) Utils.sep_or_term -> document =
  fun sep print -> function
    None     -> empty
  | Some seq -> print_nsep_or_term sep print seq

(* Enclosed structures *)

let is_enclosed_expr = function
  E_Par _ | E_Array _ | E_Object _ | E_Update _ | E_Do _ -> true
| _ -> false

let is_enclosed_statement = function
  S_Block _ -> true
| S_Expr  e -> is_enclosed_expr e
| _         -> false

let is_enclosed_type = function
  T_Par _ | T_Array _ | T_Object _ -> true
| _ -> false

(* UTILITIES *)

(* let (<@) f g x = f (g x) *)

let unroll_S_Attr (attr, stmt) =
  let rec aux attrs = function
    S_Attr (attr, stmt) -> aux (attr :: attrs) stmt
  | stmt                -> List.rev attrs, stmt
  in aux [attr] stmt

let unroll_I_Attr (attr, entry) =
  let rec aux attrs = function
    I_Attr (attr, entry) -> aux (attr :: attrs) entry
  | entry                -> List.rev attrs, entry
  in aux [attr] entry

let unroll_T_Attr (attr, t_expr) =
  let rec aux attrs = function
    T_Attr (attr, t_expr) -> aux (attr :: attrs) t_expr
  | t_expr                -> List.rev attrs, t_expr
  in aux [attr] t_expr

let unroll_P_Attr (attr, pattern) =
  let rec aux attrs = function
    P_Attr (attr, pattern) -> aux (attr :: attrs) pattern
  | pattern                -> List.rev attrs, pattern
  in aux [attr] pattern

let unroll_E_Attr (attr, expr) =
  let rec aux attrs = function
    E_Attr (attr, expr) -> aux (attr :: attrs) expr
  | expr                -> List.rev attrs, expr
  in aux [attr] expr

(* PRINTING LITERALS *)

let print_bytes (node : (lexeme * Hex.t) wrap) =
  let prefix = print_comments node#comments
               ^/^ string ("0x" ^ Hex.show (snd node#payload))
  in print_line_comment_opt prefix node#line_comment

let print_mutez (node : (lexeme * Int64.t) wrap) =
  let prefix =
    print_comments node#comments
    ^/^ (Int64.to_string (snd node#payload) ^ "mutez" |> string)
  in print_line_comment_opt prefix node#line_comment

let print_ctor (node : ctor) = token node

let print_string (node : lexeme wrap) = dquotes (token node)

and print_verbatim (node : lexeme wrap) = bquotes (token node)

let print_int (node : (lexeme * Z.t) wrap) =
  let prefix = print_comments node#comments
               ^/^ string (Z.to_string (snd node#payload))
  in print_line_comment_opt prefix node#line_comment

and print_nat (node : (lexeme * Z.t) wrap) =
  let prefix = print_comments node#comments
               ^/^ string (Z.to_string (snd node#payload) ^ "n")
  in print_line_comment_opt prefix node#line_comment

(* Attributes *)

let print_attribute state (node : Attr.t wrap) =
  let key, val_opt = node#payload in
  let thread = string "@" ^^ string key in
  let thread = match val_opt with
                 Some String value ->
                   thread ^^ string "("
                   ^^ nest state#indent (string ("\"" ^ value ^ "\""))
                   ^^ string ")"
              | _ -> thread
  in group (print_comments node#comments ^/^ thread)

let print_attributes state thread = function
  []    -> thread
| attrs -> separate_map (break 0) (print_attribute state) attrs ^/^ thread

(* PRINTING THE CST *)

let rec print state (node : CST.t) =
  let {statements; eof} = node in
  let prog = Utils.nseq_to_list statements
             |> List.map ~f:(print_statement_semi state)
             |> separate_map (hardline ^^ hardline) group
             |> Fun.flip ( ^^ ) hardline
  in match eof#comments with
       [] -> prog
     | comments -> prog ^/^ print_comments comments

and print_statement state = function
  S_Attr      s -> print_S_Attr      state s
| S_Block     s -> print_S_Block     state s
| S_Break     s -> print_S_Break     state s
| S_Continue  s -> print_S_Continue  state s
| S_Decl      s -> print_S_Decl      state s
| S_Directive s -> print_S_Directive state s
| S_Export    s -> print_S_Export    state s
| S_Expr      s -> print_S_Expr      state s
| S_For       s -> print_S_For       state s
| S_ForOf     s -> print_S_ForOf     state s
| S_If        s -> print_S_If        state s
| S_Return    s -> print_S_Return    state s
| S_Switch    s -> print_S_Switch    state s
| S_While     s -> print_S_While     state s

(* Decorated statements *)

and print_S_Attr state (node : attribute * statement) =
  let attributes, stmt = unroll_S_Attr node in
  let thread = print_statement state stmt
  in print_attributes state thread attributes

(* Blocks of statements *)

and print_S_Block state (node : statements braces) =
  print_block state node

and print_block state (node : statements braces) =
  print_braces ~force_hardline:true state (print_statements state) node

and print_statements state (node : statements) =
  print_nseq (break 1) (print_statement_semi state) node

and print_statement_semi state (node : statement * semi option) =
  let statement, semi_opt = node in
  let thread = print_statement state statement in
  thread ^^ Option.value_map semi_opt ~default:empty ~f:token

(* Break statement *)

and print_S_Break state (node : kwd_break) = token node

(* Continue statement *)

and print_S_Continue state (node : kwd_continue) = token node

(* Declarations as statements *)

and print_S_Decl state (node : declaration) = print_declaration state node

and print_declaration state = function
  D_Fun       d -> print_D_Fun       state d
| D_Import    d -> print_D_Import    state d
| D_Interface d -> print_D_Interface state d
| D_Namespace d -> print_D_Namespace state d
| D_Type      d -> print_D_Type      state d
| D_Value     d -> print_D_Value     state d

(* Function declaration *)

and print_D_Fun state (node : fun_decl reg) =
  let {kwd_function; fun_name; generics;
       parameters; rhs_type; fun_body} = node.value in
  let thread = token kwd_function ^^ space ^^ print_variable fun_name in
  let thread = thread ^^ print_generics_opt state generics in
  let thread = thread ^^ print_fun_params state parameters in
  let thread = thread ^^ print_type_annotation_opt state rhs_type
  in group (thread ^^ space ^^ print_block state fun_body)

and print_generics_opt state (node : generics option) =
  Option.value_map node ~default:empty ~f:(print_generics state)

and print_generic (node : generic) = print_variable node

and print_generics state (node : generics) =
  print_chevrons state (print_sep_or_term (break 1) print_generic) node

and print_fun_params state (node : fun_params) =
  if Option.is_some node.value.inside then
    let print = print_sep_or_term (break 1) (print_pattern state)
    in print_par state print node
  else
    token node.value.lpar ^^ token node.value.rpar

(* Type annotation *)

and print_type_annotation_opt state (node : type_annotation option) =
  Option.value_map node ~default:empty ~f:(print_type_annotation state)

and print_type_annotation state (node : type_annotation) =
  let colon, type_expr = node in
  let rhs = print_type_expr state type_expr in
  group (token colon ^^ space ^^ rhs)

(* Import declaration *)

and print_D_Import state = function
  ImportAlias i -> print_ImportAlias state i
| ImportAllAs i -> print_ImportAllAs state i
| ImportFrom  i -> print_ImportFrom  state i

and print_ImportAlias state (node : import_alias reg) =
  let {kwd_import; alias; equal; namespace_path} = node.value
  in group (token kwd_import ^^ space ^^ token alias
            ^^ space ^^ token equal ^^ space
            ^^ print_namespace_selection state namespace_path)

and print_namespace_selection state = function
  M_Path  s -> print_M_Path  state s
| M_Alias s -> print_M_Alias s

and print_M_Path state (node : namespace_name namespace_path reg) =
  print_namespace_path state token node.value

and print_namespace_path :
  'a.state -> ('a -> document) -> 'a namespace_path -> document =
  fun state print node ->
    let {namespace_path; selector; property} = node in
    let thread = print_nsepseq (break 0) token namespace_path
    in group (thread ^^ token selector ^^ break 0 ^^ print property)

and print_M_Alias (node : namespace_name) = token node

and print_ImportAllAs state (node : import_all_as reg) =
  let {kwd_import; times; kwd_as; alias; kwd_from; file_path} = node.value
  in group (token kwd_import ^^ space ^^ token times ^^ space
            ^^ token kwd_as ^^ space ^^ token alias ^^ space
            ^^ token kwd_from ^^ space ^^ print_string file_path)

and print_ImportFrom state (node : import_from reg) =
  let {kwd_import; imported; kwd_from; file_path} = node.value in
  let print_vars = print_sep_or_term (break 1) print_variable in
  group (token kwd_import ^^ space ^^
         print_braces ~force_hardline:true state print_vars imported
         ^^ space ^^ token kwd_from ^^ space ^^ print_string file_path)

(* Interfaces *)

and print_D_Interface state (node : interface_decl reg) =
  let {kwd_interface; intf_name; intf_extends = _; intf_body} = node.value in
  group (token kwd_interface ^^ space ^^ token intf_name ^^ space
         ^^ print_intf_body state intf_body)

and print_intf_body state (node : intf_body) =
  print_braces ~force_hardline:true state (print_intf_entries state) node

and print_intf_entries state (node : intf_entries) =
  print_sep_or_term (break 1) (print_intf_entry state) node

and print_intf_entry state = function
  I_Attr  i -> print_I_Attr  state i
| I_Type  i -> print_I_Type  state i
| I_Const i -> print_I_Const state i

and print_I_Attr state (node : attribute * intf_entry) =
  let attributes, entry = unroll_I_Attr node in
  let thread = print_intf_entry state entry
  in print_attributes state thread attributes

and print_I_Type state (node : intf_type reg) =
  let {kwd_type; type_name; type_rhs} = node.value in
  let thread = token kwd_type ^^ space ^^ print_variable type_name
  in group (print_rhs state thread type_rhs)

and print_rhs state thread (node : (equal * type_expr) option) =
  let print state (eq, type_expr) =
    let rhs = print_type_expr state type_expr in
    if is_enclosed_type type_expr
    then thread ^^ space ^^ token eq ^^ space ^^ rhs
    else thread ^^ prefix state#indent 1 (space ^^ token eq) rhs
  in Option.value_map node ~default:thread ~f:(print state)

and print_I_Const state (node : intf_const reg) =
  let {kwd_const; const_name; const_optional; const_type} = node.value in
  let thread = match const_optional with
    | None -> token kwd_const ^^ space ^^ print_variable const_name
    | Some qmark -> token kwd_const ^^ space
                    ^^ print_variable const_name ^^ token qmark
  in group (thread ^^ print_type_annotation state const_type)

(* Namespace declaration *)

and print_D_Namespace state (node : namespace_decl reg) =
  let {kwd_namespace; namespace_name;
       namespace_type; namespace_body} = node.value in
  let thread = token kwd_namespace ^^ space ^^ token namespace_name in
  let thread = thread ^^ space ^^ print_namespace_type state namespace_type
  in group (thread ^^ print_block state namespace_body)

and print_namespace_type state (node : interface option) =
  Option.value_map node ~default:empty ~f:(print_interface state)

and print_interface state (node : interface) =
  let kwd_implements, intf_exprs = node.value in
  let intf_exprs = print_nsepseq (break 1) (print_intf_expr state) intf_exprs in
  token kwd_implements ^^ space ^^ intf_exprs

and print_intf_expr state = function
  I_Body i -> print_I_Body state i
| I_Path i -> print_I_Path state i

and print_I_Body state (node : intf_body) = print_intf_body state node

and print_I_Path state (node : namespace_selection) =
  print_namespace_selection state node

(* Type declarations *)

and print_D_Type state (node : type_decl reg) =
  let {kwd_type; name; generics; eq; type_expr} = node.value in
  let thread = token kwd_type ^^ space ^^ print_variable name in
  let thread = thread ^^ print_generics_opt state generics in
  let rhs = print_type_expr state type_expr in
  group (thread ^^
         if is_enclosed_type type_expr
         then space ^^ token eq ^^ space ^^ rhs
         else prefix state#indent 1 (space ^^ token eq) rhs)

(* Value declarations *)

and print_D_Value state (node : value_decl reg) =
  let {kind; bindings} = node.value in
  let thread   = print_var_kind kind ^^ space
  and bindings = print_nsepseq (break 1) (print_val_binding state) bindings
  in group (thread ^^ bindings)

and print_var_kind = function
  `Let   kwd_let   -> token kwd_let
| `Const kwd_const -> token kwd_const

and print_val_binding state (node : val_binding reg) =
  let {pattern; generics; rhs_type; eq; rhs_expr} = node.value in
  (* In case the RHS is a lambda function, we want to try to display
     it in the same line instead of causing a line break. For example,
     we want to see this:

     let f = (x: int) => x

     And not this:

     let f =
       (x: int) => x
  *)
  let join = function
    E_ArrowFun _ -> ( ^^ )
  | expr when is_enclosed_expr expr -> ( ^^ )
  | _ -> prefix state#indent 0 in
  let lhs = print_pattern state pattern
            ^^ Option.value_map rhs_type ~default:empty
              ~f:(print_type_annotation state) in
  let lhs = lhs ^^ space ^^ token eq ^^ space
  and rhs = print_expr state rhs_expr
  in join rhs_expr lhs rhs

(* Preprocessing directives *)

and print_S_Directive state (node : Directive.t) =
  string (Directive.to_lexeme node).Region.value

(* Export statements *)

and print_S_Export state (node : export_stmt reg) =
  let kwd_export, declaration = node.value in
  token kwd_export ^^ space ^^ print_declaration state declaration

(* Expressions as statements *)

and print_S_Expr state (node : expr) = print_expr state node

(* For-loops *)

and print_S_For state (node : for_stmt reg) =
  let {kwd_for; range; for_body} = node.value in
  let thread = token kwd_for ^^ space ^^ print_range_for state range in
  match for_body with
    None -> thread
  | Some stmt -> prefix state#indent 1 thread (print_statement state stmt)

and print_range_for state (node : range_for par) =
  let {lpar; inside; rpar} = node.value in
  let {initialiser; semi1; condition; semi2; afterthought} = inside in
  let par =
    Option.value_map initialiser ~default:empty ~f:(print_statement state)
    ^^ token semi1
    ^^ Option.value_map condition ~default:empty
                        ~f:(fun expr -> break 1 ^^ print_expr state expr)
    ^^ token semi2
    ^^ Option.value_map afterthought ~default:empty
      ~f:(fun s -> break 1 ^^ print_nsepseq (break 1) (print_expr state) s)
  in print_par_like_document state par lpar rpar

(* For-of loops *)

and print_S_ForOf state (node: for_of_stmt reg) =
  let {kwd_for; range; for_of_body} = node.value in
  let thread = token kwd_for ^^ space ^^ print_range_for_of state range
  in group (thread ^^ space ^^ print_statement state for_of_body)

and print_range_for_of state (node : range_of par) =
  let {lpar; inside; rpar} = node.value in
  let {index_kind; index; kwd_of; expr} = inside in
  let par = print_var_kind index_kind ^^ space ^^ print_variable index
            ^^ space ^^ token kwd_of ^^ space ^^ print_expr state expr
  in print_par_like_document state par lpar rpar

(* Conditional statement *)

and print_S_If state (node : if_stmt reg) =
  let {kwd_if; test; if_so; if_not} = node.value in
  let thread = token kwd_if ^^ space ^^ print_par_expr state test in
  let thread = thread ^^ space ^^ print_statement_semi state if_so
  in group (print_if_not state thread if_not)

and print_if_not state thread = function
  None -> thread
| Some (kwd_else, stmt) ->
    thread ^^ space ^^ token kwd_else ^^ space
    ^^ print_statement state stmt

and print_par_expr state (node : expr par) =
  let {lpar; inside; rpar} = node.value in
  print_par_like_document state (print_expr state inside) lpar rpar

(* Return statement *)

and print_S_Return state (node : return_stmt reg) =
  match node.value with
    kwd_return, None -> token kwd_return
  | kwd_return, Some expr ->
      group (token kwd_return ^^ space ^^ print_expr state expr)

(* Switch statement *)

and print_S_Switch state (node : switch_stmt reg) =
  let {kwd_switch; subject; cases} = node.value in
  let {lbrace; inside; rbrace} = cases.value in
  let braces = print_braces_like_document state ~force_hardline:true
                 (print_cases state inside) lbrace rbrace
  and thread = token kwd_switch ^^ space ^^ print_par_expr state subject
  in group (thread ^^ space ^^ braces)

and print_cases state = function
  AllCases c -> print_AllCases state c
| Default  c -> print_Default  state c

and print_AllCases state (node : all_cases) =
  let switch_cases, default_opt = node in
  let thread = print_switch_cases state switch_cases in
  match default_opt with
    None -> thread
  | Some default -> thread ^^ hardline ^^ print_switch_default state default

and print_switch_cases state (node : switch_case reg Utils.nseq) =
  print_nseq hardline (print_switch_case state) node

and print_switch_case state (node : switch_case reg) =
  let {kwd_case; expr; colon; case_body} = node.value in
  let thread = token kwd_case ^^ space ^^ print_expr state expr in
  let thread = thread ^^ token colon in
  print_label_and_statements state thread case_body

and print_Default state (node : switch_default reg) =
  print_switch_default state node

and print_switch_default state (node : switch_default reg) =
  let {kwd_default; colon; default_body} = node.value in
  let thread = token kwd_default ^^ token colon in
  print_label_and_statements state thread default_body

and print_label_and_statements state label = function
  None -> label
| Some ((stmt,_), []) when is_enclosed_statement stmt ->
    label ^^ space ^^ group (print_statement state stmt)
| Some stmts ->
    hang state#indent (label ^/^ print_statements state stmts)

(* While-loop *)

and print_S_While state (node : while_stmt reg) =
  let {kwd_while; invariant; while_body} = node.value in
  let thread = token kwd_while ^^ space ^^ print_par_expr state invariant
  in group (thread ^^ space ^^ print_statement state while_body)

(* EXPRESSIONS *)

and print_expr state = function
  E_Add        e -> print_E_Add        state e
| E_AddEq      e -> print_E_AddEq      state e
| E_And        e -> print_E_And        state e
| E_App        e -> print_E_App        state e
| E_Array      e -> print_E_Array      state e
| E_ArrowFun   e -> print_E_ArrowFun   state e
| E_Assign     e -> print_E_Assign     state e
| E_Attr       e -> print_E_Attr       state e
| E_BitAnd     e -> print_E_BitAnd     state e
| E_BitAndEq   e -> print_E_BitAndEq   state e
| E_BitNeg     e -> print_E_BitNeg     state e
| E_BitOr      e -> print_E_BitOr      state e
| E_BitOrEq    e -> print_E_BitOrEq    state e
| E_BitSl      e -> print_E_BitSl      state e
| E_BitSlEq    e -> print_E_BitSlEq    state e
| E_BitSr      e -> print_E_BitSr      state e
| E_BitSrEq    e -> print_E_BitSrEq    state e
| E_BitXor     e -> print_E_BitXor     state e
| E_BitXorEq   e -> print_E_BitXorEq   state e
| E_Bytes      e -> print_E_Bytes            e
| E_CodeInj    e -> print_E_CodeInj    state e
| E_ContractOf e -> print_E_ContractOf state e
| E_CtorApp    e -> print_E_CtorApp    state e
| E_Div        e -> print_E_Div        state e
| E_DivEq      e -> print_E_DivEq      state e
| E_Do         e -> print_E_Do         state e
| E_Equal      e -> print_E_Equal      state e
| E_False      e -> print_E_False            e
| E_Function   e -> print_E_Function   state e
| E_Geq        e -> print_E_Geq        state e
| E_Gt         e -> print_E_Gt         state e
| E_Int        e -> print_E_Int              e
| E_Leq        e -> print_E_Leq        state e
| E_Lt         e -> print_E_Lt         state e
| E_Match      e -> print_E_Match      state e
| E_Mult       e -> print_E_Mult       state e
| E_MultEq     e -> print_E_MultEq     state e
| E_Mutez      e -> print_E_Mutez            e
| E_NamePath   e -> print_E_NamePath   state e
| E_Nat        e -> print_E_Nat              e
| E_Neg        e -> print_E_Neg        state e
| E_Neq        e -> print_E_Neq        state e
| E_Not        e -> print_E_Not        state e
| E_Object     e -> print_E_Object     state e
| E_Or         e -> print_E_Or         state e
| E_Par        e -> print_E_Par        state e
| E_PostDecr   e -> print_E_PostDecr   state e
| E_PostIncr   e -> print_E_PostIncr   state e
| E_PreDecr    e -> print_E_PreDecr    state e
| E_PreIncr    e -> print_E_PreIncr    state e
| E_Proj       e -> print_E_Proj       state e
| E_Rem        e -> print_E_Rem        state e
| E_RemEq      e -> print_E_RemEq      state e
| E_String     e -> print_E_String           e
| E_Sub        e -> print_E_Sub        state e
| E_SubEq      e -> print_E_SubEq      state e
| E_Ternary    e -> print_E_Ternary    state e
| E_True       e -> print_E_True             e
| E_Typed      e -> print_E_Typed      state e
| E_Update     e -> print_E_Update     state e
| E_Var        e -> print_E_Var              e
| E_Verbatim   e -> print_E_Verbatim         e
| E_Xor        e -> print_E_Xor        state e

(* Addition *)

and print_E_Add state (node : plus bin_op reg) = print_bin_op state node

and print_bin_op state (node : lexeme wrap bin_op reg) =
  let {op; arg1; arg2} = node.value in
  let length = String.length op#payload + 1
  in group (print_expr state arg1 ^/^ token op ^^ space
            ^^ nest length (print_expr state arg2))

(* Add and assign *)

and print_E_AddEq state (node : plus_eq bin_op reg) = print_bin_op state node

(* Logical conjunction *)

and print_E_And state (node : bool_and bin_op reg) = print_bin_op state node

(* Function application *)

and print_E_App state (node : (expr * arguments) reg) =
  let lambda,
      arguments = node.value in
  let lambda    = print_expr state lambda in
  let print     = print_nsepseq (break 1) (print_expr state) in
  let print     = Option.value_map ~default:empty ~f:print in
  let arguments = print_par state print arguments
  in lambda ^^ arguments

(* Array expression *)

and print_E_Array state (node : expr _array) =
  print_array state (print_element print_expr) node

and print_array :
  'a.state -> (state -> 'a element -> document) -> 'a _array -> document =
  fun state print ->
    print_brackets state (print_sep_or_term (break 1) (print state))

and print_element :
  'a.(state -> 'a -> document) -> state -> 'a element -> document =
  fun print state (ellipsis, item) ->
    let item = print state item in
    match ellipsis with
      None -> item
    | Some ellipsis -> token ellipsis ^^ item

(* Arrow function *)

and print_arrow_fun_params state = function
  ParParams  node -> print_fun_params state node
| NakedParam node -> print_pattern state node

and print_E_ArrowFun state (node : arrow_fun_expr reg) =
  let {generics; parameters; rhs_type; arrow; fun_body} = node.value in
  let thread = print_generics_opt state generics in
  let thread = thread ^^ print_arrow_fun_params state parameters in
  let thread = thread ^^ print_type_annotation_opt state rhs_type in
  let lhs    = thread ^^ space ^^ token arrow ^^ space
  in print_fun_body state lhs fun_body

and print_fun_body state lhs = function
  StmtBody s ->
    (* If the function has only one statement we may try to display
       it inline rather than in a new one. *)
    let force_hardline = not @@ List.is_empty @@ snd s.value.inside in
    lhs ^^ print_braces state ~force_hardline (print_statements state) s
| ExprBody e -> prefix state#indent 0 lhs (print_expr state e)

(* Assignment *)

and print_E_Assign state (node : equal bin_op reg) = print_bin_op state node

(* Attributes expressions *)

and print_E_Attr state (node : attribute * expr) =
  let attributes, expr = unroll_E_Attr node in
  let thread = print_expr state expr
  in print_attributes state thread attributes

(* Bitwise conjunction *)

and print_E_BitAnd state (node : bit_and bin_op reg) = print_bin_op state node

(* Bitwise conjuction & Assignment *)

and print_E_BitAndEq state (node : bit_and_eq bin_op reg) =
  print_bin_op state node

(* Bitwise negation *)

and print_E_BitNeg state (node : bit_neg un_op reg) = print_un_op state node

(* Bitwise disjunction *)

and print_E_BitOr state (node : bit_or bin_op reg) = print_bin_op state node

(* Bitwise disjunction & Assignment *)

and print_E_BitOrEq state (node : bit_or_eq bin_op reg) =
  print_bin_op state node

(* Bitwise left shift *)

and print_E_BitSl state (node : bit_sl bin_op reg) = print_bin_op state node

(* Bitwise left shift & Assignment *)

and print_E_BitSlEq state (node : bit_sl_eq bin_op reg) =
  print_bin_op state node

(* Bitwise right shift *)

and print_E_BitSr state (node : bit_sr bin_op reg) = print_bin_op state node

(* Bitwise right shift & Assignmen *)

and print_E_BitSrEq state (node : bit_sr_eq bin_op reg) =
  print_bin_op state node

(* Bitwise exclusive disjunction *)

and print_E_BitXor state (node : bit_xor bin_op reg) = print_bin_op state node

(* Bitwise exclusive disjunction & Assignment *)

and print_E_BitXorEq state (node : bit_xor_eq bin_op reg) =
  print_bin_op state node

(* Bytes expressions *)

and print_E_Bytes (node : (lexeme * Hex.t) wrap) = print_bytes node

(* Code injection *)

and print_E_CodeInj state (node : code_inj reg) =
  let {language; code} = node.value in
  let language = token language in
  let code     = print_expr state code
  in group (language ^/^ code)

(* Contract-of expression *)

and print_E_ContractOf state (node : contract_of_expr reg) =
  let {kwd_contract_of; namespace_path} = node.value in
  token kwd_contract_of ^^
  print_par state (print_namespace_selection state) namespace_path

(* Constructor application *)

and print_E_CtorApp state (node : expr variant_kind) =
  print_variant_kind print_expr state node

(* Euclidean division *)

and print_E_Div state (node : slash bin_op reg) = print_bin_op state node

(* Euclidean division & Assignment *)

and print_E_DivEq state (node : div_eq bin_op reg) = print_bin_op state node

(* Do-expression *)

and print_E_Do state (node : do_expr reg) =
  let {kwd_do; statements} = node.value in
  token kwd_do ^^ space ^^
  print_braces state (print_statements state) statements

(* Equality *)

and print_E_Equal state (node : equal bin_op reg) = print_bin_op state node

(* Logical falsity *)

and print_false (node : kwd_false) = token node

and print_E_False (node : kwd_false) = print_false node

(* Function expression *)

and print_E_Function state (node : function_expr reg) =
  let {kwd_function; generics; parameters; rhs_type; fun_body} = node.value in
  let kwd_function = token kwd_function in
  let generics     = print_generics_opt state generics in
  let parameters   = print_arrow_fun_params state parameters in
  let rhs_type     = print_type_annotation_opt state rhs_type in
  let lhs = generics ^^ parameters ^^ rhs_type in
  let thread =
    kwd_function ^/^
    match fun_body with
      StmtBody s ->
        (* If the function has only one statement we may try to display
           it inline rather than in a new one.  *)
        let force_hardline = not @@ List.is_empty @@ snd s.value.inside in
        lhs ^^ space
        ^^ print_braces state ~force_hardline (print_statements state) s
    | ExprBody e -> prefix state#indent 1 lhs (print_expr state e)
    in group thread

(* Greater of equal *)

and print_E_Geq state (node : geq bin_op reg) = print_bin_op state node

(* Greater than *)

and print_E_Gt state (node : gt bin_op reg) = print_bin_op state node

(* Integers *)

and print_E_Int (node : (lexeme * Z.t) wrap) = print_int node

(* Lower or equal than *)

and print_E_Leq state (node : leq bin_op reg) = print_bin_op state node

(* Lower than *)

and print_E_Lt state (node : lt bin_op reg) = print_bin_op state node

(* Pattern matching *)

and print_E_Match state (node : match_expr reg) =
  let {kwd_match; subject; clauses} = node.value in
  group (token kwd_match
         ^^ print_par state (print_expr state) subject ^^ space
         ^^ print_braces ~force_hardline:true state (print_clauses state)
                         clauses)

and print_clauses state = function
  AllClauses    c -> print_AllClauses    state c
| DefaultClause c -> print_DefaultClause state c

and print_AllClauses state (node : all_match_clauses) =
  let match_clauses, default_opt = node in
  let thread = print_match_clauses state match_clauses in
  match default_opt with
    None -> thread
  | Some default -> thread ^^ hardline ^^ print_DefaultClause state default

and print_match_clauses state (node : match_clause reg Utils.nseq) =
  print_nseq hardline (print_match_clause state) node

and print_match_clause state (node : match_clause reg) =
  let {kwd_when; filter; colon; clause_expr} = node.value in
  let thread = token kwd_when ^^ space ^^ print_par state (print_pattern state) filter in
  let thread = thread ^^ token colon in
  print_label_and_expr state thread clause_expr

and print_DefaultClause state (node : match_default reg) =
  print_match_default state node

and print_match_default state (node : match_default reg) =
  let {kwd_default; colon; default_expr} = node.value in
  let thread = token kwd_default ^^ token colon in
  print_label_and_expr state thread default_expr

and print_label_and_expr state label expr =
  hang state#indent (label ^/^ print_expr state expr)

(* Multiplication *)

and print_E_Mult state (node : times bin_op reg) = print_bin_op state node

(* Multiplication && Assignment *)

and print_E_MultEq state (node : times_eq bin_op reg) =
  print_bin_op state node

(* Mutez as an expression *)

and print_E_Mutez (node : (lexeme * Int64.t) wrap) = print_mutez node

(* Selection through nested namespaces *)

and print_E_NamePath state (node : expr namespace_path reg) =
  print_namespace_path state (print_expr state) node.value

(* Natural numbers in expressions *)

and print_E_Nat (node :  (lexeme * Z.t) wrap) = print_nat node

(* Arithmetic negation *)

and print_E_Neg state (node : minus un_op reg) =
  let {op; arg} = node.value in
  token op ^^ print_expr state arg

and print_un_op state (node : lexeme wrap un_op reg) =
  let {op; arg} = node.value in
  token op ^^ space ^^ print_expr state arg

(* Arithmetic difference *)

and print_E_Neq state (node : neq bin_op reg) = print_bin_op state node

(* Logical negation *)

and print_E_Not state (node : bool_neg un_op reg) = print_un_op state node

(* Objects *)

and print_E_Object state (node : expr _object) =
  print_object state print_expr node

and print_object :
  'a.state -> (state -> 'a -> document) -> 'a _object -> document =
  fun state print node ->
    let print = print_sep_or_term (break 1) (print_property state print)
    in print_braces state print node

and print_property :
  'a.state -> (state -> 'a -> document) -> 'a property reg -> document =
  fun state print node ->
  let {attributes; property_id; property_rhs} = node.value in
  let attributes = print_attributes state empty attributes in
  let property_id = print_property_id state property_id in
  let property_rhs =
    match property_rhs with
      None -> empty
    | Some (colon, value) -> token colon ^^ space ^^ print state value
  in attributes ^/^ property_id ^^ property_rhs

and print_property_id state = function
  F_Int  i -> print_int i
| F_Name n -> print_variable n
| F_Str  s -> print_string s

(* Logical disjunction *)

and print_E_Or state (node : bool_or bin_op reg) = print_bin_op state node

(* Parenthesised expression *)

and print_E_Par state (node : expr par) =
  print_par state (print_expr state) node

(* Post-decrementation *)

and print_E_PostDecr state (node : decrement un_op reg) =
  let {op; arg} = node.value in
  print_expr state arg ^^ token op

(* Post-incrementation *)

and print_E_PostIncr state (node : increment un_op reg) =
  let {op; arg} = node.value in
  print_expr state arg ^^ token op

(* Pre-decrementation *)

and print_E_PreDecr state (node : decrement un_op reg) =
  let {op; arg} = node.value in
  token op ^^ print_expr state arg

(* Pre-incrementation *)

and print_E_PreIncr state (node : increment un_op reg) =
  let {op; arg} = node.value in
  token op ^^ print_expr state arg

(* Projections *)

and print_selection state = function
  PropertyName (dot, n) -> token dot ^^ print_variable n
| PropertyStr        s  -> print_brackets state print_string s
| Component          i  -> print_brackets state print_int i

and print_E_Proj state (node : projection reg) =
  let {object_or_array; property_path} = node.value in
  let thread = print_expr state object_or_array in
  let path   = print_nseq (break 0) (print_selection state) property_path
  in group (thread ^^ path)

(* Arithmetic remainder *)

and print_E_Rem state (node : remainder bin_op reg) = print_bin_op state node

(* Arithmetic remainder & Assignment *)

and print_E_RemEq state (node : rem_eq bin_op reg) = print_bin_op state node

(* String expression *)

and print_E_String (node : lexeme wrap) = print_string node

(* Arithmetic subtraction *)

and print_E_Sub state (node : minus bin_op reg) = print_bin_op state node

(* Subtraction & Assignment *)

and print_E_SubEq state (node : minus_eq bin_op reg) = print_bin_op state node

(* Ternary conditional *)

and print_E_Ternary state (node : ternary reg) =
  let {condition; qmark; truthy; colon; falsy} = node.value in
  print_expr state condition ^^
  space ^^ token qmark ^^ space ^^
  nest state#indent (print_expr state truthy) ^^
  space ^^ token colon ^^ space ^^
  nest state#indent (print_expr state falsy)

(* Logical truth *)

and print_true (node : kwd_true) = token node

and print_E_True (node : kwd_true) = print_true node

(* Typed expressions *)

and print_infix state lhs middle rhs =
  group (lhs ^^ space ^^ prefix state#indent 1 middle rhs)

and print_E_Typed state (node : typed_expr reg) =
  let expr, kwd_as, type_expr = node.value in
  let lhs = print_expr state expr in
  let rhs = print_type_expr state type_expr
  in print_infix state lhs (token kwd_as) rhs

(* Object functional updates *)

and print_updates state
    (node : (expr property reg, property_sep) Utils.sep_or_term) =
  print_sep_or_term (break 1) (print_property state print_expr) node

and print_update state (node : update_expr) =
  let {ellipsis; _object; sep; updates} = node in
  group (token ellipsis ^^ print_expr state _object
         ^^ token sep ^/^ print_updates state updates)

and print_E_Update state (node : update_expr braces) =
  print_braces state (print_update state) node

(* Expression variable *)

and print_E_Var (node : variable) = print_variable node

(* Verbatim string expressions *)

and print_E_Verbatim (node : lexeme wrap) = print_verbatim node

(* Logical exclusive disjunction *)

and print_E_Xor state (node : bool_xor bin_op reg) = print_bin_op state node

(* PATTERNS *)

and print_pattern state = function
  P_Array    p -> print_P_Array    state p
| P_Attr     p -> print_P_Attr     state p
| P_Bytes    p -> print_P_Bytes          p
| P_CtorApp  p -> print_P_CtorApp  state p
| P_False    p -> print_P_False          p
| P_Int      p -> print_P_Int            p
| P_Mutez    p -> print_P_Mutez          p
| P_NamePath p -> print_P_NamePath state p
| P_Nat      p -> print_P_Nat            p
| P_Object   p -> print_P_Object   state p
| P_String   p -> print_P_String         p
| P_True     p -> print_P_True           p
| P_Typed    p -> print_P_Typed    state p
| P_Var      p -> print_P_Var            p
| P_Verbatim p -> print_P_Verbatim       p

(* Array patterns *)

and print_P_Array state (node : pattern _array) =
  print_array state print_P_element node

and print_P_element state (node : pattern element) =
  print_element print_pattern state node

(* Attributed pattern *)

and print_P_Attr state (node : attribute * pattern) =
  let attributes, pattern = unroll_P_Attr node in
  let thread = print_pattern state pattern
  in print_attributes state thread attributes

(* Bytes pattern *)

and print_P_Bytes (node : (lexeme * Hex.t) wrap) = print_bytes node

(* Pattern for constructor application *)

and print_P_CtorApp state (node : pattern variant_kind) =
  print_variant_kind print_pattern state node

(* Logical false pattern *)

and print_P_False (node : kwd_false) = print_false node

(* Integer in a pattern *)

and print_P_Int (node : (lexeme * Z.t) wrap) = print_int node

(* Mutez in patterns *)

and print_P_Mutez (node : (lexeme * Int64.t) wrap) = print_mutez node

(* Selected pattern *)

and print_P_NamePath state (node : pattern namespace_path reg) =
  print_namespace_path state (print_pattern state) node.value

(* Natural numbers in patterns *)

and print_P_Nat (node : (lexeme * Z.t) wrap) = print_nat node

(* Object patterns *)

and print_P_Object state (node : pattern _object) =
  print_object state print_pattern node

(* String patterns *)

and print_P_String (node : lexeme wrap) = print_string node

(* Logical truth pattern *)

and print_P_True (node : kwd_true) = print_true node

(* Typed patterns *)

and print_P_Typed state (node : typed_pattern reg) =
  let pattern, type_annot = node.value in
  print_pattern state pattern ^^
  print_type_annotation state type_annot

(* Variable pattern *)

and print_P_Var (node : variable) = print_variable node

(* Verbatim string patterns *)

and print_P_Verbatim (node : lexeme wrap) = print_verbatim node

(* TYPE EXPRESSIONS *)

and print_type_expr state = function
  T_App         t -> print_T_App         state t
| T_Attr        t -> print_T_Attr        state t
| T_Array       t -> print_T_Array       state t
| T_Fun         t -> print_T_Fun         state t
| T_Int         t -> print_T_Int               t
| T_NamePath    t -> print_T_NamePath    state t
| T_Object      t -> print_T_Object      state t
| T_Par         t -> print_T_Par         state t
| T_ParameterOf t -> print_T_ParameterOf state t
| T_String      t -> print_T_String            t
| T_Union       t -> print_T_Union       state t
| T_Var         t -> print_T_Var               t
| T_Variant     t -> print_T_Variant     state t

(* Type constructor application *)

and print_T_App state (node : (type_expr * type_ctor_args) reg) =
  let ctor, tuple = node.value in
  print_type_expr state ctor ^^ print_type_tuple state tuple

and print_type_tuple state (node : type_ctor_args) =
  let print = print_nsep_or_term (break 1) (print_type_expr state)
  in print_chevrons state print node

(* Attributed type *)

and print_T_Attr state (node : attribute * type_expr) =
  let attributes, t_expr = unroll_T_Attr node in
  let thread = print_type_expr state t_expr
  in print_attributes state thread attributes

(* Array type *)

and print_T_Array state (node : array_type) =
  let seq = print_nsep_or_term (break 1) (print_type_expr state)
  in group (print_brackets state seq node)

(* Function type *)

and print_fun_type_param state (node : fun_type_param reg) =
  let pattern, type_annotation = node.value in
  group (print_pattern state pattern
         ^^ print_type_annotation state type_annotation)

and print_fun_type_params state (node : fun_type_params) =
  let print = print_sep_or_term (break 1) (print_fun_type_param state)
  in print_par state print node

and print_fun_type state (node : fun_type) =
  let lhs, arrow, rhs = node.value in
  let lhs = print_fun_type_params state lhs in
  let rhs = print_type_expr state rhs in
  group (lhs ^^ space ^^ token arrow ^^ space ^^ rhs)

and print_T_Fun state (node : fun_type) = print_fun_type state node

(* Integer type *)

and print_T_Int (node : (lexeme * Z.t) wrap) = print_int node

(* Selection of a type *)

and print_T_NamePath state (node : type_expr namespace_path reg) =
  print_namespace_path state (print_type_expr state) node.value

(* Object type *)

and print_object_type state (node : type_expr _object) =
  print_object state print_type_expr node

and print_T_Object state (node : type_expr _object) =
  print_object_type state node

(* Parenthesised type expressions *)

and print_T_Par state (node : type_expr par) =
  print_par state (print_type_expr state) node

(* Parameter-of type *)

and print_T_ParameterOf state (node : parameter_of_type reg) =
  let {kwd_parameter_of; namespace_path} = node.value in
  token kwd_parameter_of ^^ space
  ^^ print_namespace_selection state namespace_path

(* String type *)

and print_T_String (node : lexeme wrap) = print_string node

(* Union type *)

and print_union_type state (node : union_type) =
  print_variant_or_union_type state print_object_type node

and print_variant_or_union_type :
  'a.state -> (state -> 'a -> document) ->
  ('a, vbar) Utils.nsep_or_pref reg -> document =
  fun state print node ->
  (* We provide an extra layer of indentation so that instead of this:

     type t =
       | [
         "Alt",
         unit
         ]

     We will get:

     type t =
       | [
           "Alt",
           unit
         ]
   *)
  let variants =
    let open Simple_utils.Function in
    Utils.nsep_or_pref_map (nest state#indent <@ print state) node.value
  in
  let bar, head, tail =
    match variants with
      `Sep variants ->
        let head, tail = variants in
        bar, head, tail
    | `Pref variants ->
        let (vbar, head), tail = variants in
        token vbar, head, tail
  in
  let padding_flat =
    let open PrettyComb in
    match state#leading_vbar with
      Avoid | Only_on_new_line -> empty
    | Always -> bar ^^ space
  in
  let padding_non_flat =
    let open PrettyComb in
    match state#leading_vbar with
      Avoid -> blank state#indent
    | Always | Only_on_new_line -> bar ^^ space
  in
  let head =
    if List.is_empty tail
    then bar ^^ space ^^ head
    else ifflat padding_flat padding_non_flat ^^ head
  in
  let app (bar, variant) = break 1 ^^ token bar ^^ space ^^ variant in
  group (head ^^ concat_map app tail)

and print_T_Union state (node : union_type) = print_union_type state node

(* Type variables *)

and print_T_Var (node : variable) = print_variable node

(* Variant type *)

and print_variant_kind : 'a. (state -> 'a -> document) -> state -> 'a variant_kind -> document =
 fun printer state node ->
  match node with
    Variant node -> print_variant printer state node
  | Bracketed node -> print_bracketed_variant printer state node
  | Legacy  node -> print_legacy_variant printer state node

and print_variant : 'a. (state -> 'a -> document) -> state -> 'a variant reg -> document =
 fun printer state node ->
  let ({tuple; attributes} : 'a variant) = node.value in
  let sharp, app = tuple in
  let tuple = Option.value_map ~default:empty ~f:token sharp
              ^^ print_app state printer app
  in group (print_attributes state tuple attributes)

and print_bracketed_variant : 'a. (state -> 'a -> document) -> state -> 'a bracketed_variant reg -> document =
 fun printer state node ->
  let {attributes; sharp; tuple} = node.value in
  let attributes = print_attributes state empty attributes in
  let sharp = token sharp in
  let tuple = print_brackets state (print_bracketed_variant_args printer state) tuple
  in group (attributes ^^ sharp ^^ tuple)

and print_bracketed_variant_args : 'a. (state -> 'a -> document) -> state -> 'a bracketed_variant_args -> document =
 fun printer state node ->
  let {ctor; args} : 'a bracketed_variant_args = node in
  let args =
    match args with
      None -> empty
    | Some (comma, args) -> token comma ^/^ print_sep_or_term (break 1) (printer state) args
  in printer state ctor ^^ args

and print_legacy_variant : 'a. (state -> 'a -> document) -> state -> 'a legacy_variant reg -> document =
 fun printer state node ->
  let {attributes; tuple} = node.value in
  let tuple = print_legacy_variant_args printer state tuple in
  print_attributes state tuple attributes

and print_legacy_variant_args : 'a. (state -> 'a -> document) -> state -> 'a legacy_variant_args brackets -> document =
 fun printer state ->
  print_brackets state (fun inside ->
    let {ctor; args} = inside in
    let ctor = print_string ctor in
    let rec separate_map = function
      []            -> empty
    | (sep', x)::xs -> token ~sep:(break 1) sep' ^^ printer state x ^^ separate_map xs
    in ctor ^^ separate_map args)

and print_app :
  'a.state -> (state -> 'a -> document) -> 'a app -> document =
  fun state print -> function
    ZeroArg ctor -> print_ctor_app_kind ctor ^^ string "()"
  | MultArg (ctor, args) ->
      print_ctor_app_kind ctor
      ^^ print_par state (print_nsep_or_term (break 1) (print state)) args

and print_ctor_app_kind = function
  CtorStr  node -> print_string node
| CtorName node -> print_ctor node

and print_variant_type state (node : variant_type) =
  print_variant_or_union_type state (print_variant_kind print_type_expr) node

and print_T_Variant state (node : variant_type) = print_variant_type state node

let print_type_expr = print_type_expr
let print_pattern   = print_pattern
let print_expr      = print_expr
let print_statement = print_statement
let print_signature_expr = print_intf_expr

type cst       = CST.t
type expr      = CST.expr
type type_expr = CST.type_expr
type pattern   = CST.pattern
type statement = CST.statement
type signature_expr = CST.intf_expr