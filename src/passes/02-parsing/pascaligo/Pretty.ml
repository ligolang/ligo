(* A pretty printer for PascaLIGO *)

module List = Core.List

(* Vendor dependencies *)

module Utils = Simple_utils.Utils
module Region = Simple_utils.Region
open! Region

(* Local dependencies *)

module CST = Cst_pascaligo.CST
open CST

(* Third party dependencies *)

open! PPrint

(* UTILITIES *)

let unroll_D_Attr (attr, decl) =
  let rec aux attrs = function
    D_Attr (attr, decl) -> aux (attr :: attrs) decl
  | decl                -> List.rev attrs, decl
  in aux [attr] decl

let unroll_T_Attr (attr, type_expr) =
  let rec aux attrs = function
    T_Attr (attr, type_expr) -> aux (attr :: attrs) type_expr
  | type_expr                -> List.rev attrs, type_expr
  in aux [attr] type_expr

let unroll_S_Attr (attr, statement) =
  let rec aux attrs = function
    S_Attr (attr, statement) -> aux (attr :: attrs) statement
  | statement                -> List.rev attrs, statement
  in aux [attr] statement

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
  string ("0x" ^ Hex.show (snd node#payload))

let print_mutez (node : (lexeme * Int64.t) wrap) =
  Int64.to_string (snd node#payload) ^ "mutez" |> string

let print_ident (node : variable) = string node#payload

let print_string (node : lexeme wrap) =
  string "\"" ^^ print_ident node ^^ string "\""

let print_verbatim (node : lexeme wrap) =
  string "{|" ^^ print_ident node ^^ string "|}"

let print_int (node : (lexeme * Z.t) wrap) =
  string (Z.to_string (snd node#payload))

and print_nat (node : (lexeme * Z.t) wrap) =
  string (Z.to_string (snd node#payload) ^ "n")


(* HIGHER-ORDER PRINTERS *)

let print_par : ('a -> document) -> 'a par reg -> document =
  fun print {value; _} ->
    string "(" ^^ nest 1 (print value.inside ^^ string ")")

let print_brackets : ('a -> document) -> 'a brackets reg -> document =
  fun print {value; _} ->
    string "[" ^^ nest 1 (print value.inside ^^ string "]")

let print_chevrons : ('a -> document) -> 'a chevrons reg -> document =
  fun print {value; _} ->
    string "<" ^^ nest 1 (print value.inside ^^ string ">")

(* PRINTING THE CST *)

let rec print cst = print_declarations cst.decl

(* DECLARATIONS (top-level) *)

and print_declarations (node : declarations) =
  let declarations = Utils.nseq_to_list node in
  List.filter_map ~f:print_declaration declarations
  |> separate_map (hardline ^^ hardline) group

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_declaration = function
  D_Attr      d -> print_D_Attr d
| D_Const     d -> Some (print_D_Const d)
| D_Directive _ -> None
| D_Fun       d -> Some (print_D_Fun d)
| D_Module    d -> Some (print_D_Module d)
| D_Type      d -> Some (print_D_Type d)

(* Attributed declaration *)

and print_D_Attr (node : attribute * declaration) =
  let attributes, declaration = unroll_D_Attr node in
  match print_declaration declaration with
    Some thread -> Some (print_attributes thread attributes)
  | None        -> None

and print_attribute (node : Attr.t reg) =
  let key, val_opt = node.value in
  let thread = string "[@" ^^ string key in
  let thread = match val_opt with
                 Some String value ->
                   group (thread ^/^ nest 2 (string value))
               | None -> thread in
  let thread = thread ^^ string "]"
  in thread

and print_attributes thread = function
     [] -> thread
| attrs -> group (separate_map (break 0) print_attribute attrs ^/^ thread)

(* Constant declaration *)

and print_D_Const (node : const_decl reg) =
  let node        = node.value in
  let pattern     = print_pattern node.pattern
  and type_params = node.type_params
  and const_type  = node.const_type
  and init        = node.init
  in
  let thread = string "const " ^^ pattern in
  let thread = print_type_params thread type_params in
  let thread = print_opt_type thread const_type in
  let thread = print_init "= " thread init
  in group thread

and print_type_params thread (node : type_params chevrons reg option) =
  match node with
    None -> thread
  | Some type_params ->
      let params = print_chevrons (print_nsepseq print_variable) type_params
      in group (thread ^//^ params)

and print_init op thread (node : expr) =
  thread ^^ group (break 1 ^^ nest 2 (string op ^^ print_expr node))

and print_opt_type thread (node : type_annotation option) =
  match node with
    None -> thread
  | Some a -> print_type_annotation thread a

and print_type_annotation thread (_, type_expr : type_annotation) =
  group (thread ^/^ nest 2 (string ": " ^^ print_type_expr type_expr))

(*
and print_dir_decl = function
  Directive.PP_Linemarker {value; _} ->
    let open Directive in
    let linenum, file_path, flag_opt = value in
    let flag =
      match flag_opt with
        Some Push -> " 1"
      | Some Pop  -> " 2"
      | None      -> "" in
    let lexeme = Printf.sprintf "# %d %S%s" linenum file_path flag
    in string lexeme
*)

(* Function declaration *)

and print_D_Fun (node : fun_decl reg) =
  let node = node.value in

  let print_return (node : expr) =
    let expr = print_expr node in
    match node with
      E_Block _ -> break 1 ^^ expr
    | _         -> nest 2 (break 1 ^^ expr) in

  let print_ret_type (node : type_annotation option) =
    match node with
      None -> string " is"
    | Some (_, e) ->
        let e = nest 2 (print_type_expr e)
        in nest 2 (break 1 ^^ string ": " ^^ e ^^ string " is") in

  let print_kwd_recursive (node : kwd_recursive option) =
    if node = None then string "function"
    else string "recursive" ^/^ string "function" in

  let kwd_recursive = print_kwd_recursive node.kwd_recursive
  and fun_name      = print_ident node.fun_name
  and type_params   = node.type_params
  and param         = node.parameters
  and ret_type      = print_ret_type node.ret_type
  and return        = print_return node.return
  in
  let thread = group kwd_recursive in
  let thread = group (thread ^/^ nest 2 fun_name) in
  let thread = print_type_params thread type_params in
  let thread = group (thread ^//^ print_par print_parameters param) in
  let thread = thread ^^ group ret_type in
  let thread = thread ^^ group return
  in thread

and print_parameters p = print_sepseq print_param_decl p

and print_nsepseq :
  'a.('a -> document) -> ('a, lexeme wrap) Utils.nsepseq -> document =
  fun print ->
    function
      head, [] -> print head
    | _, (sep, _)::_ as elements ->
        let elems = Utils.nsepseq_to_list elements
        and sep   = string sep#payload ^^ break 1
        in separate_map sep print elems

and print_sepseq :
  'a.('a -> document) -> ('a, lexeme wrap) Utils.sepseq -> document =
  fun print ->
    function None -> empty | Some seq -> print_nsepseq print seq

and print_param_decl (node : param_decl reg) =
  let {param_kind; pattern; param_type} = node.value in
  match param_kind with
    `Var   _ -> print_param "var"   pattern param_type
  | `Const _ -> print_param "const" pattern param_type

and print_param thread pattern param_type =
  let thread = string (thread ^ " ") ^^ print_pattern pattern in
  match param_type with
    None -> thread
  | Some (_, e) -> (thread ^^ string " :") ^//^ print_type_expr e

and print_variable (node : variable) = print_ident node

(* Module declaration (structure) *)

and print_D_Module (node : module_decl reg) =
  let node        = node.value in
  let name        = print_ident node.name
  and module_expr = print_module_expr node.module_expr
  in group (string "module " ^^ name ^^ string " is "
            ^^ module_expr)

and print_module_expr (node : module_expr) =
  match node with
    M_Body e -> print_M_Body e
  | M_Path e -> print_M_Path e
  | M_Var  e -> print_M_Var  e

and print_M_Body (node : module_body reg) =
  let node = node.value in
  let decls = print_declarations node.declarations in
  group (string "{"
         ^^ nest 2 (break 0 ^^ decls) ^^ hardline ^^ string "}")

and print_M_Path (node : module_name module_path reg) =
  print_module_path print_ident node

and print_M_Var (node : module_name) = print_ident node

(* Type declaration *)

and print_D_Type (node : type_decl reg) =
  let node      = node.value in
  let name      = print_ident node.name
  and params    = print_type_vars node.params
  and type_expr = print_type_expr node.type_expr
  in
  let type_expr = nest 2 (break 1 ^^ type_expr)
  in group (string "type " ^^ name ^^ params ^^ string " is" ^^ type_expr)

and print_type_vars (node : variable tuple option) =
  match node with
    None -> empty
  | Some tuple -> string " " ^^ print_tuple print_ident tuple

(* TYPE EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_type_expr = function
  T_App     t -> print_T_App     t
| T_Attr    t -> print_T_Attr    t
| T_Cart    t -> print_T_Cart    t
| T_Fun     t -> print_T_Fun     t
| T_Int     t -> print_T_Int     t
| T_ModPath t -> print_T_ModPath t
| T_Par     t -> print_T_Par     t
| T_Record  t -> print_T_Record  t
| T_String  t -> print_T_String  t
| T_Sum     t -> print_T_Sum     t
| T_Var     t -> print_T_Var     t

(* Application of type constructor *)

and print_T_App (node : (type_expr * type_tuple) reg) =
  let ctor_expr, tuple = node.value in
  print_type_expr ctor_expr ^//^ print_type_tuple tuple

and print_type_tuple (node : type_tuple) =
  let node       = node.value in
  let head, tail = node.inside in
  let head       = print_type_expr head in
  let rec app = function
    []       -> empty
  | [e]      -> group (break 1 ^^ print_type_expr e)
  | e::items -> group (break 1 ^^ print_type_expr e ^^ string ",")
               ^^ app items in
  let components =
    if List.is_empty tail then head
    else head ^^ string "," ^^ app (List.map ~f:snd tail)
  in string "(" ^^ nest 1 (components ^^ string ")")

(* Attributed type *)

and print_T_Attr (node : attribute * type_expr) =
  let attributes, type_expr = unroll_T_Attr node in
  let thread =
    match type_expr with
      T_Sum t ->
        print_sum_type ~attr:(not (List.is_empty attributes)) t
    | _ -> print_type_expr type_expr in
  let thread = print_attributes thread attributes
  in thread

(* Cartesian type *)

and print_T_Cart (node : cartesian) =
  let head, _, tail = node.value in
  let head          = print_type_expr head in
  let rec app = function
    []       -> empty
  | [e]      -> group (break 1 ^^ print_type_expr e)
  | e::items -> group (break 1 ^^ print_type_expr e ^^ string " *")
                ^^ app items
  in head ^^ string " *" ^^ app (Utils.nsepseq_to_list tail)

(* Functional type *)

and print_T_Fun (node : (type_expr * arrow * type_expr) reg) =
  let lhs, _, rhs = node.value in
  let lhs         = print_type_expr lhs
  and rhs         = print_type_expr rhs
  in group (lhs ^^ string " ->" ^/^ rhs)

(* Integer type *)

and print_T_Int (node :  (lexeme * Z.t) wrap) = print_int node

(* Module path *)

and print_T_ModPath (node : type_expr module_path reg) =
  print_module_path print_type_expr node

and print_module_path
  : type a.(a -> document) -> a module_path reg -> document =
  fun print node ->
    let node        = node.value in
    let module_path = node.module_path
    and field       = node.field
    in
    let modules     = Utils.nsepseq_to_list module_path
    and sep         = string "." ^^ break 0 in
    let modules     = separate_map sep print_ident modules in
    group (modules ^^ sep ^^ print field)

(* Type variable *)

and print_T_Var (node : variable) = print_ident node

(* Type string *)

and print_T_String (node : lexeme wrap) = print_string node

(* Sum type *)

and print_T_Sum (node : sum_type reg) =
  print_sum_type ~attr:false node

and print_sum_type ~(attr: bool) (node : sum_type reg) =
  let head, tail = node.value.variants in
  let head = print_variant head
  and padding_flat =
    if attr then string "| " else empty
  and padding_non_flat =
    if attr then string "| " else blank 2 in
  let head =
    if List.is_empty tail then head
    else ifflat (padding_flat ^^ head) (padding_non_flat ^^ head)
  and tail = List.map ~f:snd tail
  and app variant =
    group (break 1 ^^ string "| " ^^ print_variant variant) in
  let thread = head ^^ concat_map app tail
  in thread

and print_variant (node : variant reg) =
  let node       = node.value in
  let ctor       = node.ctor
  and args       = node.ctor_args
  and attributes = node.attributes
  in
  let thread     = print_ident ctor in
  let thread     = print_attributes thread attributes in
  match args with
    None -> thread
  | Some (_,e) -> let args = print_type_expr e
                 in prefix 4 1 (thread ^^ string " of") args

(* Record type *)

and print_T_Record (node : field_decl reg compound reg) =
  group (print_compound print_field_decl node)

and print_field_decl (node : field_decl reg) =
  let node       = node.value in
  let field_name = node.field_name
  and field_type = node.field_type
  and attributes = node.attributes
  in
  let thread     = print_ident field_name in
  let thread     = print_attributes thread attributes in
  match field_type with
    None -> thread
  | Some (_, e) -> (thread ^^ string " :") ^//^ print_type_expr e

and print_compound : 'a.('a -> document) -> 'a compound reg -> document =
  fun print node ->
    let node     = node.value in
    let kind     = node.kind
    and elements = node.elements
    in
    let sep      = string ";" ^^ break 1 in
    let elements = Utils.sepseq_to_list elements in
    let elements = separate_map sep print elements in
    let thread   = string (kind#payload ^ " [") in
    let thread   = thread ^^ nest 2 (break 0 ^^ elements) in
    let thread   = thread ^^ break 0 ^^ string "]"
    in group thread

(* Parenthesised type *)

and print_T_Par (node : type_expr par reg) =
  print_par print_type_expr node

(* Blocks *)

and print_block (node : block reg) =
  string "{" ^^ print_in_block node ^^ hardline ^^ string "}"

and print_in_block (node : block reg) =
  let statements = print_statements node.value.statements
  in nest 2 (hardline ^^ statements)

(* STATEMENTS *)

and print_statements (node : statements) =
  let statements = Utils.nsepseq_to_list node in
  let sep        = string ";" ^^ hardline ^^ hardline
  in List.filter_map ~f:print_statement statements
     |> separate_map sep group

and print_statement (node : statement) =
  match node with
    S_Attr    s -> print_S_Attr s
  | S_Decl    s -> print_S_Decl s
  | S_Instr   s -> Some (print_S_Instr   s)
  | S_VarDecl s -> Some (print_S_VarDecl s)

(* Attributed declarations *)

and print_S_Attr (node : attribute * statement) =
  let attributes, statement = unroll_S_Attr node in
  match print_statement statement with
    Some thread -> Some (print_attributes thread attributes)
  | None        -> None

(* Declaration as a statement *)

and print_S_Decl (node : declaration) = print_declaration node

(* Variable declaration (invalid at the top-level) *)

and print_S_VarDecl (node : var_decl reg) =
  let node        = node.value in
  let pattern     = print_pattern node.pattern
  and type_params = node.type_params
  and var_type    = node.var_type
  and init        = node.init
  in
  let thread = string "var " ^^ pattern in
  let thread = print_type_params thread type_params in
  let thread = print_opt_type thread var_type in
  let thread = print_init ":= " thread init
  in group thread

(* INSTRUCTIONS *)

and print_S_Instr (node : instruction) = print_instruction node

and print_instruction (node : instruction) =
  match node with
    I_Assign i -> print_I_Assign i
  | I_Call   i -> print_I_Call   i
  | I_Case   i -> print_I_Case   i
  | I_Cond   i -> print_I_Cond   i
  | I_For    i -> print_I_For    i
  | I_ForIn  i -> print_I_ForIn  i
  | I_Patch  i -> print_I_Patch  i
  | I_Remove i -> print_I_Remove i
  | I_Skip   i -> print_I_Skip   i
  | I_While  i -> print_I_While  i

(* Assignment *)

and print_I_Assign (node : assignment reg) =
  let node = node.value in
  let lhs  = node.lhs
  and rhs  = node.rhs
  in group (print_expr lhs ^^ string " :=" ^//^ print_expr rhs)

(* Procedure call *)

and print_I_Call (node : call) = print_call node

and print_call (node : call) =
  let node              = node.value in
  let lambda, arguments = node in
  let arguments         = print_call_args arguments in
  group (print_expr lambda ^^ nest 2 (break 1 ^^ arguments))

and print_call_args (node : call_args) =
  print_par (print_sepseq print_expr) node

(* Case *)

and print_I_Case (node : test_clause case reg) =
  print_case print_test_clause node

and print_case : 'a.('a -> document) -> 'a case Region.reg -> document =
  fun printer node ->
    let node  = node.value in
    let expr  = node.expr
    and cases = node.cases in
    let expr = nest 5 (print_expr expr)
    in
    group (string "case " ^^ expr ^/^ string "of [")
    ^^ hardline ^^ print_cases printer cases
    ^^ hardline ^^ string "]"

and print_cases :
  'a.('a -> document) ->
    ('a case_clause reg, vbar) Utils.nsepseq ->
    document =
  fun printer node ->
    let head, tail = node in
    let head       = print_case_clause printer head in
    let head       = blank 2 ^^ head in
    let tail       = List.map ~f:snd tail in
    let app clause = break 1 ^^ string "| "
                     ^^ print_case_clause printer clause
    in group (head ^^ concat_map app tail)

and print_case_clause :
  'a.('a -> document) -> 'a case_clause Region.reg -> document =
  fun print node ->
    let node    = node.value in
    let pattern = print_pattern node.pattern
    and rhs     = node.rhs in
    pattern ^^ prefix 4 1 (string " ->") (print rhs)

(* Conditional instruction *)

and print_I_Cond (node : test_clause conditional reg) =
  let node   = node.value in
  let test   = print_expr node.test
  and if_so  = node.if_so
  and if_not = node.if_not
  in
  let thread = string "if " ^^ group (nest 3 test) in
  let thread = thread ^/^ string "then"
               ^^ match if_so with
                    ClauseInstr instr ->
                      group (nest 2 (break 1 ^^ print_ClauseInstr instr))
                  | ClauseBlock block ->
                      string " {"
                      ^^ group (print_in_block block)
                      ^^ hardline ^^ string "}" in
  let thread = match if_not with
                Some (_, (ClauseInstr instr)) ->
                  thread ^/^ string "else"
                  ^^ group (nest 2 (break 1 ^^ print_ClauseInstr instr))
              | Some (_, (ClauseBlock block)) ->
                  thread ^/^ string "else {"
                  ^^ group (print_in_block block)
                  ^^ hardline ^^ string "}"
              | None -> thread
  in group thread

and print_test_clause = function
  ClauseInstr i -> print_ClauseInstr i
| ClauseBlock i -> print_ClauseBlock i

and print_ClauseInstr (node : instruction) = print_instruction node

and print_ClauseBlock (node : block reg) = print_block node

(* Interation over integer intervals *)

and print_I_For (node : for_int reg) =
  let node  = node.value in
  let index = print_ident node.index
  and init  = print_expr node.init
  and bound = print_expr node.bound
  and step  = node.step
  and block = print_block node.block
  in
  let first = (index ^^ string " :=") ^//^ init
  and last = string " to" ^//^ bound
  and step = match step with
               None -> empty
             | Some (_, e) -> string " step" ^//^ print_expr e
  in group (string "for" ^//^ first ^^ last ^^ step ^^ hardline ^^ block)

(* Iteration over collections *)

and print_I_ForIn = function
  ForMap       f -> print_ForMap       f
| ForSetOrList f -> print_ForSetOrList f

and print_ForMap (node : for_map reg) =
  let node        = node.value in
  let src, _, dst = node.binding
  and collection  = print_expr node.collection
  and block       = print_block node.block
  in
  let binding = print_ident src ^^ string " -> " ^^ print_ident dst
  in group ((string "for" ^//^ binding)
            ^^ (string " in map" ^//^ collection)
            ^^ hardline ^^ block)

and print_ForSetOrList (node : for_set_or_list reg) =
  let node       = node.value in
  let var        = print_ident node.var
  and kind       = print_for_kind node.for_kind
  and collection = print_expr node.collection
  and block      = print_block node.block
  in group ((string "for" ^//^ var)
            ^^ (string " in " ^^ kind ^//^ collection)
            ^^ hardline ^^ block)

and print_for_kind = function
  `Set  _ -> string "set"
| `List _ -> string "list"

(* Patches for maps, records, and sets. *)

and print_I_Patch (node : patch reg) =
  let node        = node.value in
  let collection  = print_expr node.collection
  and with_clause = print_with node.patch_kind node.patch
  and patch       = print_expr node.patch
  in
  string "patch"
  ^^ group (nest 2 (break 1 ^^ collection) ^/^ with_clause)
  ^^ group (nest 2 (break 1 ^^ patch))

and print_with (patch_kind : patch_kind) (patch : expr) =
  match patch with
    E_Record _ | E_Map _ | E_Set _ -> string "with"
  | _ -> match patch_kind with
           `Map    _ -> string "with map"
         | `Record _ -> string "with record"
         | `Set    _ -> string "with set"

(* Removal from sets and maps *)

and print_I_Remove (node : removal reg) =
  let node       = node.value in
  let item       = print_expr node.item
  and from       = print_remove_kind node.remove_kind
  and collection = print_expr node.collection
  in
  string "remove" ^^ group (nest 2 (break 1 ^^ item))
  ^^ group (break 1 ^^ string "from " ^^ from ^//^ collection)

and print_remove_kind = function
  `Set _ -> string "set"
| `Map _ -> string "map"

(* Skipping *)

and print_I_Skip (node : kwd_skip) = string node#payload

(* General loop *)

and print_I_While (node : while_loop reg) =
  let node  = node.value in
  let cond  = print_expr node.cond
  and block = print_block node.block
  in group ((string "while" ^//^ cond) ^^ hardline ^^ block)


(* PATTERNS *)

and print_pattern (node : pattern) =
  match node with
    P_App      p -> print_P_App     p
  | P_Attr     p -> print_P_Attr    p
  | P_Bytes    p -> print_P_Bytes   p
  | P_Cons     p -> print_P_Cons    p
  | P_Ctor     p -> print_P_Ctor    p
  | P_Int      p -> print_P_Int     p
  | P_List     p -> print_P_List    p
  | P_ModPath  p -> print_P_ModPath p
  | P_Mutez    p -> print_P_Mutez   p
  | P_Nat      p -> print_P_Nat     p
  | P_Nil      p -> print_P_Nil     p
  | P_Par      p -> print_P_Par     p
  | P_Record   p -> print_P_Record  p
  | P_String   p -> print_P_String  p
  | P_Tuple    p -> print_P_Tuple   p
  | P_Typed    p -> print_P_Typed   p
  | P_Var      p -> print_P_Var     p
  | P_Verbatim p -> print_P_Verbatim p

(* Pattern for the application of a data constructor *)

and print_P_App (node : (pattern * pattern tuple option) reg) =
  match node.value with
    ctor, None -> print_pattern ctor
  | ctor, Some tuple ->
      group (print_pattern ctor ^//^ print_tuple print_pattern tuple)

and print_tuple : 'a.('a -> document) -> 'a tuple -> document =
  fun print node ->
    let node = node.value in
    let head, tail = node.inside in
    let rec app = function
        []       -> empty
      | [x]      -> group (break 1 ^^ print x)
      | x::items -> group (break 1 ^^ print x ^^ string ",") ^^ app items in
    let components =
      if   List.is_empty tail
      then print head
      else print head ^^ string "," ^^ app (List.map ~f:snd tail)
    in string "(" ^^ nest 1 (components ^^ string ")")

(* Attributed pattern *)

and print_P_Attr (node : attribute * pattern) =
  let attributes, pattern = unroll_P_Attr node in
  let thread = print_pattern pattern in
  let thread = print_attributes thread attributes
  in thread

(* Pattern bytes *)

and print_P_Bytes (node : (lexeme * Hex.t) wrap) = print_bytes node

(* Pattern for consing *)

and print_P_Cons (node : (pattern * sharp * pattern) reg) =
  let node = node.value in
  let head, _, tail = node
  in group ((print_pattern head ^^ string " #") ^//^ print_pattern tail)

(* Constructor in a pattern *)

and print_P_Ctor (node : ctor) = print_ident node

(* Integer in a pattern *)

and print_P_Int (node : (lexeme * Z.t) wrap) = print_int node

(* Lists *)

and print_P_List (node : pattern compound reg) =
  print_compound print_pattern node

(* Module paths in patterns *)

and print_P_ModPath (node : pattern module_path reg) =
  print_module_path print_pattern node

(* Mutez in patterns *)

and print_P_Mutez (node : (lexeme * Int64.t) wrap) = print_mutez node

(* Natural numbers in patterns *)

and print_P_Nat (node : (lexeme * Z.t) wrap) = print_nat node

(* The empty list in patterns *)

and print_P_Nil (node : kwd_nil) = string node#payload

(* Parenthesised pattern *)

and print_P_Par (node : pattern par reg) =
  print_par print_pattern node

(* Record pattern *)

and print_P_Record (node : record_pattern) =
  print_compound print_field_pattern node

and print_field_pattern (node : field_pattern reg) =
  match node.value with
    Punned {pun; attributes} ->
      let thread = print_pattern pun in
      let thread = print_attributes thread attributes
      in thread
  | Complete {field_lhs; field_lens; field_rhs; attributes} ->
     let thread = group ((print_pattern field_lhs
                          ^^ string " "
                          ^^ print_field_lens field_lens)
                          ^//^ print_pattern field_rhs) in
      let thread = print_attributes thread attributes
      in thread

and print_field_lens (node : field_lens) =
  match node with
    Lens_Id   l
  | Lens_Add  l
  | Lens_Sub  l
  | Lens_Mult l
  | Lens_Div  l
  | Lens_Fun  l -> string l#payload

(* String patterns *)

and print_P_String (node : lexeme wrap) = print_string node

(* Tuple patterns *)

and print_P_Tuple (node : pattern tuple) = print_tuple print_pattern node

(* Typed patterns *)

and print_P_Typed (node : typed_pattern reg) =
  let {pattern; type_annot} = node.value in
  let _, type_expr = type_annot in
  group ((print_pattern pattern ^^ string ":")
         ^//^ print_type_expr type_expr)

(* Variable pattern *)

and print_P_Var (node : variable) = print_variable node

(* Verbatim string patterns *)

and print_P_Verbatim (node : lexeme wrap) = print_verbatim node


(* EXPRESSIONS *)

and print_expr (node : expr) =
  match node with
    E_Add       e -> print_E_Add       e
  | E_And       e -> print_E_And       e
  | E_App       e -> print_E_App       e
  | E_Attr      e -> print_E_Attr      e
  | E_BigMap    e -> print_E_BigMap    e
  | E_Block     e -> print_E_Block     e
  | E_Bytes     e -> print_E_Bytes     e
  | E_Call      e -> print_E_Call      e
  | E_Case      e -> print_E_Case      e
  | E_Cat       e -> print_E_Cat       e
  | E_CodeInj   e -> print_E_CodeInj   e
  | E_Ctor      e -> print_E_Ctor      e
  | E_Equal     e -> print_E_Equal     e
  | E_Cond      e -> print_E_Cond      e
  | E_Cons      e -> print_E_Cons      e
  | E_Div       e -> print_E_Div       e
  | E_Fun       e -> print_E_Fun       e
  | E_Geq       e -> print_E_Geq       e
  | E_Gt        e -> print_E_Gt        e
  | E_Int       e -> print_E_Int       e
  | E_Leq       e -> print_E_Leq       e
  | E_List      e -> print_E_List      e
  | E_Lt        e -> print_E_Lt        e
  | E_Map       e -> print_E_Map       e
  | E_MapLookup e -> print_E_MapLookup e
  | E_Mod       e -> print_E_Mod       e
  | E_Mult      e -> print_E_Mult      e
  | E_Mutez     e -> print_E_Mutez     e
  | E_Nat       e -> print_E_Nat       e
  | E_Neg       e -> print_E_Neg       e
  | E_Nil       e -> print_E_Nil       e
  | E_Neq       e -> print_E_Neq       e
  | E_Not       e -> print_E_Not       e
  | E_Or        e -> print_E_Or        e
  | E_Par       e -> print_E_Par       e
  | E_Record    e -> print_E_Record    e
  | E_Set       e -> print_E_Set       e
  | E_SetMem    e -> print_E_SetMem    e
  | E_String    e -> print_E_String    e
  | E_Sub       e -> print_E_Sub       e
  | E_Tuple     e -> print_E_Tuple     e
  | E_Typed     e -> print_E_Typed     e
  | E_Update    e -> print_E_Update    e
  | E_Verbatim  e -> print_E_Verbatim  e
  | E_ModPath   e -> print_E_ModPath   e
  | E_Var       e -> print_E_Var       e
  | E_Proj      e -> print_E_Proj      e

(* Addition *)

and print_E_Add (node : plus bin_op reg) = print_bin_op node

and print_bin_op (node : lexeme wrap bin_op reg) =
  let node = node.value in
  let {op; arg1; arg2} = node in
  let length = String.length op#payload + 1
  in group (print_expr arg1 ^/^ string (op#payload ^ " ")
            ^^ nest length (print_expr arg2))

(* Application to data constructors *)

and print_E_App (node : (expr * expr tuple option) reg) =
  match node.value with
    ctor, None -> print_expr ctor
  | ctor, Some tuple ->
      group (print_expr ctor ^//^ print_tuple print_expr tuple)

(* Attributes expressions *)

and print_E_Attr (node : attribute * expr) =
  let attributes, expr = unroll_E_Attr node in
  let thread = print_expr expr in
  let thread = print_attributes thread attributes
  in thread

(* Logical conjunction *)

and print_E_And (node : kwd_and bin_op reg) = print_bin_op node

(* Big map expression *)

and print_E_BigMap (node : binding reg compound reg) =
  print_compound print_binding node

and print_binding (node : binding reg) =
  let node  = node.value in
  let key   = print_expr node.key
  and value = print_expr node.value
  in key ^^ string " ->" ^^ group (nest 2 (break 1 ^^ value))

(* Block expression *)

and print_E_Block (node : block_with reg) =
  let node  = node.value in
  let block = node.block
  and expr  = print_expr node.expr
  in group (print_block block ^^ string " with"
            ^^ group (nest 4 (break 1 ^^ expr)))

(* Bytes expressions *)

and print_E_Bytes (node : (lexeme * Hex.t) wrap) = print_bytes node

(* Function calls *)

and print_E_Call (node : call) = print_call node

(* Case expressions *)

and print_E_Case (node : expr case reg) =
  print_case print_expr node

(* String concatenation *)

and print_E_Cat (node : caret bin_op reg) = print_bin_op node

(* Code injection *)

and print_E_CodeInj (node : code_inj reg) =
  let node     = node.value in
  let language = string node.language.value.value
  and code     = print_expr node.code
  in group (string "[%" ^^ language ^/^ code ^^ string "]")

(* Constructor in expressions *)

and print_E_Ctor (node : ctor) = print_ident node

(* Equality *)

and print_E_Equal (node : equal bin_op reg) = print_bin_op node

(* Conditional expression *)

and print_E_Cond (node : expr conditional reg) =
  let node   = node.value in
  let test   = print_expr node.test
  and if_so  = print_expr node.if_so
  and if_not = node.if_not
  in
  let thread = string "if "  ^^ group (nest 3 test) in
  let thread = thread ^/^ string "then"
               ^^ group (nest 2 (break 1 ^^ if_so)) in
  let thread =
    match if_not with
      None -> thread
    | Some (_, e) -> thread ^/^ string "else"
                    ^^ group (nest 2 (break 1 ^^ print_expr e))
  in group thread

(* Consing expression *)

and print_E_Cons (node : sharp bin_op reg) = print_bin_op node

(* Arithmetic division *)

and print_E_Div (node : slash bin_op reg) = print_bin_op node

(* Function expressions *)

and print_E_Fun (node : fun_expr reg) =
  let node        = node.value in
  let type_params = node.type_params
  and parameters  = print_par print_parameters node.parameters
  and ret_type    = node.ret_type
  and ret_expr    = print_expr node.return
  in
  let thread   = string "function" in
  let thread   = print_type_params thread type_params in
  let thread   = group (thread ^^ nest 2 (break 1 ^^ parameters)) in
  let thread   = print_opt_type thread ret_type in
  let ret_expr = nest 4 (break 1 ^^ ret_expr) in
  let thread   = thread ^^ string " is" ^^ group ret_expr
  in group thread

(* Greater or equal than *)

and print_E_Geq (node : geq bin_op reg) = print_bin_op node

(* Greater than *)

and print_E_Gt (node : gt bin_op reg) = print_bin_op node

(* Integers *)

and print_E_Int (node : (lexeme * Z.t) wrap) = print_int node

(* Lower or equal than *)

and print_E_Leq (node : leq bin_op reg) = print_bin_op node

(* List expressions *)

and print_E_List (node : expr compound reg) =
  print_compound print_expr node

(* Lower than *)

and print_E_Lt (node : lt bin_op reg) = print_bin_op node

(* Map expression *)

and print_E_Map (node : binding reg compound reg) =
  print_compound print_binding node

(* Map lookup *)

and print_E_MapLookup (node : map_lookup reg) =
  let {map; keys} = node.value in
  group (print_expr map ^//^ print_keys keys)

and print_keys (node : expr brackets reg Utils.nseq) =
  let keys = Utils.nseq_to_list node in
  let apply (key: expr brackets reg) acc =
    print_brackets print_expr key ^/^ acc
  in group (List.fold_right ~f:apply ~init:empty keys)

(* Modulo *)

and print_E_Mod (node : kwd_mod bin_op reg) = print_bin_op node

(* Module paths *)

and print_E_ModPath (node : expr module_path reg) =
  print_module_path print_expr node

(* Multiplication *)

and print_E_Mult (node : times bin_op reg) = print_bin_op node

(* Mutez as an expression *)

and print_E_Mutez (node : (lexeme * Int64.t) wrap) = print_mutez node

(* Natural numbers in expressions *)

and print_E_Nat (node :  (lexeme * Z.t) wrap) = print_nat node

(* Arithmetic negation *)

and print_E_Neg (node : minus un_op reg) = print_un_op node

and print_un_op (node : lexeme wrap un_op reg) =
  let {op; arg} = node.value in
  string (op#payload ^ " ") ^^ print_expr arg

(* The empty list as an expression *)

and print_E_Nil (node : kwd_nil) = string node#payload

(* Arithmetic difference *)

and print_E_Neq (node : neq bin_op reg) = print_bin_op node

(* Logical negation *)

and print_E_Not (node : kwd_not un_op reg) = print_un_op node

(* Logical disjunction *)

and print_E_Or (node : kwd_or bin_op reg) = print_bin_op node

(* Parenthesised expression *)

and print_E_Par (node : expr par reg) = print_par print_expr node

(* Record expression *)

and print_E_Record (node : record_expr) =
  print_compound print_field_expr node

and print_field_expr (node : (expr, expr) field reg) =
  match node.value with
    Punned {pun; attributes} ->
      let thread = print_expr pun in
      let thread = print_attributes thread attributes
      in thread
  | Complete {field_lhs; field_rhs; attributes; _} ->
      let thread = group ((print_expr field_lhs ^^ string " =")
                          ^//^ print_expr field_rhs) in
      let thread = print_attributes thread attributes
      in thread

(* Set expression *)

and print_E_Set (node : expr compound reg) =
  print_compound print_expr node

(* Set membership *)

and print_E_SetMem (node : set_membership reg) =
  let node    = node.value in
  let set     = print_expr node.set
  and element = print_expr node.element
  in group (set ^/^ string "contains" ^/^ element)

(* String expression *)

and print_E_String (node : lexeme wrap) = print_string node

(* Arithmetic subtraction *)

and print_E_Sub (node : minus bin_op reg) = print_bin_op node

(* Tuple expression *)

and print_E_Tuple (node : expr tuple) = print_tuple print_expr node

(* Typed expression *)

and print_E_Typed (node : typed_expr par reg) =
  let expr, (_, type_expr) = node.value.inside in
  group (string "("
         ^^ nest 1 (print_expr expr ^/^ string ": "
                    ^^ print_type_expr type_expr ^^ string ")"))

(* Functional update *)

and print_E_Update (node : update reg) =
  let node      = node.value in
  let structure = print_expr node.structure
  and update    = print_expr node.update
  in
  group (structure ^^ string " with" ^^ nest 2 (break 1 ^^ update))

(* Verbatim string expressions *)

and print_E_Verbatim (node : lexeme wrap) = print_verbatim node

(* Expression variable *)

and print_E_Var (node : variable) = print_variable node

(* Projection *)

and print_E_Proj (node : projection reg) =
  let node            = node.value in
  let record_or_tuple = print_expr node.record_or_tuple
  and field_path      = print_nsepseq print_selection node.field_path
  in group (record_or_tuple ^^ string "." ^^ break 0 ^^ field_path)

and print_selection (node : selection) =
  match node with
    FieldName name -> string name#payload
  | Component cmp  -> cmp#payload |> snd |> Z.to_string |> string

(* EXPORTS *)

let print_type_expr = print_type_expr
let print_pattern   = print_pattern
let print_expr      = print_expr

type cst        = CST.t
type expr       = CST.expr
type type_expr  = CST.type_expr
type pattern    = CST.pattern
