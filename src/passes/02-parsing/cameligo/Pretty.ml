(* A pretty printer for CameLIGO *)

(* Jane Street dependency *)

module List = Core.List

(* Vendored dependencies *)

module Utils = Simple_utils.Utils
module Region = Simple_utils.Region
open! Region

(* Local dependencies *)

module CST = Cst_cameligo.CST
open CST

(* Third party dependencies *)

open! PPrint

(* UTILITIES *)

let unroll_D_Attr (attr, decl) =
  let rec aux attrs = function
    D_Attr { value = (attr, decl); _ } -> aux (attr :: attrs) decl
  | decl                -> List.rev attrs, decl
  in aux [attr] decl

let unroll_T_Attr (attr, type_expr) =
  let rec aux attrs = function
    T_Attr (attr, type_expr) -> aux (attr :: attrs) type_expr
  | type_expr                -> List.rev attrs, type_expr
  in aux [attr] type_expr

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

let print_par : ('a -> document) -> 'a par -> document =
  fun print {value; _} ->
    string "(" ^^ nest 1 (print value.inside ^^ string ")")

(* PRINTING THE CST *)

let rec print _state cst = print_declarations cst.decl

(* DECLARATIONS (top-level) *)

and print_declarations (node : declaration Utils.nseq) =
   print_decl_list (Utils.nseq_to_list node)

and print_decl_list (node : declaration list) =
  List.map ~f:print_declaration node |> separate_map hardline group

and print_declaration = function
  D_Attr      d -> print_D_Attr      d
| D_Directive d -> print_D_Directive d
| D_Let       d -> print_D_Let       d ^^ hardline
| D_Module    d -> print_D_Module    d ^^ hardline
| D_Type      d -> print_D_Type      d ^^ hardline

(* Attributed declaration *)

and print_D_Attr (node : (attribute * declaration) reg) =
  let attributes, declaration = unroll_D_Attr node.value in
  let thread = print_declaration declaration
  in print_attributes thread attributes

and print_attribute (node : Attr.t wrap) =
  let key, val_opt = node#payload in
  let thread = string "[@" ^^ string key in
  let thread = match val_opt with
                 Some (String value | Ident value) ->
                   group (thread ^/^ nest 2 (string value))
    | None -> thread
  in thread ^^ string "]"

and print_attributes thread = function
     [] -> thread
| attrs -> group (separate_map (break 0) print_attribute attrs ^/^ thread)

(* Preprocessing directives *)

and print_D_Directive (node : Directive.t) =
  let comments  = Directive.get_comments node in
  let comments  = List.map ~f:(fun x -> x.value) comments in
  let directive = string (Directive.to_lexeme node).Region.value in
  if List.is_empty comments then directive else 
    let comments  = separate_map hardline string comments in
    comments ^/^ directive

(* Value declarations *)

and print_D_Let (node : let_decl reg) =
  let _, rec_opt, let_binding = node.value in
  let let_str =
    match rec_opt with
        None -> "let "
    | Some _ -> "let rec "
  in string let_str ^^ print_let_binding let_binding

and print_let_binding (node : let_binding) =
  let {binders; type_params; rhs_type; let_rhs; _} = node in
  let head, tail = binders in
  let thread = print_type_params (print_pattern head) type_params in
  let thread =
    if List.is_empty tail then thread
    else
      thread ^^
      group (nest 2 (break 1 ^^ separate_map (break 1) print_pattern tail)) in
  let thread = print_opt_type thread rhs_type in
  group (thread ^^ string " =" ^//^ print_expr let_rhs)

and print_opt_type thread (node : type_annotation option) =
  match node with
    None   -> thread
  | Some a -> print_type_annotation thread a

and print_type_annotation thread (_, type_expr : type_annotation) =
  group (thread ^/^ nest 2 (string ": " ^^ print_type_expr type_expr))

and print_type_params thread = function
  None    -> thread
| Some tv -> let params = print_nseq print_ident (snd tv.value.inside)
             in thread ^^ string "(type " ^^ params ^^ string ") "

and print_nseq : 'a.('a -> document) -> 'a Utils.nseq -> document =
  fun print (head, tail) -> separate_map (break 1) print (head::tail)

(* Module declaration (structure) *)

and print_D_Module (node : module_decl reg) =
  let node        = node.value in
  let name        = print_ident node.name
  and module_expr = print_module_expr node.module_expr
  in group (string "module " ^^ name ^^ string " = " ^^ module_expr)

and print_module_expr = function
  M_Body e -> print_M_Body e
| M_Path e -> print_M_Path e
| M_Var  e -> print_M_Var  e

and print_M_Body (node : module_body reg) =
  let decls = print_decl_list node.value.declarations in
  let decls = nest 2 (break 0 ^^ decls) in
  group (string "struct" ^^ decls ^^ hardline ^^ string "end")

and print_M_Path (node : module_name module_path reg) =
  print_module_path print_ident node

and print_M_Var (node : module_name) = print_ident node

(* Type declaration *)

and print_D_Type (node : type_decl reg) =
  let {name; params; type_expr; _} = node.value in
  let name    = print_ident name
  and params  = print_type_vars params
  and padding = match type_expr with T_Variant _ -> 0 | _ -> 2
  and t_expr  = print_type_expr type_expr in
  string "type " ^^ params ^^ name ^^ string " ="
  ^^ group (nest padding (break 1 ^^ t_expr))

and print_type_vars (node : type_vars option) =
  match node with
    None -> empty
  | Some TV_Single param ->
      print_type_var param ^^ string " "
  | Some TV_Tuple tuple ->
      print_par (print_nsepseq print_type_var) tuple ^^ string " "

and print_nsepseq :
  'a.('a -> document) -> ('a, lexeme wrap) Utils.nsepseq -> document =
  fun print ->
    function
      head, [] -> print head
    | _, (sep, _)::_ as elements ->
        let elems = Utils.nsepseq_to_list elements
        and sep   = string sep#payload ^^ hardline (*break 1*)
        in separate_map sep print elems

and print_type_var (node : type_var) =
  string "'" ^^ print_ident (snd node.value)

(* TYPE EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_type_expr = function
  T_App       t -> print_T_App       t
| T_Arg       t -> print_T_Arg       t
| T_Attr      t -> print_T_Attr      t
| T_Cart      t -> print_T_Cart      t
| T_Fun       t -> print_T_Fun       t
| T_Int       t -> print_T_Int       t
| T_ModPath   t -> print_T_ModPath   t
| T_Par       t -> print_T_Par       t
| T_Record    t -> print_T_Record    t
| T_String    t -> print_T_String    t
| T_Variant   t -> print_T_Variant   t
| T_Var       t -> print_T_Var       t
| T_Parameter t -> print_T_Parameter t

(* Type application *)

and print_T_App (node : (type_expr * type_ctor_arg) reg) =
  let ctor, arg = node.value in
  print_type_ctor_arg arg ^//^ print_type_expr ctor

and print_type_ctor_arg = function
  TC_Single t -> print_type_expr      t
| TC_Tuple  t -> print_ctor_arg_tuple t

and print_ctor_arg_tuple (node : type_expr tuple par) =
  let head, tail = node.value.inside in
  let rec app = function
    [] -> empty
  | [(_, e)] -> group (break 1 ^^ print_type_expr e)
  | (_, e) :: items ->
      group (break 1 ^^ print_type_expr e ^^ string ",") ^^ app items
  in
  match tail with
    [] -> print_type_expr head
  | h :: tail ->
    let components =
      print_type_expr head ^^ string "," ^^ app (h :: tail)
    in string "(" ^^ nest 1 (components ^^ string ")")

(* Type variable *)

and print_T_Arg (node : type_var) =
  let quote_opt, variable = node.value in
  let var_doc = print_ident variable in
  match quote_opt with
    None   -> var_doc
  | Some _ -> string "'" ^^ var_doc

(* Attributed type *)

and print_T_Attr (node : attribute * type_expr) =
  let attributes, type_expr = unroll_T_Attr node in
  let thread =
    match type_expr with
      T_Variant t ->
        print_variant_type ~attr:(not (List.is_empty attributes)) t
    | _ -> print_type_expr type_expr
  in print_attributes thread attributes

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
  group (print_type_expr lhs ^^ string " ->" ^/^ print_type_expr rhs)

(* Integer type *)

and print_T_Int (node : (lexeme * Z.t) wrap) = print_int node

(* Module path *)

and print_T_ModPath (node : type_expr module_path reg) =
  print_module_path print_type_expr node

and print_module_path
  : type a.(a -> document) -> a module_path reg -> document =
  fun print {value; _} ->
    let modules = Utils.nsepseq_to_list value.module_path
    and sep     = string "." ^^ break 0 in
    let modules = separate_map sep print_ident modules
    in group (modules ^^ sep ^^ print value.field)

(* Parenthesised type expressions *)

and print_T_Par (node : type_expr par) = print_par print_type_expr node

(* Record type *)

and print_T_Record (node : field_decl reg record) =
  print_record print_field_decl node

and print_field_decl (node : field_decl reg) =
  let {attributes; field_name; field_type} = node.value in
  let thread = print_ident field_name in
  let thread = print_attributes thread attributes
  in group (print_opt_type thread field_type)

and print_record : 'a.('a -> document) -> 'a record -> document =
  fun print {value; _} ->
    let fields = print_sepseq print value.inside in
    let fields = nest 1 (break 0 ^^ fields) in
    group (string "{" ^^ fields ^^ break 0 ^^ string "}")

and print_sepseq :
  'a.('a -> document) -> ('a, lexeme wrap) Utils.sepseq -> document =
  fun print -> function
    None     -> empty
  | Some seq -> print_nsepseq print seq

(* String type *)

and print_T_String (node : lexeme wrap) = print_string node

(* Variant types *)

and print_T_Variant (node : variant_type reg) =
  print_variant_type ~attr:false node

and print_variant_type ~(attr: bool) (node : variant_type reg) =
  let head, tail       = node.value.variants in
  let head             = print_variant head
  and padding_flat     = if attr then string "| " else empty
  and padding_non_flat = if attr then string "| " else blank 2 in
  let head =
    if List.is_empty tail then head
    else ifflat (padding_flat ^^ head) (padding_non_flat ^^ head)
  and app variant =
    group (hardline ^^ string "| " ^^ print_variant variant)
  in head ^^ concat_map app (List.map ~f:snd tail)

and print_variant (node : variant reg) =
  let {attributes; ctor; ctor_args} = node.value in
  let thread = print_ident ctor in
  let thread = print_attributes thread attributes in
  match ctor_args with
    None -> thread
  | Some (_,e) -> let args = print_type_expr e
                 in prefix 4 1 (thread ^^ string " of") args

(* Type variables *)

and print_T_Var (node : variable) = print_ident node

(* Parameter of *)

and print_T_Parameter (node : (module_name, dot) Utils.nsepseq reg) =
  let path = print_nsepseq print_ident node.value in
  let path = group (nest 0 (break 1 ^^ path))
  in string "(parameter_of " ^^ path ^^ string ")"

(* PATTERNS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_pattern = function
  P_App      p -> print_P_App      p
| P_Attr     p -> print_P_Attr     p
| P_Bytes    p -> print_P_Bytes    p
| P_Cons     p -> print_P_Cons     p
| P_Ctor     p -> print_P_Ctor     p
| P_Int      p -> print_P_Int      p
| P_List     p -> print_P_List     p
| P_ModPath  p -> print_P_ModPath  p
| P_Mutez    p -> print_P_Mutez    p
| P_Nat      p -> print_P_Nat      p
| P_Par      p -> print_P_Par      p
| P_Record   p -> print_P_Record   p
| P_String   p -> print_P_String   p
| P_Tuple    p -> print_P_Tuple    p
| P_Typed    p -> print_P_Typed    p
| P_Var      p -> print_P_Var      p
| P_Verbatim p -> print_P_Verbatim p
| P_Unit     p -> print_P_Unit     p

(* Pattern for the application of a data constructor *)

and print_P_App (node : (pattern * pattern option) reg) =
  match node.value with
    ctor, None   -> print_pattern ctor
  | ctor, Some p -> prefix 4 1 (print_pattern ctor) (print_pattern p)

(* Attributed pattern *)

and print_P_Attr (node : attribute * pattern) =
  let attributes, pattern = unroll_P_Attr node in
  print_attributes (print_pattern pattern) attributes

(* Pattern bytes *)

and print_P_Bytes (node : (lexeme * Hex.t) wrap) = print_bytes node

(* Pattern for consing *)

and print_P_Cons (node : (pattern * cons * pattern) reg) =
  let p1, _, p2 = node.value in
  print_pattern p1 ^^ string " ::" ^//^ print_pattern p2

(* Constructor in a pattern *)

and print_P_Ctor (node : ctor) = print_ident node

(* Integer in a pattern *)

and print_P_Int (node : (lexeme * Z.t) wrap) = print_int node

(* Lists *)

and print_P_List (node : pattern list_) =
  print_list print_pattern node.value.inside

and print_list : 'a.('a -> document) -> ('a, semi) Utils.sepseq -> document =
  fun print sepseq ->
    let sep      = string ";" ^^ break 1 in
    let elements = Utils.sepseq_to_list sepseq in
    let elements = separate_map sep print elements in
    group (string "[" ^^ nest 2 (break 0 ^^ elements)
           ^^ break 0 ^^ string "]")

(* Module paths in patterns *)

and print_P_ModPath (node : pattern module_path reg) =
  print_module_path print_pattern node

(* Mutez in patterns *)

and print_P_Mutez (node : (lexeme * Int64.t) wrap) = print_mutez node

(* Natural numbers in patterns *)

and print_P_Nat (node : (lexeme * Z.t) wrap) = print_nat node

(* Parenthesised pattern *)

and print_P_Par (node : pattern par) = print_par print_pattern node

(* Record pattern *)

and print_P_Record (node : record_pattern) =
  print_record print_field_pattern node

and print_field_pattern (node : (field_name, equal, pattern) field) =
  print_field ~lhs:print_ident ~lens:print_ident ~rhs:print_pattern node

and print_field :
  'lhs 'op 'rhs.
  lhs:('lhs -> document) ->
  lens:('op -> document) ->
  rhs:('rhs -> document) ->
  ('lhs,'op,'rhs) field ->
  document =
  fun ~lhs:print_lhs ~lens:print_lens ~rhs:print_rhs -> function
    Punned node ->
      let {pun; attributes} = node.value in
      print_attributes (print_lhs pun) attributes
  | Complete node ->
      let {field_lhs; field_lens; field_rhs; attributes; _} = node.value in
      let thread = print_lhs field_lhs
                   ^^ string " " ^^ print_lens field_lens
                   ^//^ print_rhs field_rhs
      in print_attributes thread attributes

(* String patterns *)

and print_P_String (node : lexeme wrap) = print_string node

(* Tuple patterns *)

and print_P_Tuple (node : pattern tuple reg) =
  let head, tail = node.value in
  let rec app = function
    []  -> empty
  | [p] -> group (break 1 ^^ print_pattern p)
  | p::items ->
      group (break 1 ^^ print_pattern p ^^ string ",") ^^ app items
  in if List.is_empty tail
     then print_pattern head
     else print_pattern head ^^ string "," ^^ app (List.map ~f:snd tail)

(* Typed patterns *)

and print_P_Typed (node : typed_pattern reg) =
  let pattern, type_annot = node.value in
  print_type_annotation (print_pattern pattern) type_annot

(* Variable pattern *)

and print_P_Var (node : variable) = print_ident node

(* Verbatim string patterns *)

and print_P_Verbatim (node : lexeme wrap) = print_verbatim node

(* Unit pattern *)

and print_P_Unit (_ : the_unit reg) = string "()"

(* EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_expr = function
  E_Add      e -> print_E_Add      e
| E_And      e -> print_E_And      e
| E_App      e -> print_E_App      e
| E_Attr     e -> print_E_Attr     e
| E_Bytes    e -> print_E_Bytes    e
| E_Cat      e -> print_E_Cat      e
| E_CodeInj  e -> print_E_CodeInj  e
| E_Cond     e -> print_E_Cond     e
| E_Cons     e -> print_E_Cons     e
| E_Contract e -> print_E_Contract e
| E_Ctor     e -> print_E_Ctor     e
| E_Div      e -> print_E_Div      e
| E_Equal    e -> print_E_Equal    e
| E_Fun      e -> print_E_Fun      e
| E_Geq      e -> print_E_Geq      e
| E_Gt       e -> print_E_Gt       e
| E_Int      e -> print_E_Int      e
| E_Land     e -> print_E_Land     e
| E_Leq      e -> print_E_Leq      e
| E_LetIn    e -> print_E_LetIn    e
| E_List     e -> print_E_List     e
| E_Lor      e -> print_E_Lor      e
| E_Lsl      e -> print_E_Lsl      e
| E_Lsr      e -> print_E_Lsr      e
| E_Lt       e -> print_E_Lt       e
| E_Lxor     e -> print_E_Lxor     e
| E_Match    e -> print_E_Match    e
| E_Mod      e -> print_E_Mod      e
| E_ModIn    e -> print_E_ModIn    e
| E_ModPath  e -> print_E_ModPath  e
| E_Mult     e -> print_E_Mult     e
| E_Mutez    e -> print_E_Mutez    e
| E_Nat      e -> print_E_Nat      e
| E_Neg      e -> print_E_Neg      e
| E_Neq      e -> print_E_Neq      e
| E_Not      e -> print_E_Not      e
| E_Or       e -> print_E_Or       e
| E_Par      e -> print_E_Par      e
| E_Proj     e -> print_E_Proj     e
| E_Record   e -> print_E_Record   e
| E_String   e -> print_E_String   e
| E_Sub      e -> print_E_Sub      e
| E_Tuple    e -> print_E_Tuple    e
| E_Typed    e -> print_E_Typed    e
| E_TypeIn   e -> print_E_TypeIn   e
| E_Unit     e -> print_E_Unit     e
| E_Update   e -> print_E_Update   e
| E_Var      e -> print_E_Var      e
| E_Verbatim e -> print_E_Verbatim e
| E_Seq      e -> print_E_Seq      e
| E_RevApp   e -> print_E_RevApp   e

(* Addition *)

and print_E_Add (node : plus bin_op reg) = print_bin_op node

and print_bin_op (node : lexeme wrap bin_op reg) =
  let {op; arg1; arg2} = node.value in
  let length = String.length op#payload + 1
  in group (print_expr arg1 ^/^ string (op#payload ^ " ")
            ^^ nest length (print_expr arg2))

(* Logical conjunction *)

and print_E_And (node : bool_and bin_op reg) = print_bin_op node

(* Application to data constructors and functions *)

and print_E_App (node : (expr * expr Utils.nseq) reg) =
  let fun_or_ctor, args = node.value in
  let args = print_nseq print_expr args in
  group (print_expr fun_or_ctor ^^ nest 2 (break 1 ^^ args))

(* Attributes expressions *)

and print_E_Attr (node : attribute * expr) =
  let attributes, expr = unroll_E_Attr node in
  let thread = print_expr expr in
  print_attributes thread attributes

(* Bytes expressions *)

and print_E_Bytes (node : (lexeme * Hex.t) wrap) = print_bytes node

(* String concatenation *)

and print_E_Cat (node : caret bin_op reg) = print_bin_op node

(* Code injection *)

and print_E_CodeInj (node : code_inj reg) =
  let {language; code; _} = node.value in
  let language = string language#payload.value
  and code     = print_expr code in
  group (string "[%" ^^ language ^/^ code ^^ string "]")

(* Conditional expression *)

and print_E_Cond (node : cond_expr reg) =
  let {test; if_so; if_not; _} = node.value in
  let test  = string "if " ^^ group (nest 3 (print_expr test))
  and if_so = string "then" ^^ group (nest 2 (break 1 ^^ print_expr if_so))
  in match if_not with
    Some (_, expr) ->
      let if_not =
        string "else" ^^ group (nest 2 (break 1 ^^ print_expr expr))
      in test ^/^ if_so ^/^ if_not
  | None -> test ^/^ if_so

(* Consing expression *)

and print_E_Cons (node : cons bin_op reg) = print_bin_op node

and print_E_Contract (node : (module_name, dot) Utils.nsepseq reg) =
  let path = print_nsepseq print_ident node.value in
  string "(contract_of " ^^ group (nest 0 (break 1 ^^ path)) ^^ string ")"

(* Constructor in expressions *)

and print_E_Ctor (node : ctor) = print_ident node

(* Arithmetic division *)

and print_E_Div (node : slash bin_op reg) = print_bin_op node

(* Equality *)

and print_E_Equal (node : equal bin_op reg) = print_bin_op node

(* Function expressions *)

and print_E_Fun (node : fun_expr reg) =
  let {type_params; binders; rhs_type; body; _} = node.value in
  let thread = string "fun " in
  let thread = print_type_params thread type_params in
  let thread = thread ^^ nest 4 (print_nseq print_pattern binders) in
  let thread = print_opt_type thread rhs_type in
  group (thread ^^ string " -> " ^^ nest 2 (print_expr body))

(* Greater or equal than *)

and print_E_Geq (node : geq bin_op reg) = print_bin_op node

(* Greater than *)

and print_E_Gt (node : gt bin_op reg) = print_bin_op node

(* Integers *)

and print_E_Int (node : (lexeme * Z.t) wrap) = print_int node

(* Bitwise conjunction *)

and print_E_Land (node : kwd_land bin_op reg) = print_bin_op node

(* Lower or equal than *)

and print_E_Leq (node : leq bin_op reg) = print_bin_op node

(* Local value definition *)

and print_E_LetIn (node : let_in reg) =
  let {kwd_rec; binding; body; _} = node.value in
  let let_str =
    match kwd_rec with
        None -> string "let "
    | Some _ -> string "let rec "
  in let_str ^^ print_let_binding binding
     ^^ string " in" ^^ hardline ^^ group (print_expr body)

(* List expressions *)

and print_E_List (node : expr list_) =
  print_list print_expr node.value.inside

(* Bitwise disjunction *)

and print_E_Lor (node : kwd_lor bin_op reg) = print_bin_op node

(* Bitwise left shift *)

and print_E_Lsl (node : kwd_lsl bin_op reg) = print_bin_op node

(* Bitwise right shift *)

and print_E_Lsr (node : kwd_lsr bin_op reg) = print_bin_op node

(* Lower than *)

and print_E_Lt (node : lt bin_op reg) = print_bin_op node

(* Bitwise exclusive disjunction *)

and print_E_Lxor (node : kwd_lxor bin_op reg) = print_bin_op node

(* Pattern matching *)

and print_E_Match (node : match_expr reg) =
  let {subject; clauses; _} = node.value in
  group (string "match " ^^ nest 6 (print_expr subject) ^/^ string "with")
  ^^ hardline ^^ print_clauses clauses

and print_clauses (node : (match_clause reg, vbar) Utils.nsepseq reg) =
  let head, tail = node.value in
  let head       = print_clause head in
  let head       = if List.is_empty tail then head else blank 2 ^^ head in
  let rest       = List.map ~f:snd tail in
  let app clause = break 1 ^^ string "| " ^^ print_clause clause
  in  head ^^ concat_map app rest

and print_clause (node: match_clause reg) =
  let {pattern; rhs; _} = node.value in
  print_pattern pattern ^^ prefix 4 1 (string " ->") (print_expr rhs)

(* Arithmethic modulo *)

and print_E_Mod (node : kwd_mod bin_op reg) = print_bin_op node

(* Local module definition *)

and print_E_ModIn (node : module_in reg) =
  let {mod_decl; body; _} = node.value in
  let {name; module_expr; _} = mod_decl
  in group (string "module"
            ^^ print_ident name ^^ string " = struct"
            ^//^ print_module_expr module_expr
            ^^ string " end in" ^^ hardline ^^ print_expr body)

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

and print_E_Neg (node : minus un_op reg) =
  string "-" ^^ print_expr node.value.arg

and print_un_op (node : lexeme wrap un_op reg) =
  let {op; arg} = node.value in
  string (op#payload ^ " ") ^^ print_expr arg

(* Arithmetic difference *)

and print_E_Neq (node : neq bin_op reg) = print_bin_op node

(* Logical negation *)

and print_E_Not (node : kwd_not un_op reg) = print_un_op node

(* Logical disjunction *)

and print_E_Or (node : kwd_or bin_op reg) = print_bin_op node

(* Parenthesised expression *)

and print_E_Par (node : expr par) = print_par print_expr node

(* Projection *)

and print_E_Proj (node : projection reg) = print_projection node

and print_projection (node : projection reg) =
  let node            = node.value in
  let record_or_tuple = print_expr node.record_or_tuple
  and field_path      = print_nsepseq print_selection node.field_path
  in group (record_or_tuple ^^ string "." ^^ break 0 ^^ field_path)

and print_selection = function
  FieldName name -> string name#payload
| Component cmp  -> cmp#payload |> snd |> Z.to_string |> string

(* Record expression *)

and print_E_Record (node : record_expr) =
  print_record print_field_expr node

and print_field_expr (node : (field_name, equal, expr) field) =
  print_field ~lhs:print_ident ~lens:print_ident ~rhs:print_expr node

(* String expression *)

and print_E_String (node : lexeme wrap) = print_string node

(* Arithmetic subtraction *)

and print_E_Sub (node : minus bin_op reg) = print_bin_op node

(* Tuple expression *)

and print_E_Tuple (node : expr tuple reg) =
  let head, tail = node.value in
  let rec app = function
    []  -> empty
  | [e] -> group (break 1 ^^ print_expr e)
  | e::items ->
      group (break 1 ^^ print_expr e ^^ string "," ^^ app items)
  in if List.is_empty tail
     then print_expr head
     else print_expr head ^^ string "," ^^ app (List.map ~f:snd tail)

(* Typed expression *)

and print_E_Typed (node : typed_expr par) =
  let expr, type_annot = node.value.inside in
  let cast = print_type_annotation (print_expr expr) type_annot
  in string "(" ^^ nest 1 cast ^^ string ")"

(* Local type definition *)

and print_E_TypeIn (node : type_in reg) =
  let {type_decl; body; _} = node.value in
  let {name; type_expr; _} = type_decl
  in string "type " ^^ print_ident name ^^ string " ="
     ^//^ print_type_expr type_expr
     ^^ string " in" ^^ hardline
     ^^ group (print_expr body)

(* Unit expression *)

and print_E_Unit (_ : the_unit reg) = string "()"

(* Functional update of records *)

and print_E_Update (node : update_expr braces) =
  let {record; updates; _} = node.value.inside in
  let updates = group (print_nsepseq print_field_path_assign updates)
  and record  = print_expr record in
  string "{" ^^ record ^^ string " with"
  ^^ nest 1 (break 1 ^^ updates ^^ string "}")

and print_field_path_assign (node : (path, lens, expr) field) =
  print_field ~lhs:print_path ~lens:print_lens ~rhs:print_expr node

and print_path = function
  Name p -> print_ident      p
| Path p -> print_projection p

and print_lens = function
  Lens_Id   l
| Lens_Add  l
| Lens_Sub  l
| Lens_Mult l
| Lens_Div  l
| Lens_Fun  l -> string l#payload

(* Expression variable *)

and print_E_Var (node : variable) = print_ident node

(* Verbatim string expressions *)

and print_E_Verbatim (node : lexeme wrap) = print_verbatim node

(* Sequence expressions *)

and print_E_Seq (node : sequence_expr reg) =
  let {compound; elements; _} = node.value in
  let sep = string ";" ^^ hardline in
  let elements = Utils.sepseq_to_list elements in
  let elements = separate_map sep print_expr elements in
  match compound with
    None -> elements
  | Some BeginEnd (kwd_begin, kwd_end) ->
      print_ident kwd_begin
      ^^ nest 2 (hardline ^^ elements) ^^ hardline
      ^^ print_ident kwd_end
  | Some Parens (lpar, rpar) ->
      print_ident lpar
      ^^ nest 2 (hardline ^^ elements) ^^ hardline
      ^^ print_ident rpar

(* Reverse application *)

and print_E_RevApp (node : rev_app bin_op reg) = print_bin_op node

(* EXPORTS *)

type environment = unit

let default_environment : environment = ()

let print_type_expr   _state = print_type_expr
let print_pattern     _state = print_pattern
let print_expr        _state = print_expr
let print_declaration _state = print_declaration

type cst         = CST.t
type expr        = CST.expr
type type_expr   = CST.type_expr
type pattern     = CST.pattern
type declaration = CST.declaration
