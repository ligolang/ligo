(* A pretty printer for CameLIGO *)

(* Jane Street dependency *)

module List = Core.List

(* Vendored dependencies *)

module Utils  = Simple_utils.Utils
module Region = Simple_utils.Region

(* Local dependencies *)

module CST = Cst_cameligo.CST
module PrettyComb = Parsing_shared.PrettyComb

(* Global openings *)

open CST
open! Region
open! PPrint

(* Utilities and local shadowings *)

let prefix = PrettyComb.prefix
let (^/^)  = PrettyComb.(^/^)
type state = PrettyComb.state

let (<@) = Utils.(<@)

(* Placement *)

let default_state : state =
  object
    method indent       = 2
    method leading_vbar = PrettyComb.Only_on_new_line
  end

(* Comments *)

let print_line_comment comment = string "//" ^^ string comment.value

let print_block_comment comment =
  string "(*" ^^ string comment.value ^^ string "*)"

let print_line_comment_opt prefix = function
  None -> prefix
| Some comment -> prefix ^^ space ^^ print_line_comment comment

let print_comment = function
  Wrap.Block comment -> print_block_comment comment
| Wrap.Line  comment -> print_line_comment  comment

let print_comments = function
  [] -> empty
| comments -> separate_map hardline print_comment comments ^^ hardline

(* Tokens *)

let token (t : string Wrap.t) : document =
  let prefix = print_comments t#comments ^/^ string t#payload
  in print_line_comment_opt prefix t#line_comment

let print_variable = function
  Var t -> token t
| Esc t ->
    let prefix = print_comments t#comments ^/^ string ("@" ^ t#payload)
    in print_line_comment_opt prefix t#line_comment

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
  print_enclosed_document state ?force_hardline inside 0 left right

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

(*
let print_par_like_document
  state inside ?(force_hardline : bool option) left right =
  print_enclosed_document state ?force_hardline inside 0 left right

let print_par state print (node : 'a par) =
  let {lpar; inside; rpar} = node.value in
  print_par_like_document
    state ~force_hardline:false (print inside) lpar rpar
*)

let print_par : ('a -> document) -> 'a par -> document =
  fun print node ->
    let {lpar; inside; rpar} = node.value in
    token lpar ^^ nest 1 (print inside ^^ token rpar)

(* The separator [sep] here represents some extra spacing (like spaces
   or newlines) that will be printed after every separator in a
   sequence of type [Utils.nsepseq]. *)

let print_nsepseq :
  'a.document -> ('a -> document) ->
  ('a, lexeme Wrap.t) Utils.nsepseq -> document =
  fun terminator print elements ->
    let hd, tl = elements in
    let rec separate_map = function
      []            -> empty
    | (sep', x)::xs -> token sep' ^^ terminator ^^ print x ^^ separate_map xs
    in group (print hd ^^ separate_map tl)

let print_sepseq :
  'a.document -> ('a -> document) ->
  ('a, lexeme wrap) Utils.sepseq -> document =
  fun terminator print -> function
    None     -> empty
  | Some seq -> print_nsepseq terminator print seq

let print_nseq : 'a.('a -> document) -> 'a Utils.nseq -> document =
  fun print (head, tail) -> separate_map (break 1) print (head::tail)

(* Enclosed structures *)
(*
let is_enclosed_expr = function
  E_List _ | E_Par _ | E_Record _ | E_Update _ | E_Seq _ -> true
| _ -> false

let is_enclosed_type = function
  T_Par _ | T_Record _ -> true
| _ -> false
*)

(* UTILITIES *)

let unroll_D_Attr (attr, decl) =
  let rec aux attrs = function
    D_Attr {value = (attr, decl); _ } -> aux (attr :: attrs) decl
  | decl                              -> List.rev attrs, decl
  in aux [attr] decl

let unroll_S_Attr (attr, sig_item) =
  let rec aux attrs = function
    S_Attr {value = (attr, sig_item); _ } -> aux (attr :: attrs) sig_item
  | sig_item                              -> List.rev attrs, sig_item
  in aux [attr] sig_item

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
  let prefix = print_comments node#comments
               ^/^ string ("0x" ^ Hex.show (snd node#payload))
  in print_line_comment_opt prefix node#line_comment

let print_mutez (node : (lexeme * Int64.t) wrap) =
  let prefix = print_comments node#comments
               ^/^ (Int64.to_string (snd node#payload) ^ "mutez" |> string)
  in print_line_comment_opt prefix node#line_comment

let print_string (node : lexeme wrap) = dquotes (token node)

let print_verbatim (node : lexeme wrap) =
  string "{|" ^^ token node ^^ string "|}"

let print_int (node : (lexeme * Z.t) wrap) =
  let prefix = print_comments node#comments
               ^/^ string (Z.to_string (snd node#payload))
  in print_line_comment_opt prefix node#line_comment

and print_nat (node : (lexeme * Z.t) wrap) =
  let prefix = print_comments node#comments
               ^/^ string (Z.to_string (snd node#payload) ^ "n")
  in print_line_comment_opt prefix node#line_comment

(* PRINTING THE CST *)

let rec print state (cst: CST.t) =
  let {decl; eof} = cst in
  let decls = print_declarations state decl
  in match eof#comments with
       [] -> decls
     | comments -> decls ^/^ print_comments comments

(* DECLARATIONS (top-level) *)

and print_declarations state (node : declaration Utils.nseq) =
   print_decl_list state (Utils.nseq_to_list node)

and print_decl_list state (node : declaration list) =
  List.map ~f:(print_declaration state) node
  |> separate_map hardline group

and print_declaration state = function
  D_Attr      d -> print_D_Attr      state d
| D_Directive d -> print_D_Directive state d
| D_Let       d -> print_D_Let       state d ^^ hardline
| D_Module    d -> print_D_Module    state d ^^ hardline
| D_Include   d -> print_D_Include   state d ^^ hardline
| D_Type      d -> print_D_Type      state d ^^ hardline
| D_Signature d -> print_D_Signature state d ^^ hardline


(* SIGNATURE DECLARATIONS *)

and print_sig_item_list state (node : sig_item list) =
  List.map ~f:(print_sig_item state) node
  |> separate_map hardline group

and print_sig_item state = function
  S_Attr    d -> print_S_Attr    state d
| S_Value   d -> print_S_Value   state d ^^ hardline
| S_Type    d -> print_S_Type    state d ^^ hardline
| S_TypeVar d -> print_S_TypeVar       d ^^ hardline
| S_Include d -> print_S_Include state d ^^ hardline

(* Attributed declaration *)

and print_D_Attr state (node : (attribute * declaration) reg) =
  let attributes, declaration = unroll_D_Attr node.value in
  let thread = print_declaration state declaration
  in print_attributes state thread attributes

and print_attribute state (node : Attr.t wrap) =
  let key, val_opt = node#payload in
  let thread = string key in
  let thread = match val_opt with
                 Some (String value | Ident value) ->
                   group (thread ^/^ nest state#indent (string value))
               | None -> thread in
  let thread = print_comments node#comments
               ^/^ lbracket ^^ at ^^ thread ^^ rbracket
  in print_line_comment_opt thread node#line_comment

and print_attributes state thread = function
  [] -> thread
| a  -> group (separate_map (break 0) (print_attribute state) a ^/^ thread)

(* Attributed sig. item *)

and print_S_Attr state (node : (attribute * sig_item) reg) =
  let attributes, sig_item = unroll_S_Attr node.value in
  let thread = print_sig_item state sig_item
  in print_attributes state thread attributes

(* Preprocessing directives *)

and print_D_Directive _state (node : Directive.t) =
  string (Directive.to_lexeme node).Region.value

(* Value declarations *)

and print_D_Let state (node : let_decl reg) =
  let kwd_let, rec_opt, let_binding = node.value in
  let let_str =
    match rec_opt with
      None         -> token kwd_let
    | Some kwd_rec -> token kwd_let ^^ space ^^ token kwd_rec
  in let_str ^^ space ^^ print_let_binding state let_binding

and print_let_binding state (node : let_binding) =
  let {binders; type_params; rhs_type; eq; let_rhs} = node in
  let head, tail = binders in
  let thread = print_type_params (print_pattern state head) type_params in
  let thread =
    if List.is_empty tail then thread
    else
      let patterns = separate_map (break 1) (print_pattern state) tail in
      thread ^^ group (nest state#indent (break 1 ^^ patterns)) in
  let thread = print_opt_type state thread rhs_type in
  group (thread ^^ space ^^ token eq ^//^ print_expr state let_rhs)

and print_opt_type state thread = function
  None   -> thread
| Some a -> print_type_annotation state thread a

and print_type_annotation state thread (colon, type_expr : type_annotation) =
  group (thread ^/^
         nest
           state#indent
           (token colon ^^ space ^^ print_type_expr state type_expr))

and print_type_params thread (node : type_params par option) =
  match node with
    None    -> thread
  | Some {value; _ } ->
      let {lpar; inside=(kwd_type, vars); rpar} = value in
      let params = print_nseq print_variable vars in
      thread ^^ space ^^ token lpar ^^ token kwd_type ^^ space ^^ params ^^ token rpar


(* Value declarations (signature) *)

and print_S_Value state (node : (kwd_val * variable * colon * type_expr) reg) =
  let kwd_val, var, colon, type_expr = node.value
  in token kwd_val ^^ space ^^ print_variable var ^^ space ^^ token colon
     ^^ space ^^ print_type_expr state type_expr

(* Module declaration (structure) *)

and print_D_Module state (node : module_decl reg) =
  let {kwd_module; name; eq; module_expr; annotation} = node.value in
  let name        = token name
  and module_expr = print_module_expr state module_expr
  and sig_expr    =
    match annotation with
      None -> empty
    | Some (colon, sig_) ->
        token colon ^^ space ^/^ print_signature_expr state sig_ ^^ space
  in group (token kwd_module ^^ space ^^ name ^^ space ^^ sig_expr
            ^^ token eq ^^ space ^^ module_expr)

and print_D_Include state (node : module_include reg) =
  let {kwd_include ; module_expr } = node.value in
  let module_expr = print_module_expr state module_expr
  in group (token kwd_include ^^ space ^^ module_expr)

and print_module_expr state = function
  M_Body e -> print_M_Body state e
| M_Path e -> print_M_Path       e
| M_Var  e -> print_M_Var        e

and print_M_Body state (node : module_body reg) =
  let {kwd_struct; declarations; kwd_end} = node.value in
  let decls = print_decl_list state declarations in
  let decls = nest state#indent (break 0 ^^ decls) in
  group (token kwd_struct ^^ decls ^^ hardline ^^ token kwd_end)

and print_M_Path (node : module_name module_path reg) =
  print_module_path token node

and print_M_Var (node : module_name) = token node

(* Module declaration (signature) *)

and print_D_Signature state (node : signature_decl reg) =
  let {kwd_module; kwd_type; name; eq; signature_expr} = node.value in
  let name           = token name
  and signature_expr = print_signature_expr state signature_expr
  in group (token kwd_module ^^ space ^^ token kwd_type ^^ space
            ^^ name ^^ space ^^ token eq ^^ space ^^ signature_expr)

and print_signature_expr state = function
  S_Sig  e -> print_S_sig  state e
| S_Path e -> print_S_path       e
| S_Var  e -> print_S_var        e

and print_S_sig state (node : signature_body reg) =
  let {kwd_sig; sig_items; kwd_end} = node.value in
  let decls = print_sig_item_list state sig_items in
  let decls = nest state#indent (break 0 ^^ decls) in
  group (token kwd_sig ^^ decls ^^ hardline ^^ token kwd_end)

and print_S_path (node : module_name module_path reg) =
  print_module_path token node

and print_S_var (node : module_name) = token node

(* Type declaration *)

and print_D_Type state (node : type_decl reg) =
  print_type_decl state node.value

and print_type_decl state (node : type_decl) =
  let {kwd_type; params; name; eq; type_expr} = node in
  let name    = print_variable name
  and params  = print_type_vars params
  and padding = match type_expr with T_Variant _ -> 0 | _ -> state#indent
  and t_expr  = print_type_expr state type_expr in
  token kwd_type ^^ space ^^ params ^^ name ^^ space ^^ token eq
  ^^ group (nest padding (break 1 ^^ t_expr))

and print_type_vars (node : type_vars option) =
  match node with
    None -> empty
  | Some TV_Single param ->
      print_type_var param ^^ space
  | Some TV_Tuple tuple ->
      print_par (print_nsepseq (break 1) print_type_var) tuple ^^ space

and print_type_var (node : type_var) =
  match node.value with
    Some quote, var -> token quote ^^ print_variable var
  | None, var       -> print_variable var

(* Type declaration (signature) *)

and print_S_Type state (node : (kwd_type * variable * equal * type_expr) reg) =
  let kwd_type, name, eq, type_expr = node.value in
  let name    = print_variable name
  and padding = match type_expr with T_Variant _ -> 0 | _ -> state#indent
  and t_expr  = print_type_expr state type_expr in
  token kwd_type ^^ space ^^ name ^^ space ^^ token eq
  ^^ group (nest padding (break 1 ^^ t_expr))


and print_S_TypeVar (node : (kwd_type * variable) reg) =
  let kwd_type, name = node.value in
  let name           = print_variable name
  in token kwd_type ^^ space ^^ name


and print_S_Include state (node : (kwd_include * signature_expr) reg) =
  let kwd_include, sig_expr = node.value in
  let sig_          = print_signature_expr state sig_expr
  in token kwd_include ^^ space ^^ sig_

(* TYPE EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_type_expr state = function
  T_App         t -> print_T_App       state t
| T_Arg         t -> print_T_Arg             t
| T_Attr        t -> print_T_Attr      state t
| T_Cart        t -> print_T_Cart      state t
| T_Fun         t -> print_T_Fun       state t
| T_Int         t -> print_T_Int             t
| T_ModPath     t -> print_T_ModPath   state t
| T_Par         t -> print_T_Par       state t
| T_Record      t -> print_T_Record    state t
| T_String      t -> print_T_String          t
| T_Variant     t -> print_T_Variant   state t
| T_Var         t -> print_T_Var             t
| T_ParameterOf t -> print_T_ParameterOf     t

(* Type application *)

and print_T_App state (node : (type_expr * type_ctor_arg) reg) =
  let ctor, arg = node.value in
  print_type_ctor_arg state arg ^//^ print_type_expr state ctor

and print_type_ctor_arg state = function
  TC_Single t -> print_type_expr      state t
| TC_Tuple  t -> print_ctor_arg_tuple state t

and print_ctor_arg_tuple state (node : type_expr tuple par) =
  let {lpar; inside; rpar} = node.value in
  let head, tail = inside in
  let rec app = function
    [] -> empty
  | [(_, e)] -> group (break 1 ^^ print_type_expr state e)
  | (sep, e) :: items ->
      group (break 1 ^^ print_type_expr state e ^^ token sep) ^^ app items
  in
  match tail with
    [] -> print_type_expr state head
  | h :: tail ->
      let components =
        print_type_expr state head ^^ comma ^^ app (h :: tail)
      in token lpar ^^ nest 1 (components ^^ token rpar)

(* Type variable *)

and print_T_Arg (node : type_var) =
  let quote_opt, variable = node.value in
  let var_doc = print_variable variable in
  match quote_opt with
    None   -> var_doc
  | Some _ -> squote ^^ var_doc

(* Attributed type *)

and print_T_Attr state (node : attribute * type_expr) =
  let attributes, type_expr = unroll_T_Attr node in
  let thread =
    match type_expr with
      T_Variant t ->
        print_variant_type state ~attr:(not (List.is_empty attributes)) t
    | _ -> print_type_expr state type_expr
  in print_attributes state thread attributes

(* Cartesian type *)

and print_T_Cart state (node : cartesian) =
  let head, times, tail = node.value in
  let head = print_type_expr state head in
  let rec app = function
    []       -> empty
  | [e]      -> group (break 1 ^^ print_type_expr state e)
  | e::items -> group (break 1 ^^ print_type_expr state e
                       ^^ space ^^ token times)
                ^^ app items
  in head ^^ space ^^ token times ^^ app (Utils.nsepseq_to_list tail)

(* Functional type *)

and print_T_Fun state (node : (type_expr * arrow * type_expr) reg) =
  let lhs, arrow, rhs = node.value in
  let lhs = print_type_expr state lhs
  and rhs = print_type_expr state rhs
  in group (lhs ^^ space ^^ token arrow ^/^ rhs)

(* Integer type *)

and print_T_Int (node : (lexeme * Z.t) wrap) = print_int node

(* Module path *)

and print_T_ModPath state (node : type_expr module_path reg) =
  print_module_path (print_type_expr state) node

and print_module_path
  : type a.(a -> document) -> a module_path reg -> document =
  fun print node ->
    let {module_path; selector; field} = node.value in
    let modules = Utils.nsepseq_to_list module_path
    and sep     = token selector ^^ break 0 in
    let modules = separate_map sep token modules
    in group (modules ^^ sep ^^ print field)

(* Parenthesised type expressions *)

and print_T_Par state (node : type_expr par) =
  print_par (print_type_expr state) node

(* Record type *)

and print_T_Record state (node : field_decl reg record) =
  print_record (print_field_decl state) node

and print_field_decl state (node : field_decl reg) =
  let {attributes; field_name; field_type} = node.value in
  let thread = print_variable field_name in
  let thread = print_attributes state thread attributes
  in group (print_opt_type state thread field_type)

and print_record : 'a.('a -> document) -> 'a record -> document =
  fun print node ->
    let {lbrace; inside; rbrace} = node.value in
    let fields = print_sepseq hardline print inside in
    let fields = nest 1 (break 0 ^^ fields) in
    group (token lbrace ^^ fields ^^ break 0 ^^ token rbrace)

(* String type *)

and print_T_String (node : lexeme wrap) = print_string node

(* Variant types *)

and print_T_Variant state (node : variant_type reg) =
  print_variant_type state ~attr:false node

and print_variant_type state ~(attr: bool) (node : variant_type reg) =
  let head, tail =
    Utils.nsepseq_map (nest state#indent <@ print_variant state)
                      node.value.variants
  and padding_flat =
    let open PrettyComb in
    if attr then bar ^^ space
    else match state#leading_vbar with
           Avoid | Only_on_new_line -> empty
         | Always -> bar ^^ space
  and padding_non_flat =
    let open PrettyComb in
    if attr then bar ^^ space
    else match state#leading_vbar with
           Avoid -> blank state#indent
         | Always | Only_on_new_line -> bar ^^ space in

  (* Do not append a vertical bar if we are in flat mode, unless we
     have attributes. The reason is that those two are different:

     type t = [@annot] | Ctor
     type t = [@annot] Ctor
  *)

  let head =
    if List.is_empty tail then head
    else ifflat padding_flat padding_non_flat ^^ head

  and app variant = group (hardline ^^ bar ^^ space ^^ variant)

  in head ^^ concat_map app (List.map ~f:snd tail)

and print_variant state (node : variant reg) =
  let {attributes; ctor; ctor_args} = node.value in
  let thread = token ctor in
  let thread = print_attributes state thread attributes in
  match ctor_args with
    None -> thread
  | Some (kwd_of, e) ->
      group (thread ^^ space
             ^^ token kwd_of ^^ space ^^ print_type_expr state e)

(* Type variables *)

and print_T_Var (node : variable) = print_variable node

(* Parameter of *)

and print_T_ParameterOf (node : (module_name, dot) Utils.nsepseq reg) =
  let path = print_nsepseq (break 0) token node.value in
  let path = group (nest 0 (break 1 ^^ path))
  in path ^^ space ^^ string "parameter_of"

(* PATTERNS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_pattern state = function
  P_App      p -> print_P_App      state p
| P_Attr     p -> print_P_Attr     state p
| P_Bytes    p -> print_P_Bytes          p
| P_Cons     p -> print_P_Cons     state p
| P_Ctor     p -> print_P_Ctor           p
| P_False    p -> print_P_False          p
| P_Int      p -> print_P_Int            p
| P_List     p -> print_P_List     state p
| P_ModPath  p -> print_P_ModPath  state p
| P_Mutez    p -> print_P_Mutez          p
| P_Nat      p -> print_P_Nat            p
| P_Par      p -> print_P_Par      state p
| P_Record   p -> print_P_Record   state p
| P_String   p -> print_P_String         p
| P_True     p -> print_P_True           p
| P_Tuple    p -> print_P_Tuple    state p
| P_Typed    p -> print_P_Typed    state p
| P_Var      p -> print_P_Var            p
| P_Verbatim p -> print_P_Verbatim       p
| P_Unit     p -> print_P_Unit           p

(* Pattern for the application of a data constructor *)

and print_P_App state (node : (pattern * pattern option) reg) =
  match node.value with
    ctor, None   -> print_pattern state ctor
  | ctor, Some p ->
      let print = print_pattern state in
      prefix (2 * state#indent) 1 (print ctor) (print p)

(* Attributed pattern *)

and print_P_Attr state (node : attribute * pattern) =
  let attributes, pattern = unroll_P_Attr node in
  print_attributes state (print_pattern state pattern) attributes

(* Pattern bytes *)

and print_P_Bytes (node : (lexeme * Hex.t) wrap) = print_bytes node

(* Pattern for consing *)

and print_P_Cons state (node : (pattern * cons * pattern) reg) =
  let p1, cons, p2 = node.value in
  let p1 = print_pattern state p1
  and p2 = print_pattern state p2
  in p1 ^^ space ^^ token cons ^//^ p2

(* Constructor in a pattern *)

and print_P_Ctor (node : ctor) = token node

(* "false" as pattern *)

and print_P_False (node : kwd_false) = token node

(* Integer in a pattern *)

and print_P_Int (node : (lexeme * Z.t) wrap) = print_int node

(* Lists *)

and print_P_List state (node : pattern list_) =
  print_list state (print_pattern state) node

and print_list :
  'a.state -> ('a -> document) -> 'a list_ -> document =
  fun state print list ->
    print_brackets state (print_sepseq (break 1) print) list

(* Module paths in patterns *)

and print_P_ModPath state (node : pattern module_path reg) =
  print_module_path (print_pattern state) node

(* Mutez in patterns *)

and print_P_Mutez (node : (lexeme * Int64.t) wrap) = print_mutez node

(* Natural numbers in patterns *)

and print_P_Nat (node : (lexeme * Z.t) wrap) = print_nat node

(* Parenthesised pattern *)

and print_P_Par state (node : pattern par) =
  print_par (print_pattern state) node

(* Record pattern *)

and print_P_Record state (node : record_pattern) =
  print_record (print_field_pattern state) node

and print_field_pattern state (node : (field_name, equal, pattern) field) =
  print_field
    state
    ~lhs:print_variable
    ~lens:token
    ~rhs:(print_pattern state)
    node

and print_field :
  'lhs 'op 'rhs.
  state ->
  lhs:('lhs -> document) ->
  lens:('op -> document) ->
  rhs:('rhs -> document) ->
  ('lhs,'op,'rhs) field ->
  document =
  fun state ~lhs:print_lhs ~lens:print_lens ~rhs:print_rhs -> function
    Punned node ->
      let {pun; attributes} = node.value in
      print_attributes state (print_lhs pun) attributes
  | Complete node ->
      let {field_lhs; field_lens; field_rhs; attributes} = node.value in
      let thread = print_lhs field_lhs
                   ^^ space ^^ print_lens field_lens
                   ^//^ print_rhs field_rhs
      in print_attributes state thread attributes

(* String patterns *)

and print_P_String (node : lexeme wrap) = print_string node

(* "true" as pattern *)

and print_P_True (node : kwd_true) = token node

(* Tuple patterns *)

and print_P_Tuple state (node : pattern tuple reg) =
  let head, tail = node.value in
  let rec app = function
    []  -> empty
  | [_, p] -> group (break 1 ^^ print_pattern state p)
  | (comma, p)::items ->
      group (break 1 ^^ print_pattern state p ^^ token comma)
      ^^ app items in
  let head = print_pattern state head in
  if List.is_empty tail
  then head
  else head ^^ comma ^^ app tail

(* Typed patterns *)

and print_P_Typed state (node : typed_pattern reg) =
  let pattern, type_annot = node.value in
  print_type_annotation state (print_pattern state pattern) type_annot

(* Variable pattern *)

and print_P_Var (node : variable) = print_variable node

(* Verbatim string patterns *)

and print_P_Verbatim (node : lexeme wrap) = print_verbatim node

(* Unit pattern *)

and print_P_Unit (node : the_unit reg) =
  let lpar, rpar = node.value
  in token lpar ^^ token rpar

(* EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_expr state = function
  E_Add        e -> print_E_Add      state e
| E_And        e -> print_E_And      state e
| E_App        e -> print_E_App      state e
| E_Assign     e -> print_E_Assign   state e
| E_Attr       e -> print_E_Attr     state e
| E_Bytes      e -> print_E_Bytes          e
| E_Cat        e -> print_E_Cat      state e
| E_CodeInj    e -> print_E_CodeInj  state e
| E_Cond       e -> print_E_Cond     state e
| E_Cons       e -> print_E_Cons     state e
| E_ContractOf e -> print_E_ContractOf     e
| E_Ctor       e -> print_E_Ctor           e
| E_Div        e -> print_E_Div      state e
| E_Equal      e -> print_E_Equal    state e
| E_False      e -> print_E_False          e
| E_For        e -> print_E_For      state e
| E_ForIn      e -> print_E_ForIn    state e
| E_Fun        e -> print_E_Fun      state e
| E_Geq        e -> print_E_Geq      state e
| E_Gt         e -> print_E_Gt       state e
| E_Int        e -> print_E_Int            e
| E_Land       e -> print_E_Land     state e
| E_Leq        e -> print_E_Leq      state e
| E_LetIn      e -> print_E_LetIn    state e
| E_LetMutIn   e -> print_E_LetMutIn state e
| E_List       e -> print_E_List     state e
| E_Lor        e -> print_E_Lor      state e
| E_Lsl        e -> print_E_Lsl      state e
| E_Lsr        e -> print_E_Lsr      state e
| E_Lt         e -> print_E_Lt       state e
| E_Lxor       e -> print_E_Lxor     state e
| E_Match      e -> print_E_Match    state e
| E_Mod        e -> print_E_Mod      state e
| E_ModIn      e -> print_E_ModIn    state e
| E_ModPath    e -> print_E_ModPath  state e
| E_Mult       e -> print_E_Mult     state e
| E_Mutez      e -> print_E_Mutez          e
| E_Nat        e -> print_E_Nat            e
| E_Neg        e -> print_E_Neg      state e
| E_Neq        e -> print_E_Neq      state e
| E_Not        e -> print_E_Not      state e
| E_Or         e -> print_E_Or       state e
| E_Par        e -> print_E_Par      state e
| E_Proj       e -> print_E_Proj     state e
| E_Record     e -> print_E_Record   state e
| E_RevApp     e -> print_E_RevApp   state e
| E_Seq        e -> print_E_Seq      state e
| E_String     e -> print_E_String         e
| E_Sub        e -> print_E_Sub      state e
| E_True       e -> print_E_True           e
| E_Tuple      e -> print_E_Tuple    state e
| E_Typed      e -> print_E_Typed    state e
| E_TypeIn     e -> print_E_TypeIn   state e
| E_Unit       e -> print_E_Unit           e
| E_Update     e -> print_E_Update   state e
| E_Var        e -> print_E_Var            e
| E_Verbatim   e -> print_E_Verbatim       e
| E_While      e -> print_E_While    state e

(* Addition *)

and print_E_Add state (node : plus bin_op reg) = print_bin_op state node

and print_bin_op state (node : lexeme wrap bin_op reg) =
  let {op; arg1; arg2} = node.value in
  let length = String.length op#payload + 1
  in group (print_expr state arg1 ^/^ token op ^^ space
            ^^ nest length (print_expr state arg2))

(* Logical conjunction *)

and print_E_And state (node : bool_and bin_op reg) =
  print_bin_op state node

(* Application to data constructors and functions *)

and print_E_App state (node : (expr * expr Utils.nseq) reg) =
  let fun_or_ctor, args = node.value in
  let args = print_nseq (print_expr state) args in
  group (print_expr state fun_or_ctor
         ^^ nest state#indent (break 1 ^^ args))

(* Mutable assignment expressions *)

and print_E_Assign state (node : assign reg) =
  let {binder; ass; expr} = node.value in
  prefix state#indent 1
    (print_variable binder ^^ space ^^ token ass)
    (print_expr state expr)

(* Attributes expressions *)

and print_E_Attr state (node : attribute * expr) =
  let attributes, expr = unroll_E_Attr node in
  let thread = print_expr state expr in
  print_attributes state thread attributes

(* Bytes expressions *)

and print_E_Bytes (node : (lexeme * Hex.t) wrap) =
  print_bytes node

(* String concatenation *)

and print_E_Cat state (node : caret bin_op reg) =
  print_bin_op state node

(* Code injection *)

and print_E_CodeInj state (node : code_inj reg) =
  let {language; code; rbracket} = node.value in
  let lang = string language#payload.value
  and code = print_expr state code in
  group (print_comments language#comments ^/^
         lbracket ^^ percent ^^ lang ^/^ code ^^ token rbracket)

(* Conditional expression *)

and print_E_Cond state (node : cond_expr reg) =
  let {kwd_if; test; kwd_then; if_so; if_not} = node.value in
  let test  = token kwd_if ^^ space
              ^^ group (nest (1 + state#indent) (print_expr state test))
  and if_so = token kwd_then
              ^^ group (nest state#indent
                          (break 1 ^^ print_expr state if_so))
  in match if_not with
    Some (kwd_else, expr) ->
      let if_not =
        token kwd_else
        ^^ group (nest state#indent (break 1 ^^ print_expr state expr))
      in test ^/^ if_so ^/^ if_not
  | None -> test ^/^ if_so

(* Consing expression *)

and print_E_Cons state (node : cons bin_op reg) = print_bin_op state node

and print_E_ContractOf (node : (module_name, dot) Utils.nsepseq reg) =
  let path = print_nsepseq (break 0) token node.value in
  string "contract_of" ^^ space ^^ group (nest 0 (break 1 ^^ path))

(* Constructor in expressions *)

and print_E_Ctor (node : ctor) = token node

(* Arithmetic division *)

and print_E_Div state (node : slash bin_op reg) = print_bin_op state node

(* Equality *)

and print_E_Equal state (node : equal bin_op reg) = print_bin_op state node

(* "false" as expression *)

and print_E_False (node : kwd_false) = token node

(* For loop *)

and print_E_For state (node : for_loop reg) =
  let {kwd_for; index; equal; bound1;
       direction; bound2; body} = node.value in
  token kwd_for ^^ space
  ^^ print_variable index
  ^^ space ^^ token equal ^^ space
  ^^ print_expr state bound1 ^^ space
  ^^ print_direction direction ^^ space
  ^^ print_expr state bound2  ^^ space
  ^^ print_loop_body state body

and print_direction = function
  Upto kwd_upto -> token kwd_upto
| Downto kwd_downto -> token kwd_downto

and print_loop_body state (node : loop_body reg) =
  let {kwd_do; seq_expr; kwd_done} = node.value in
  let seq_expr = print_sepseq hardline (print_expr state) seq_expr in
  token kwd_do
  ^^ nest state#indent (hardline ^^ seq_expr) ^^ hardline
  ^^ token kwd_done

(* ForIn loop *)

and print_E_ForIn state (node : for_in_loop reg) =
  let {kwd_for; pattern; kwd_in; collection; body} = node.value in
  token kwd_for ^^ space
  ^^ print_pattern state pattern
  ^^ space ^^ token kwd_in ^^ space
  ^^ print_expr state collection
  ^^ space ^^ print_loop_body state body

(* Function expressions *)

and print_E_Fun state (node : fun_expr reg) =
  let {kwd_fun; type_params; binders; rhs_type; arrow; body} = node.value in
  let thread  = token kwd_fun in
  let thread  = print_type_params thread type_params in
  let thread  = thread ^^ space
                ^^ nest state#indent
                        (print_nseq (print_pattern state) binders) in
  let thread  = print_opt_type state thread rhs_type in
  group (thread ^^ space ^^ token arrow ^^ space
         ^^ nest state#indent (print_expr state body))

(* Greater or equal than *)

and print_E_Geq state (node : geq bin_op reg) = print_bin_op state node

(* Greater than *)

and print_E_Gt state (node : gt bin_op reg) = print_bin_op state node

(* Integers *)

and print_E_Int (node : (lexeme * Z.t) wrap) = print_int node

(* Bitwise conjunction *)

and print_E_Land state (node : kwd_land bin_op reg) = print_bin_op state node

(* Lower or equal than *)

and print_E_Leq state (node : leq bin_op reg) = print_bin_op state node

(* Local value definition *)

and print_E_LetIn state (node : let_in reg) =
  let {kwd_let; kwd_rec; binding; kwd_in; body} = node.value in
  let let_str =
    match kwd_rec with
      None -> token kwd_let
    | Some kwd_rec -> token kwd_let ^^ space ^^ token kwd_rec
  in let_str ^^ space ^^ print_let_binding state binding.value ^^ space
     ^^ token kwd_in ^^ hardline ^^ group (print_expr state body)

(* Mutable value definition *)

and print_E_LetMutIn state (node : let_mut_in reg) =
  let {kwd_let; kwd_mut; binding; kwd_in; body} = node.value in
  let let_str = token kwd_let ^^ space ^^ token kwd_mut ^^ space
  in let_str ^^ print_let_binding state binding.value
     ^^ space ^^ token kwd_in ^^ hardline ^^ group (print_expr state body)

(* List expressions *)

and print_E_List state (node : expr list_) =
  print_list state (print_expr state) node

(* Bitwise disjunction *)

and print_E_Lor state (node : kwd_lor bin_op reg) = print_bin_op state node

(* Bitwise left shift *)

and print_E_Lsl state (node : kwd_lsl bin_op reg) = print_bin_op state node

(* Bitwise right shift *)

and print_E_Lsr state (node : kwd_lsr bin_op reg) = print_bin_op state node

(* Lower than *)

and print_E_Lt state (node : lt bin_op reg) = print_bin_op state node

(* Bitwise exclusive disjunction *)

and print_E_Lxor state (node : kwd_lxor bin_op reg) = print_bin_op state node

(* Pattern matching *)

and print_E_Match state (node : match_expr reg) =
  let {kwd_match; subject; kwd_with; lead_vbar=_; clauses} = node.value in
  group (token kwd_match
         ^^ space ^^ nest state#indent (print_expr state subject)
         ^/^ token kwd_with)
  ^^ hardline ^^ print_clauses state clauses

and print_clauses state (node : (match_clause reg, vbar) Utils.nsepseq reg) =
  let head, tail = node.value in
  let head       = print_clause state head in
  let head       = if List.is_empty tail then head
                   else blank state#indent ^^ head in
  let rest       = List.map ~f:snd tail in
  let app clause = break 1 ^^ bar ^^ space ^^ print_clause state clause
  in  head ^^ concat_map app rest

and print_clause state (node: match_clause reg) =
  let {pattern; arrow; rhs} = node.value in
  let offset = state#indent * 2 in
  print_pattern state pattern
  ^^ prefix offset 1 (space ^^ token arrow) (print_expr state rhs)

(* Arithmethic modulo *)

and print_E_Mod state (node : kwd_mod bin_op reg) = print_bin_op state node

(* Local module definition *)

and print_E_ModIn state (node : module_in reg) =
  let {mod_decl; kwd_in; body} = node.value in
  let {kwd_module; name; eq; module_expr; annotation = _} = mod_decl.value
  in group (token kwd_module ^^ space
            ^^ token name ^^ space ^^ token eq ^^ space
            ^^ print_module_expr state module_expr
            ^^ space ^^ token kwd_in ^^ hardline ^^ print_expr state body)

(* Module paths *)

and print_E_ModPath state (node : expr module_path reg) =
  print_module_path (print_expr state) node

(* Multiplication *)

and print_E_Mult state (node : times bin_op reg) = print_bin_op state node

(* Mutez as an expression *)

and print_E_Mutez (node : (lexeme * Int64.t) wrap) =
  print_mutez node

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

and print_E_Not state (node : kwd_not un_op reg) = print_un_op state node

(* Logical disjunction *)

and print_E_Or state (node : kwd_or bin_op reg) = print_bin_op state node

(* Parenthesised expression *)

and print_E_Par state (node : expr par) =
  print_par (print_expr state) node

(* Projection *)

and print_E_Proj state (node : projection reg) = print_projection state node

and print_projection state (node : projection reg) =
  let {record_or_tuple; selector; field_path} = node.value in
  let record_or_tuple = print_expr state record_or_tuple
  and field_path      = print_nsepseq (break 0) print_selection field_path
  in group (record_or_tuple ^^ token selector ^^ break 0 ^^ field_path)

and print_selection = function
  FieldName name -> print_variable name
| Component cmp  ->
    let prefix = print_comments cmp#comments
                 ^/^ (cmp#payload |> snd |> Z.to_string |> string)
    in print_line_comment_opt prefix cmp#line_comment

(* Record expression *)

and print_E_Record state (node : record_expr) =
  print_record (print_field_expr state) node

and print_field_expr state (node : (field_name, equal, expr) field) =
  print_field
    state
    ~lhs:print_variable
    ~lens:token
    ~rhs:(print_expr state)
    node

(* String expression *)

and print_E_String (node : lexeme wrap) = print_string node

(* Arithmetic subtraction *)

and print_E_Sub state (node : minus bin_op reg) = print_bin_op state node

(* "true" as expression *)

and print_E_True (node : kwd_true) = token node

(* Tuple expression *)

and print_E_Tuple state (node : expr tuple reg) =
  print_nsepseq (break 1) (print_expr state) node.value

(* Typed expression *)

and print_E_Typed state (node : typed_expr par) =
  let print (expr, type_annot) =
    print_type_annotation state (print_expr state expr) type_annot
  in print_par print node

(* Local type definition *)

and print_E_TypeIn state (node : type_in reg) =
  let {type_decl; kwd_in; body} = node.value in
  print_type_decl state type_decl.value
  ^^ space ^^ token kwd_in ^^ hardline
  ^^ group (print_expr state body)

(* Unit expression *)

and print_E_Unit (node : the_unit reg) =
  let lpar, rpar = node.value
  in token lpar ^^ token rpar

(* Functional update of records *)

and print_E_Update state (node : update_expr braces) =
  let print (node : update_expr) =
    let {record; kwd_with; updates} = node in
    let print_field = print_field_path_assign state in
    let updates     = print_nsepseq (break 1) print_field updates
    and record      = print_expr state record
    in record ^^ space ^^ token kwd_with
       ^^ nest state#indent (break 1 ^^ updates)
  in group (print_braces ~force_hardline:false state print node)

and print_field_path_assign state (node : (path, lens, expr) field) =
  print_field
    state
    ~lhs:(print_path state)
    ~lens:print_lens
    ~rhs:(print_expr state)
    node

and print_path state = function
  Name p -> print_variable         p
| Path p -> print_projection state p

and print_lens = function
  Lens_Id   l
| Lens_Add  l
| Lens_Sub  l
| Lens_Mult l
| Lens_Div  l
| Lens_Fun  l -> token l

(* Expression variable *)

and print_E_Var (node : variable) = print_variable node

(* Verbatim string expressions *)

and print_E_Verbatim (node : lexeme wrap) = print_verbatim node

(* While loop *)

and print_E_While state (node : while_loop reg) =
  let {kwd_while; cond; body} = node.value in
  token kwd_while ^^ space ^^ print_expr state cond
  ^^ space ^^ print_loop_body state body

(* Sequence expressions *)

and print_E_Seq state (node : sequence_expr reg) =
  let {compound; elements} = node.value in
  let elements = print_sepseq hardline (print_expr state) elements in
  match compound with
    None -> elements
  | Some BeginEnd (kwd_begin, kwd_end) ->
      token kwd_begin
      ^^ nest state#indent (hardline ^^ elements) ^^ hardline
      ^^ token kwd_end
  | Some Parens (lpar, rpar) ->
      token lpar
      ^^ nest state#indent (hardline ^^ elements) ^^ hardline
      ^^ token rpar

(* Reverse application *)

and print_E_RevApp state (node : rev_app bin_op reg) =
  print_bin_op state node

(* EXPORTS *)

let print_type_expr   = print_type_expr
let print_pattern     = print_pattern
let print_expr        = print_expr
let print_declaration = print_declaration

type cst         = CST.t
type expr        = CST.expr
type type_expr   = CST.type_expr
type pattern     = CST.pattern
type declaration = CST.declaration
type signature_expr = CST.signature_expr
