(* PRINTING THE CST *)

(* This module produces an arborescent, textual representation of a
   subset of the Concrete Abstract Tree (CST). It aims at a readable
   format with the most relevant nodes, with source locations. This
   functionality is most useful when testing the parser, for example,
   checking that a particular node corresponding to an operator has
   the expected associativity with the same kind, or the expected
   priority over another. *)

[@@@coverage exclude_file]

(* Vendor dependencies *)

module Directive = Preprocessor.Directive
module Utils     = Simple_utils.Utils
module Region    = Simple_utils.Region

(* Internal dependencies *)

module Attr = Lexing_shared.Attr
module Tree = Cst_shared.Tree

open CST (* THE ONLY GLOBAL OPENING *)

(* UTILITIES *)

type ('a, 'sep) nsepseq = ('a, 'sep) Utils.nsepseq
type 'a nseq = 'a Utils.nseq

let print_attribute state (node : Attr.t wrap) =
  let key, val_opt = node#payload in
  match val_opt with
    None ->
      Tree.(make_unary state "<attribute>" make_node key)
  | Some String value ->
      let children = Tree.[
        mk_child make_node key;
        mk_child make_node (Printf.sprintf "%S" value)]
      in Tree.make state "<attribute>" children
  | Some Ident value ->
      let children = Tree.[
        mk_child make_node key;
        mk_child make_node value]
      in Tree.make state "<attribute>" children

let mk_children_attr (node : Attr.t wrap list) =
  Tree.mk_children_list print_attribute ~root:"<attributes>" node

(* PRINTING THE CST *)

let rec print_cst state (node : cst) =
  Tree.of_nseq state "<cst>" print_declaration node.decl

(* DECLARATIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too. *)

and print_declaration state = function
  D_Attr      d -> print_D_Attr      state d
| D_Directive d -> print_D_Directive state d
| D_Let       d -> print_D_Let       state d
| D_Module    d -> print_D_Module    state d
| D_Type      d -> print_D_Type      state d
| D_Signature d -> print_D_Signature state d

(* SIGNATURE DECLARATIONS *)

and print_sig_item state = function
  S_Attr     d -> print_S_Attr       state d
| S_Value    d -> print_S_Value      state d
| S_Type     d -> print_S_Type       state d
| S_Type_var d -> print_S_Type_var state d

(* Attributed declaration *)

and print_D_Attr state (node : (attribute * declaration) reg) =
  let attribute, declaration = node.value in
  let children = Tree.[
    mk_child print_attribute   attribute;
    mk_child print_declaration declaration]
  in Tree.make state "D_Attr" children

(* Attributed sig. item *)

and print_S_Attr state (node : (attribute * sig_item) reg) =
  let attribute, sig_item = node.value in
  let children = Tree.[
    mk_child print_attribute   attribute;
    mk_child print_sig_item     sig_item]
  in Tree.make state "S_Attr" children

(* Preprocessing directives *)

and print_D_Directive state (node : Directive.t) =
  let region, string = Directive.project node in
  Tree.(make_unary state "D_Directive" make_node ~region string)

(* Non-recursive, top-level values *)

and print_D_Let state (node : let_decl reg) =
  let Region.{value; region} = node in
  let _kwd_let, kwd_rec_opt, let_binding = value in
  let children = Tree.(mk_child_opt make_literal kwd_rec_opt)
                 :: mk_children_binding let_binding
  in Tree.make ~region state "D_Let" children

and mk_children_binding (node : let_binding) =
  let {type_params; binders; rhs_type; let_rhs; _} = node in
  Tree.[mk_child     print_binders         binders;
        mk_child_opt print_type_params     type_params;
        mk_child_opt print_type_annotation rhs_type;
        mk_child     print_expr            let_rhs]

and print_binders state (node : pattern nseq) =
  Tree.of_nseq state "<binders>" print_pattern node

and print_type_params state (node : type_params par) =
  let nseq = snd node.value.inside in
  Tree.(of_nseq state "<type parameters>" make_literal nseq)

and print_type_annotation state (_, type_expr) =
  Tree.make_unary state "<type>" print_type_expr type_expr

(* Type declaration *)

and print_D_Type state (node : type_decl reg) =
  let Region.{value; region} = node in
  Tree.make ~region state "D_Type" @@ mk_children_type_decl value

and mk_children_type_decl (node : type_decl) =
  let {name; params; type_expr; _} = node
  in
  let print_TV_Single state (node : type_var) =
    Tree.make_unary state "TV_Single" print_type_var node

  and print_TV_Tuple state (node : type_var tuple par) =
      let Region.{value; region} = node in
      Tree.of_nsepseq state ~region "TV_Tuple" print_type_var value.inside
  in
  let print_type_vars state = function
    TV_Single tv -> print_TV_Single state tv
  | TV_Tuple  tv -> print_TV_Tuple  state tv
  in
  Tree.[mk_child     make_literal    name;
        mk_child_opt print_type_vars params;
        mk_child     print_type_expr type_expr]

and print_type_var state (node : type_var) =
  let Region.{value; region} = node in
  let _, var = value in (* We don't print the backquote, if any. *)
  Tree.make_node ~region state var#payload

(* Module declaration *)

and print_D_Module state (node : module_decl reg) =
  let Region.{value; region} = node in
  let children = mk_children_module_decl value
  in Tree.make state ~region "D_Module" children

and mk_children_module_decl (node : module_decl) =
  Tree.[mk_child make_literal      node.name;
        mk_child print_module_expr node.module_expr]

and print_module_expr state = function
  M_Body       e -> print_M_Body       state e
| M_Path       e -> print_M_Path       state e
| M_Var        e -> print_M_Var        state e

and print_M_Body state (node : module_body reg) =
  let Region.{value; region} = node in
  let decl = value.declarations in
  let children = Tree.mk_children_list print_declaration decl
  in Tree.make ~region state "M_Body" children

and print_M_Path state (node : module_name module_path reg) =
  print_module_path Tree.make_literal "M_Path" state node

and print_M_Var state (node : module_name) =
  Tree.(make_unary state "M_Var" make_literal node)

and print_module_path :
  'a.'a Tree.printer -> Tree.root -> Tree.state -> 'a module_path reg -> unit =
  fun print root state {value; region} ->
    let children =
      (List.map ~f:Tree.(mk_child make_literal)
       @@ Utils.nsepseq_to_list value.module_path)
      @ [Tree.mk_child print value.field]
    in Tree.make state root ~region children

(* Signature declaration *)

and print_D_Signature state (node : signature_decl reg) =
  let Region.{value; region} = node in
  let children = mk_children_signature_decl value
  in Tree.make state ~region "D_Signature" children

and mk_children_signature_decl (node : signature_decl) =
  Tree.[mk_child make_literal      node.name;
        mk_child print_signature_expr node.signature_expr]

and print_signature_expr state = function
  S_Sig  e -> print_S_Sig  state e
| S_Path e -> print_S_Path state e
| S_Var  e -> print_S_Var  state e

and print_S_Sig state (node : signature_body reg) =
  let Region.{value; region} = node in
  let sig_items = value.sig_items in
  let children = Tree.mk_children_list print_sig_item sig_items
  in Tree.make ~region state "S_Sig" children

and print_S_Path state (node : module_name module_path reg) =
  print_module_path Tree.make_literal "S_Path" state node

and print_S_Var state (node : module_name) =
  Tree.(make_unary state "S_Var" make_literal node)

(* Value declarations (signature) *)

and print_S_Value state (node : (kwd_val * variable * colon * type_expr) reg) =
  let Region.{value; region} = node in
  let _kwd_val, var, _colon, type_expr = value in
  let children = Tree.[mk_child make_literal var;
                       mk_child print_type_expr type_expr]
  in Tree.make ~region state "S_Value" children

(* Type declarations (signature) *)

and print_S_Type state (node : (kwd_type * variable * equal * type_expr) reg) =
  let Region.{value; region} = node in
  let _kwd_type, var, _eq, type_expr = value in
  let children = Tree.[mk_child make_literal var;
                       mk_child print_type_expr type_expr]
  in Tree.make ~region state "S_Type" children

(* Type declarations (signature) *)

and print_S_Type_var state (node : (kwd_type * variable) reg) =
  let Region.{value; region} = node in
  let _kwd_type, var = value in
  let children = Tree.[mk_child make_literal var]
  in Tree.make ~region state "S_Type_var" children

(* TYPE EXPRESSIONS *)

and print_type_expr state = function
  T_App       t -> print_T_App       state t
| T_Arg       t -> print_T_Arg       state t
| T_Attr      t -> print_T_Attr      state t
| T_Cart      t -> print_T_Cart      state t
| T_Fun       t -> print_T_Fun       state t
| T_Int       t -> print_T_Int       state t
| T_ModPath   t -> print_T_ModPath   state t
| T_Par       t -> print_T_Par       state t
| T_Record    t -> print_T_Record    state t
| T_String    t -> print_T_String    state t
| T_Variant   t -> print_T_Variant   state t
| T_Var       t -> print_T_Var       state t
| T_Parameter t -> print_T_Parameter state t

(* Constructor application *)

and print_T_App state (node : (type_expr * type_ctor_arg) reg) =
  let Region.{value; region} = node in
  let type_expr, arg = value in
  let children = Tree.[
    mk_child print_type_expr type_expr;
    mk_child print_type_ctor_arg arg]
  in Tree.make state ~region "T_App" children

and print_type_ctor_arg state = function
  TC_Single t ->
    Tree.make_unary state "<argument>" print_type_expr t
| TC_Tuple t ->
    let Region.{value; region} = t in
    Tree.of_nsepseq ~region state "<arguments>" print_type_expr value.inside

(* Type variable *)

and print_T_Arg state (node : type_var) =
  let Region.{value; region} = node in
  Tree.(make_unary ~region state "T_Arg" make_literal (snd value))

(* Attributed type expression *)

and print_T_Attr state (node : attribute * type_expr) =
  let attribute, type_expr = node in
  let children = Tree.[
    mk_child print_attribute attribute;
    mk_child print_type_expr type_expr]
  in Tree.make state "T_Attr"children

(* Cartesian products *)

and print_T_Cart state (node : cartesian) =
  let Region.{value; region} = node in
  let first, sep, others = value in
  let seq = Utils.nsepseq_cons first sep others in
  Tree.of_nsepseq ~region state "T_Cart" print_type_expr seq

(* Functional types *)

and print_T_Fun state (node : (type_expr * arrow * type_expr) reg) =
  let Region.{value; region} = node in
  let domain, _, codomain = value in
  let children = Tree.[
    mk_child print_type_expr domain;
    mk_child print_type_expr codomain]
  in Tree.make state "T_Fun" ~region children

(* The integer type *)

and print_T_Int state (node : (lexeme * Z.t) wrap) =
  Tree.make_int "T_Int" state node

(* Module paths in type expressions *)

and print_T_ModPath state (node : type_expr module_path reg) =
  print_module_path print_type_expr "T_ModPath" state node

(* Parenthesised type expressions *)

and print_T_Par state (node : type_expr par) =
  Tree.make_unary state "T_Par" print_type_expr node.value.inside

(* Record types *)

and print_T_Record state (node : field_decl reg record) =
  let Region.{value; region} = node in
  Tree.of_sepseq ~region state "T_Record" print_field_decl value.inside

and print_field_decl state (node : field_decl reg) =
  let Region.{value; region} = node in
  let {attributes; field_name; field_type} = value in
  let children = Tree.[
    mk_child     make_literal          field_name;
    mk_child_opt print_type_annotation field_type]
  @ mk_children_attr attributes
  in Tree.make ~region state "<field>" children

(* Type string *)

and print_T_String state (node : lexeme wrap) =
  Tree.(make_unary state "T_String" make_verbatim node)

(* Variant types *)

and print_T_Variant state (node : variant_type reg) =
  let Region.{value; region} = node in
  Tree.of_nsepseq ~region state "T_Variant" print_variant value.variants

and print_variant state (node : variant reg) =
  let Region.{value; region} = node in
  let {attributes; ctor; ctor_args} = value in
  let children = Tree.mk_child_opt print_of_type_expr ctor_args
                 :: mk_children_attr attributes in
  let root = ctor#payload in
  Tree.make ~region state root children

and print_of_type_expr state (_, type_expr) =
  print_type_expr state type_expr

(* Type variable *)

and print_T_Var state (node : variable) =
  Tree.(make_unary state "T_Var" make_literal node)

(* Type parameter *)

and print_T_Parameter state (node : (module_name, dot) nsepseq reg) =
  let Region.{value; region} = node in
  Tree.(of_nsepseq state ~region "T_Parameter" make_literal value)

(* PATTERNS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_pattern state = function
  P_App      p -> print_P_App      state p
| P_Attr     p -> print_P_Attr     state p
| P_Bytes    p -> print_P_Bytes    state p
| P_Cons     p -> print_P_Cons     state p
| P_Ctor     p -> print_P_Ctor     state p
| P_Int      p -> print_P_Int      state p
| P_List     p -> print_P_List     state p
| P_ModPath  p -> print_P_ModPath  state p
| P_Mutez    p -> print_P_Mutez    state p
| P_Nat      p -> print_P_Nat      state p
| P_Par      p -> print_P_Par      state p
| P_Record   p -> print_P_Record   state p
| P_String   p -> print_P_String   state p
| P_Tuple    p -> print_P_Tuple    state p
| P_Typed    p -> print_P_Typed    state p
| P_Var      p -> print_P_Var      state p
| P_Verbatim p -> print_P_Verbatim state p
| P_Unit     p -> print_P_Unit     state p

(* A constructor application (or constant constructor) in patterns *)

and print_P_App state (node : (pattern * pattern option) reg) =
  let Region.{value; region} = node in
  let ctor_pattern, ctor_arg_pattern = value in
  let children = Tree.[
    mk_child     print_pattern ctor_pattern;
    mk_child_opt print_pattern ctor_arg_pattern]
  in Tree.make state "P_App" ~region children

(* Attributes patterns *)

and print_P_Attr state (node : attribute * pattern) =
  let attribute, pattern = node in
  let children = Tree.[
    mk_child print_attribute attribute;
    mk_child print_pattern   pattern]
  in Tree.make state "P_Attr" children

(* Bytes as literals in patterns *)

and print_P_Bytes state (node : (lexeme * Hex.t) wrap) =
  Tree.make_bytes "P_Bytes" state node

(* List consing in patterns *)

and print_P_Cons state (node : (pattern * cons * pattern) reg) =
  let Region.{value; region} = node in
  let head, _, tail = value in
  let children = Tree.[
    mk_child print_pattern head;
    mk_child print_pattern tail]
  in Tree.make state "P_Cons" ~region children

(* Data constructors as patterns *)

and print_P_Ctor state (node : ctor) =
  let region = node#region in
  Tree.(make_unary ~region state "P_Ctor" make_literal node)

(* Integers in patterns *)

and print_P_Int state (node : (lexeme * Z.t) wrap) =
  Tree.make_int "P_Int" state node

(* Patterns of lists by extension *)

and print_P_List state (node : pattern list_) =
  let Region.{value; region} = node in
  let children = List.map ~f:(Tree.mk_child print_pattern)
                 @@ Utils.sepseq_to_list value.inside
  in Tree.make state "P_List" ~region children

(* Qualified patterns *)

and print_P_ModPath state (node : pattern module_path reg) =
  print_module_path print_pattern "P_ModPath" state node

(* Mutez in patterns *)

and print_P_Mutez state (node : (lexeme * Int64.t) wrap) =
  Tree.make_mutez "P_Mutez" state node

(* Natural numbers in patterns *)

and print_P_Nat state (node : (lexeme * Z.t) wrap) =
  Tree.make_int "P_Nat" state node

(* Parenthesised patterns *)

and print_P_Par state (node : pattern par) =
  Tree.make_unary state "P_Par" print_pattern node.value.inside

(* Record patterns *)

and print_P_Record state (node : record_pattern) =
  print_record print_field_pattern "P_Record" state node

and print_field_pattern state (node : (field_name, equal, pattern) field) =
  let print_lhs state =
    Tree.(make_unary state "<lhs>" make_literal)
  and print_rhs state =
    Tree.make_unary state "<rhs>" print_pattern
  in print_field
       state ~lhs:print_lhs ~lens:Tree.make_literal ~rhs:print_rhs node

and print_record :
  'a.'a Tree.printer -> Tree.root -> Tree.state -> 'a record -> unit =
  fun print root state node ->
    let Region.{region; value} = node in
    let children = List.map ~f:(Tree.mk_child print)
                   @@ Utils.sepseq_to_list value.inside
    in Tree.make state root ~region children

and print_field :
  'lhs 'lens 'rhs.Tree.state ->
  lhs:'lhs Tree.printer ->
  lens:'lens Tree.printer ->
  rhs:'rhs Tree.printer ->
  ('lhs, 'lens, 'rhs) field ->
  unit =
  fun state ~lhs:print_lhs ~lens:print_lens ~rhs:print_rhs ->
    function
      Punned Region.{value; region} ->
        let {pun; attributes} = value in
        let children = Tree.mk_child print_lhs pun
                       :: mk_children_attr attributes
        in Tree.make ~region state "<punned field>" children
    | Complete Region.{value; region} ->
        let {field_lhs; field_lens; field_rhs; attributes} = value in
        let children = Tree.[
          mk_child print_lhs  field_lhs;
          mk_child print_lens field_lens;
          mk_child print_rhs  field_rhs]
        @ mk_children_attr attributes
        in Tree.make ~region state "<field>" children

and print_update_lens state = function
  Lens_Id   l -> Tree.(make_unary state "Lens_Id"   make_literal l)
| Lens_Add  l -> Tree.(make_unary state "Lens_Add"  make_literal l)
| Lens_Sub  l -> Tree.(make_unary state "Lens_Sub"  make_literal l)
| Lens_Mult l -> Tree.(make_unary state "Lens_Mult" make_literal l)
| Lens_Div  l -> Tree.(make_unary state "Lens_Div"  make_literal l)
| Lens_Fun  l -> Tree.(make_unary state "Lens_Fun"  make_literal l)

(* String literals as patterns *)

and print_P_String state (node : lexeme wrap) =
  Tree.(make_unary state "P_String" make_string node)

(* The pattern matching a tuple *)

and print_P_Tuple state (node : pattern tuple reg) =
  let Region.{value; region} = node in
  Tree.of_nsepseq state ~region "P_Tuple" print_pattern value

(* Typed pattern *)

and print_P_Typed state (node : typed_pattern reg) =
  let Region.{value; region} = node in
  let pattern, type_annot = value in
  let children = Tree.[
    mk_child print_pattern         pattern;
    mk_child print_type_annotation type_annot]
  in Tree.make state "P_Typed" ~region children

(* A pattern variable *)

and print_P_Var state (node : variable) =
  Tree.(make_unary state "P_Var" make_literal node)

(* A verbatim string *)

and print_P_Verbatim state (node : lexeme wrap) =
  Tree.(make_unary state "P_Verbatim" make_string node)

(* Unit pattern *)

and print_P_Unit state (node : the_unit reg) =
  let Region.{region; _} = node in
  Tree.make_node ~region state "P_Unit"

(* EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too. *)

and print_expr state = function
  E_Add      e -> print_E_Add      state e
| E_And      e -> print_E_And      state e
| E_App      e -> print_E_App      state e
| E_Assign   e -> print_E_Assign   state e
| E_Attr     e -> print_E_Attr     state e
| E_Bytes    e -> print_E_Bytes    state e
| E_Cat      e -> print_E_Cat      state e
| E_CodeInj  e -> print_E_CodeInj  state e
| E_Cond     e -> print_E_Cond     state e
| E_Contract e -> print_E_Contract state e
| E_Ctor     e -> print_E_Ctor     state e
| E_Cons     e -> print_E_Cons     state e
| E_Div      e -> print_E_Div      state e
| E_Equal    e -> print_E_Equal    state e
| E_For      e -> print_E_For      state e
| E_ForIn    e -> print_E_ForIn    state e
| E_Fun      e -> print_E_Fun      state e
| E_Geq      e -> print_E_Geq      state e
| E_Gt       e -> print_E_Gt       state e
| E_Int      e -> print_E_Int      state e
| E_Land     e -> print_E_Land     state e
| E_Leq      e -> print_E_Leq      state e
| E_LetIn    e -> print_E_LetIn    state e
| E_LetMutIn e -> print_E_LetMutIn state e
| E_List     e -> print_E_List     state e
| E_Lor      e -> print_E_Lor      state e
| E_Lsl      e -> print_E_Lsl      state e
| E_Lsr      e -> print_E_Lsr      state e
| E_Lt       e -> print_E_Lt       state e
| E_Lxor     e -> print_E_Lxor     state e
| E_Match    e -> print_E_Match    state e
| E_Mod      e -> print_E_Mod      state e
| E_ModIn    e -> print_E_ModIn    state e
| E_ModPath  e -> print_E_ModPath  state e
| E_Mult     e -> print_E_Mult     state e
| E_Mutez    e -> print_E_Mutez    state e
| E_Nat      e -> print_E_Nat      state e
| E_Neg      e -> print_E_Neg      state e
| E_Neq      e -> print_E_Neq      state e
| E_Not      e -> print_E_Not      state e
| E_Or       e -> print_E_Or       state e
| E_Par      e -> print_E_Par      state e
| E_Proj     e -> print_E_Proj     state e
| E_Record   e -> print_E_Record   state e
| E_String   e -> print_E_String   state e
| E_Sub      e -> print_E_Sub      state e
| E_Tuple    e -> print_E_Tuple    state e
| E_Typed    e -> print_E_Typed    state e
| E_TypeIn   e -> print_E_TypeIn   state e
| E_Unit     e -> print_E_Unit     state e
| E_Update   e -> print_E_Update   state e
| E_Var      e -> print_E_Var      state e
| E_Verbatim e -> print_E_Verbatim state e
| E_Seq      e -> print_E_Seq      state e
| E_RevApp   e -> print_E_RevApp   state e
| E_While    e -> print_E_While    state e

(* Arithmetic addition *)

and print_E_Add state (node : plus bin_op reg) =
  print_bin_op state "E_Add" node

and print_bin_op state root (node : 'op bin_op reg) =
  let Region.{value; region} = node in
  let children = Tree.[
    mk_child print_expr value.arg1;
    mk_child print_expr value.arg2]
  in Tree.make state root ~region children

(* Boolean conjunction *)

and print_E_And state (node : bool_and bin_op reg) =
  print_bin_op state "E_And" node

(* Constructor application (or constant constructor) as expressions *)

and print_E_App state (node : (expr * expr nseq) reg) =
  let Region.{value; region} = node in
  let fun_ctor, args = value
  and mk_func state =
    Tree.make_unary state "<fun/ctor>" print_expr
  and mk_args state (node : expr Utils.nseq) =
    Tree.of_nseq state "<arguments>" print_expr node
  in
  let children = Tree.[
    mk_child mk_func fun_ctor;
    mk_child mk_args args]
  in Tree.make state "E_App" ~region children

(* Mutable value assignement *)

and print_E_Assign state (node : assign reg) =
  let Region.{value; region} = node in
  let {binder; expr; _} = value in
  let children =
    Tree.(mk_child make_literal binder) :: [Tree.mk_child print_expr expr] in
  Tree.make ~region state "E_Assign" children

(* Attributed expressions *)

and print_E_Attr state (node : attribute * expr) =
  let attribute, expr = node in
  let children = Tree.[
    mk_child print_attribute attribute;
    mk_child print_expr      expr]
  in Tree.make state "E_Attr" children

(* Bytes as expressions *)

and print_E_Bytes state (node : (lexeme * Hex.t) wrap) =
  Tree.make_bytes "E_Bytes" state node

(* String catenation *)

and print_E_Cat state (node : caret bin_op reg) =
  print_bin_op state "E_Cat" node

(* Code Injection *)

and print_E_CodeInj state (node : code_inj reg) =
  let Region.{value; region} = node in
  let children = Tree.[
    mk_child print_language value.language;
    mk_child print_code     value.code]
  in Tree.make state "E_CodeInj" ~region children

and print_language state (node : language) =
  Tree.(make_unary state "<language>" make_node node#payload.value)

and print_code state (node : expr) =
  Tree.make_unary state "<code>" print_expr node

(* Contract of expression *)

and print_E_Contract state (node : (module_name, dot) nsepseq reg) =
  let Region.{region; value} = node in
  Tree.(of_nsepseq ~region state "E_Contract" make_literal value)

(* Conditional expressions *)

and print_E_Cond state (node : cond_expr reg) =
  let Region.{value; region} = node in
  let print_cond state =
    Tree.make_unary state "<condition>" print_expr
  and print_then state =
    Tree.make_unary state "<true>" print_expr
  and print_else state (_, if_not) =
    Tree.make_unary state "<false>" print_expr if_not in
  let children = Tree.[
    mk_child     print_cond value.test;
    mk_child     print_then value.if_so;
    mk_child_opt print_else value.if_not]
  in Tree.make state "E_Cond" ~region children

(* Consing (that is, pushing an item on top of a stack/list) *)

and print_E_Cons state (node : cons bin_op reg) =
  print_bin_op state "E_Cons" node

(* Data constructor as expressions *)

and print_E_Ctor state (node : ctor) =
  let region = node#region in
  Tree.(make_unary ~region state "E_Ctor" make_literal node)

(* The Euclidean quotient *)

and print_E_Div state (node : slash bin_op reg) =
  print_bin_op state "E_Div" node

(* Equality *)

and print_E_Equal state (node : equal bin_op reg) =
  print_bin_op state "E_Equal" node

(* For loops *)

and print_E_For state (node : for_loop reg) =
  let Region.{value; region} = node in
  let {index; bound1; bound2; body; _} = value in
  let children = Tree.[
    mk_child make_literal    index;
    mk_child print_expr      bound1;
    mk_child print_expr      bound2;
    mk_child print_loop_body body] in
  Tree.make ~region state "E_For" children

and print_E_ForIn state (node : for_in_loop reg) =
  let Region.{value; region} = node in
  let {pattern; collection; body; _} = value in
  let children = Tree.[
    mk_child print_pattern   pattern;
    mk_child print_expr      collection;
    mk_child print_loop_body body] in
  Tree.make ~region state "E_ForIn" children

and print_loop_body state (node : loop_body reg) =
  let Region.{value; region} = node in
  let {seq_expr; _} = value in
  Tree.of_sepseq ~region state "<body>" print_expr seq_expr

(* Functional expressions *)

and print_E_Fun state (node : fun_expr reg) =
  let node = node.value in
  let children = Tree.[
    mk_child_opt print_type_params     node.type_params;
    mk_child     print_parameters      node.binders;
    mk_child_opt print_type_annotation node.rhs_type;
    mk_child     print_expr            node.body]
  in Tree.make state "E_Fun" children

and print_parameters state (node : pattern nseq) =
  let children =
    List.map ~f:(Tree.mk_child print_pattern) @@ Utils.nseq_to_list node
  in Tree.make state "<parameters>" children

(* Greater or Equal *)

and print_E_Geq state (node : geq bin_op reg) =
  print_bin_op state "E_Geq" node

(* Greater Than *)

and print_E_Gt state (node : gt bin_op reg) =
  print_bin_op state "E_Gt" node

(* Integer literals as expressions *)

and print_E_Int state (node : (lexeme * Z.t) wrap) =
  Tree.make_int "E_Int" state node

and print_E_Land state (node : kwd_land bin_op reg) =
  print_bin_op state "E_Land" node

(* Lower or Equal *)

and print_E_Leq state (node : leq bin_op reg) =
  print_bin_op state "E_Leq" node

(* Local value definition *)

and print_E_LetIn state (node : let_in reg) =
  let Region.{value; region} = node in
  let {kwd_rec; binding; body; _} = value in
  let binding_children = mk_children_binding binding in
  let children =
    Tree.(mk_child_opt make_literal kwd_rec) :: binding_children
    @ [Tree.mk_child print_body body] in
  Tree.make ~region state "E_LetIn" children

and print_body state (node : expr) =
  Tree.make_unary state "<body>" print_expr node

(* Mutable value definition *)

and print_E_LetMutIn state (node : let_mut_in reg) =
  let Region.{value; region} = node in
  let {binding; body; _} = value in
  let binding_children = mk_children_binding binding in
  let children =
    binding_children @ [Tree.mk_child print_body body] in
  Tree.make ~region state "E_LetMutIn" children

(* Expression lists *)

and print_E_List state (node : expr list_) =
  let Region.{value; region} = node in
  Tree.of_sepseq ~region state "E_List" print_expr value.inside

(* Bitwise disjunction *)

and print_E_Lor state (node : kwd_lor bin_op reg) =
  print_bin_op state "E_Lor" node

(* Bitwise left-shift *)

and print_E_Lsl state (node : kwd_lsl bin_op reg) =
  print_bin_op state "E_Lsl" node

(* Bitwise right-shifrt *)

and print_E_Lsr state (node : kwd_lsr bin_op reg) =
  print_bin_op state "E_Lsr" node

(* Lower Than *)

and print_E_Lt state (node : lt bin_op reg) =
  print_bin_op state "E_lt" node

(* Bitwise exclusive disjunction *)

and print_E_Lxor state (node : kwd_lxor bin_op reg) =
  print_bin_op state "E_Lxor" node

(* Pattern matching *)

and print_E_Match state (node : match_expr reg) =
  let Region.{value; region} = node in
  let {subject; clauses; _} = value in
  let mk_clauses state (node : (match_clause reg, vbar) nsepseq reg) =
    let Region.{region; value} = node in
    Tree.of_nsepseq ~region state "<clauses>" print_match_clause value in
  let children = Tree.[
    mk_child print_expr subject;
    mk_child mk_clauses clauses]
  in Tree.make ~region state "E_Match" children

and print_match_clause state (node : match_clause reg) =
  let Region.{value; region} = node in
  let {pattern; rhs; _} = value in
  let children = Tree.[
    mk_child print_pattern pattern;
    mk_child print_expr    rhs]
  in Tree.make ~region state "<clause>" children

(* Arithmetic modulo *)

and print_E_Mod state (node : kwd_mod bin_op reg) =
  print_bin_op state "E_Mod" node

(* Local module definition *)

and print_E_ModIn state (node : module_in reg) =
  let Region.{value; region} = node in
  let children = mk_children_module_decl value.mod_decl
  in Tree.make state ~region "E_ModIn" children

(* Qualified expression *)

and print_E_ModPath state (node : expr module_path reg) =
  print_module_path print_expr "E_ModPath" state node

(* Multiplication *)

and print_E_Mult state (node : times bin_op reg) =
  print_bin_op state "E_Mult" node

(* Mutez literals *)

and print_E_Mutez state (node : (lexeme * Int64.t) wrap) =
  Tree.make_mutez "E_Mutez" state node

(* Natural numbers *)

and print_E_Nat state (node : (lexeme * Z.t) wrap) =
  Tree.make_nat "E_Nat" state node

(* Arithmetic negation *)

and print_E_Neg state (node : minus un_op reg) =
  print_un_op state "E_Neg" node

and print_un_op state root (node : 'op un_op reg) =
  let Region.{value; region} = node in
  Tree.make_unary state root ~region print_expr value.arg

(* Inequality *)

and print_E_Neq state (node : neq bin_op reg) =
  print_bin_op state "E_Neq" node

(* Logical negation *)

and print_E_Not state (node : kwd_not un_op reg) =
  print_un_op state "E_Not" node

(* Logical disjunction *)

and print_E_Or state (node : kwd_or bin_op reg) =
  print_bin_op state "E_Or" node

(* Parenthesised expressions *)

and print_E_Par state (node : expr par) =
  let Region.{value; region} = node in
  Tree.make_unary state "E_Par" ~region print_expr value.inside

(* Projections *)

and print_E_Proj state (node : projection reg) =
  let Region.{value; region} = node in
  Tree.of_nsepseq state ~region "E_Proj" print_selection value.field_path

and print_selection state = function
  FieldName name -> print_FieldName state name
| Component comp -> print_Component state comp

and print_FieldName state (node : field_name) =
  Tree.(make_unary state "FieldName" make_literal node)

and print_Component state (node : (lexeme * Z.t) wrap) =
  Tree.make_int "Component" state node

(* Record expressions *)

and print_E_Record state (node : record_expr) =
  print_record print_field_expr "E_Record" state node

and print_field_expr state (node : (field_name, equal, expr) field) =
  let print_lhs state =
    Tree.(make_unary state "<lhs>" make_literal)
  and print_rhs state =
    Tree.make_unary state "<rhs>" print_expr
  in print_field
       state ~lhs:print_lhs ~lens:Tree.make_literal ~rhs:print_rhs node

(* String literals *)

and print_E_String state (node : lexeme wrap) =
  Tree.(make_unary state "E_String" make_string node)

(* Arithmetic subtraction *)

and print_E_Sub state (node : minus bin_op reg) =
  print_bin_op state "E_Sub" node

(* Tuple of expression *)

and print_E_Tuple state (node : expr tuple reg) =
  let Region.{value; region} = node in
  Tree.of_nsepseq state ~region "E_Tuple" print_expr value

(* Typed expression *)

and print_E_Typed state (node : typed_expr par) =
  let Region.{value; region} = node in
  let expr, annotation = value.inside in
  let children = Tree.[
    mk_child print_expr expr;
    mk_child print_type_annotation annotation]
  in Tree.make state "E_Typed" ~region children

(* Local type definition *)

and print_E_TypeIn state (node : type_in reg) =
  let Region.{value; region} = node in
  let {type_decl; body; _} = value in
  let children =
    mk_children_type_decl type_decl
    @ [Tree.mk_child print_body body]
  in Tree.make ~region state "E_TypeIn" children

(* Unit value *)

and print_E_Unit state (node : the_unit reg) =
  Tree.make_node ~region:node.region state "E_Unit"

(* Functional updates of records *)

and print_E_Update state (node : update_expr braces) =
  let Region.{value; region} = node in
  let {record; updates; _} = value.inside in
  let print_update state (node : (path, lens, expr) field) =
    print_field
      state ~lhs:print_path ~lens:print_update_lens ~rhs:print_expr node in
  let print_updates state (node : ((path, lens, expr) field, semi) nsepseq) =
    Tree.of_nsepseq state "<updates>" print_update node in
  let children = Tree.[
    mk_child print_expr    record;
    mk_child print_updates updates]
  in Tree.make ~region state "E_Update" children

and print_path state = function
  Name p -> Tree.(make_unary state "Name" make_literal p)
| Path p -> print_Path state p

and print_Path state (node : projection reg) =
  let Region.{value; region} = node in
  Tree.of_nsepseq state ~region "Path" print_selection value.field_path

(* Variables in expressions *)

and print_E_Var state (node : variable) =
  Tree.(make_unary state "E_Var" make_literal node)

(* Verbatim strings *)

and print_E_Verbatim state (node : lexeme wrap) =
  Tree.(make_unary state "E_Verbatim" make_verbatim node)

(* Sequence *)

and print_E_Seq state (node : sequence_expr reg) =
  let Region.{value; region} = node in
  Tree.of_sepseq ~region state "E_Seq" print_expr value.elements

(* Reverse application operator *)

and print_E_RevApp state (node : rev_app bin_op reg) =
  print_bin_op state "E_RevApp" node

(* While loop *)

and print_E_While state (node : while_loop reg) =
  let Region.{value; region} = node in
  let {cond; body; _} = value in
  let children = Tree.[
    mk_child print_expr cond;
    mk_child print_loop_body body] in
  Tree.make ~region state "E_While" children

(* PRINTING (client-slide) *)

type ('src, 'dst) printer = Tree.state -> 'src -> 'dst

let print_to_buffer state cst =
  print_cst state cst; Tree.to_buffer state

let print_to_string state cst =
  Buffer.contents (print_to_buffer state cst)

let print_pattern_to_string state pattern =
  print_pattern state pattern;
  Buffer.contents (Tree.to_buffer state)

(* Aliases *)

let to_buffer = print_to_buffer
let to_string = print_to_string
let pattern_to_string = print_pattern_to_string
