open Lsp_helpers

let all_types : SemanticTokenTypes.t array =
  [| Namespace
   ; Type
   ; Class
   ; Enum
   ; Interface
   ; Struct
   ; TypeParameter
   ; Parameter
   ; Variable
   ; Property
   ; EnumMember
   ; Event
   ; Function
   ; Method
   ; Macro
   ; Keyword
   ; Modifier
   ; Comment
   ; String
   ; Number
   ; Regexp
   ; Operator
   ; Decorator
  |]


let all_modifiers : SemanticTokenModifiers.t array =
  [| Declaration
   ; Definition
   ; Readonly
   ; Static
   ; Deprecated
   ; Abstract
   ; Async
   ; Modification
   ; Documentation
   ; DefaultLibrary
  |]


let mk_type_code : SemanticTokenTypes.t -> int = function
  | Namespace -> 0
  | Type -> 1
  | Class -> 2
  | Enum -> 3
  | Interface -> 4
  | Struct -> 5
  | TypeParameter -> 6
  | Parameter -> 7
  | Variable -> 8
  | Property -> 9
  | EnumMember -> 10
  | Event -> 11
  | Function -> 12
  | Method -> 13
  | Macro -> 14
  | Keyword -> 15
  | Modifier -> 16
  | Comment -> 17
  | String -> 18
  | Number -> 19
  | Regexp -> 20
  | Operator -> 21
  | Decorator -> 22


let mk_modifier_code : SemanticTokenModifiers.t -> int = function
  | Declaration -> 1 lsl 0
  | Definition -> 1 lsl 1
  | Readonly -> 1 lsl 2
  | Static -> 1 lsl 3
  | Deprecated -> 1 lsl 4
  | Abstract -> 1 lsl 5
  | Async -> 1 lsl 6
  | Modification -> 1 lsl 7
  | Documentation -> 1 lsl 8
  | DefaultLibrary -> 1 lsl 9


let mk_type_legend : SemanticTokenTypes.t -> string = function
  | Namespace -> "namespace"
  | Type -> "type"
  | Class -> "class"
  | Enum -> "enum"
  | Interface -> "interface"
  | Struct -> "struct"
  | TypeParameter -> "typeParameter"
  | Parameter -> "parameter"
  | Variable -> "variable"
  | Property -> "property"
  | EnumMember -> "enumMember"
  | Event -> "event"
  | Function -> "function"
  | Method -> "method"
  | Macro -> "macro"
  | Keyword -> "keyword"
  | Modifier -> "modifier"
  | Comment -> "comment"
  | String -> "string"
  | Number -> "number"
  | Regexp -> "regexp"
  | Operator -> "operator"
  | Decorator -> "decorator"


let mk_modifier_legend : SemanticTokenModifiers.t -> string = function
  | Declaration -> "declaration"
  | Definition -> "definition"
  | Readonly -> "readonly"
  | Static -> "static"
  | Deprecated -> "deprecated"
  | Abstract -> "abstract"
  | Async -> "async"
  | Modification -> "modification"
  | Documentation -> "documentation"
  | DefaultLibrary -> "defaultLibrary"


type token = int * int * int * int * int

type 'a wrap_field =
  | Attribute of Lexing_shared.Attr.attribute Simple_utils.Region.reg
  | Comment of Lexing_shared.Wrap.comment
  | Directive of Preprocessor.Directive.t
  | Line_comment of string Simple_utils.Region.reg
  | Payload of 'a Simple_utils.Region.reg

let compare_wrap_fields_regs (type a b) (x : a wrap_field) (y : b wrap_field) : int =
  let get_reg = function
    | Attribute a -> a.region
    | Comment (Block c | Line c) | Line_comment c -> c.region
    | Directive d -> Preprocessor.Directive.to_region d
    | Payload p -> p.region
  in
  Simple_utils.Region.compare (get_reg x) (get_reg y)


let mk_diff (tokens : int array) : unit Handler.t =
  let open Handler in
  let len = Array.length tokens in
  let rec go (i : int) (last_line : int) (last_start_char : int) : unit Handler.t =
    (* Don't change it with [when_]!!!
       Because of strict computations some let-ins (till [let@ () ...] in our case)
       would be evaluated before checking the actual condition. *)
    if i < len
    then (
      let line = tokens.(i) in
      let start_char = tokens.(i + 1) in
      let delta_line = line - last_line in
      let delta_start_char =
        if delta_line = 0 then start_char - last_start_char else start_char - 1
      in
      let length = tokens.(i + 2) in
      let@ () =
        when_ (delta_line < 0 || delta_start_char < 0 || length < 0)
        @@ send_log_msg ~type_:Warning
        @@ Format.asprintf
             "Got negative delta (%d, %d) or length at %d:%d (length %d) with token type \
              %d (modifier %d) at group %d.\n\
              %!"
             delta_line
             delta_start_char
             line
             start_char
             length
             tokens.(i + 3)
             tokens.(i + 4)
             (i / 5)
      in
      tokens.(i) <- delta_line;
      tokens.(i + 1) <- delta_start_char;
      go (i + 5) line start_char)
    else pass
  in
  go 0 0 1


type env =
  | Normal
  | FunApp
  | FunArg
  | FunDef

let semantic_tokens (cst : Dialect_cst.t) (range : Range.t) : int array =
  let data = Vector.create ~dummy:0 in
  let outside (reg : Simple_utils.Region.t) : bool =
    not Range.(intersects range (of_region reg))
  in
  let inside (reg : Simple_utils.Region.t) : bool =
    Range.(inside ~big:range ~small:(of_region reg))
  in
  let mk_tokens
      ?(token_modifiers : SemanticTokenModifiers.t array = [||])
      (token_type : SemanticTokenTypes.t)
      (region : Simple_utils.Region.t)
      : unit
    =
    if (not region#is_ghost) && inside region
    then (
      let start = region#start in
      let stop = region#stop in
      let start_line = start#line - 1 in
      let stop_line = stop#line - 1 in
      (* The client might not support multi-line tokens, so we need to break it
         down into multiple lines. Indeed, VS Code, for example, doesn't. *)
      for line = start_line to stop_line do
        let start_char = if line = start_line then start#column `Point else 1 in
        let length =
          (* We don't know the line length if we are not at the stop line,
             unfortunately. The best thing we can do at this point is to guess a
             number hoping it will be reasonable.
             Warning: Do not use a big number like [Int.pow 2 31 - 1], it will
             freeze the language server on Visual Studio Code. *)
          if line = stop_line then stop#column `Point - start_char else 256
        in
        let type' = mk_type_code token_type in
        let modifiers =
          Array.fold token_modifiers ~init:0 ~f:(fun acc code ->
              acc lor mk_modifier_code code)
        in
        Vector.push data line;
        Vector.push data start_char;
        Vector.push data length;
        Vector.push data type';
        Vector.push data modifiers
      done)
  in
  let mk_tokens_reg
      (type node)
      ?(token_modifiers : SemanticTokenModifiers.t array option)
      (token_type : SemanticTokenTypes.t)
      (node : node Simple_utils.Region.reg)
      : unit
    =
    mk_tokens ?token_modifiers token_type node.region
  in
  let comment : [< Preprocessor.Directive.comment ] -> unit = function
    | `BlockComment c -> mk_tokens_reg Comment c
    | `LineComment c -> mk_tokens_reg Comment c
  in
  let directive : Preprocessor.Directive.t -> unit = function
    (* FIXME: regions overlap *)
    | PP_Include d ->
      List.iter ~f:comment d#previous_comments;
      mk_tokens Macro d#region;
      mk_tokens_reg String d#file_path;
      Option.iter ~f:comment d#trailing_comment
    | PP_Import d ->
      List.iter ~f:comment d#previous_comments;
      mk_tokens Macro d#region;
      mk_tokens_reg String d#file_path;
      mk_tokens_reg String d#module_name;
      Option.iter ~f:comment d#trailing_comment
    | PP_If d | PP_Elif d ->
      List.iter ~f:comment d#previous_comments;
      mk_tokens Macro d#region;
      let rec go : Preprocessor.E_AST.t -> unit = function
        | Or e | And e | Eq e | Neq e ->
          go (fst e.value);
          go (snd e.value)
        | Not e -> go e.value
        | True reg | False reg -> mk_tokens Keyword reg
        | Ident i -> mk_tokens_reg Variable i
        | Parens e -> go e.value
      in
      go d#expression;
      Option.iter ~f:comment d#trailing_comment
    | PP_Else reg | PP_Endif reg -> mk_tokens Macro reg
    | PP_Define sym | PP_Undef sym ->
      mk_tokens Macro sym#region;
      mk_tokens_reg Variable sym#symbol;
      Option.iter ~f:comment sym#trailing_comment
    | PP_Error (reg, sym) ->
      mk_tokens Macro reg;
      mk_tokens_reg Variable sym
    | PP_Linemarker d ->
      mk_tokens Macro d#region;
      mk_tokens_reg String d#file_path;
      mk_tokens_reg Number d#linenum;
      Option.iter ~f:(mk_tokens_reg Number) d#flag
  in
  let mk_tokens_wrap_impl
      (type node)
      ?(token_modifiers : SemanticTokenModifiers.t array option)
      (token_type : SemanticTokenTypes.t option)
      (node : node Lexing_shared.Wrap.t)
      : unit
    =
    let attributes = List.map ~f:(fun a -> Attribute a) node#attributes in
    let comments = List.map ~f:(fun c -> Comment c) node#comments in
    let directives = List.map ~f:(fun d -> Directive d) node#directives in
    let line_comments =
      Option.value_map ~default:[] ~f:(fun c -> [ Line_comment c ]) node#line_comment
    in
    let payload = [ Payload { region = node#region; value = node#payload } ] in
    (* We need to iterate through each region in the right order, so we must
       first sort each field before visiting them. *)
    let fields =
      List.sort ~compare:compare_wrap_fields_regs
      @@ List.concat [ attributes; comments; directives; line_comments; payload ]
    in
    List.iter fields ~f:(function
        | Attribute a -> mk_tokens_reg Decorator a
        | Comment (Block c | Line c) | Line_comment c -> mk_tokens_reg Comment c
        | Directive d -> directive d
        | Payload p ->
          Option.iter token_type ~f:(fun token_type ->
              mk_tokens ?token_modifiers token_type p.region))
  in
  let mk_tokens_wrap
      (type node)
      ?(token_modifiers : SemanticTokenModifiers.t array option)
      (token_type : SemanticTokenTypes.t)
      (node : node Lexing_shared.Wrap.t)
      : unit
    =
    mk_tokens_wrap_impl ?token_modifiers (Some token_type) node
  in
  let () =
    let open Cst_shared.Fold in
    let terminal
        (type node)
        (token_type : SemanticTokenTypes.t)
        (node : node Lexing_shared.Wrap.t)
        : unit fold_control
      =
      mk_tokens_wrap token_type node;
      Stop
    in
    let keyword = terminal Keyword in
    let operator = terminal Operator in
    let eof (type node) (node : node Lexing_shared.Wrap.t) =
      mk_tokens_wrap_impl None node;
      Stop
    in
    match cst with
    | CameLIGO cst ->
      let open Cst_cameligo.CST in
      let open Cst_cameligo.Fold in
      let variable_to_wrap = function
        | Var v -> v
        | Esc v -> v
      in
      let fold_ f = fold' () (fun () -> f) in
      let rec collect (type_override : env) (Some_node (node, sing)) : unit fold_control =
        let fold_collect type_override node sing =
          fold_ (collect type_override) (node -| sing)
        in
        let type_override : SemanticTokenTypes.t =
          match type_override with
          | Normal -> Variable
          | FunApp -> Function
          | FunArg -> Parameter
          | FunDef -> Function
        in
        match sing with
        (* Don't create tokens which fall outside the input [range]. *)
        | S_reg _ when outside node.region -> Stop
        | S_wrap _ when outside node#region -> Stop
        (* Keywords *)
        | S_kwd_begin -> keyword node
        | S_kwd_do -> keyword node
        | S_kwd_done -> keyword node
        | S_kwd_downto -> keyword node
        | S_kwd_else -> keyword node
        | S_kwd_end -> keyword node
        | S_kwd_false -> keyword node
        | S_kwd_for -> keyword node
        | S_kwd_fun -> keyword node
        | S_kwd_if -> keyword node
        | S_kwd_in -> keyword node
        | S_kwd_include -> keyword node
        | S_kwd_land -> keyword node
        | S_kwd_let -> keyword node
        | S_kwd_lor -> keyword node
        | S_kwd_lsl -> keyword node
        | S_kwd_lsr -> keyword node
        | S_kwd_lxor -> keyword node
        | S_kwd_match -> keyword node
        | S_kwd_mod -> keyword node
        | S_kwd_module -> keyword node
        | S_kwd_mut -> keyword node
        | S_kwd_not -> keyword node
        | S_kwd_of -> keyword node
        | S_kwd_or -> keyword node
        | S_kwd_rec -> keyword node
        | S_kwd_struct -> keyword node
        | S_kwd_then -> keyword node
        | S_kwd_true -> keyword node
        | S_kwd_type -> keyword node
        | S_kwd_upto -> keyword node
        | S_kwd_val -> keyword node
        | S_kwd_while -> keyword node
        | S_kwd_with -> keyword node
        (* Symbols *)
        | S_append -> operator node
        | S_arrow -> operator node
        | S_ass -> operator node
        | S_bool_and -> operator node
        | S_bool_or -> operator node
        | S_caret -> operator node
        | S_colon -> operator node
        | S_comma -> operator node
        | S_cons -> operator node
        | S_dot -> operator node
        | S_equal -> operator node
        | S_geq -> operator node
        | S_gt -> operator node
        | S_lbrace -> operator node
        | S_lbracket -> operator node
        | S_leq -> operator node
        | S_lpar -> operator node
        | S_lt -> operator node
        | S_minus -> operator node
        | S_minus_eq -> operator node
        | S_neq -> operator node
        | S_plus -> operator node
        | S_plus_eq -> operator node
        | S_quote -> operator node
        | S_rbrace -> operator node
        | S_rbracket -> operator node
        | S_rev_app -> operator node
        | S_rpar -> operator node
        | S_semi -> operator node
        | S_slash -> operator node
        | S_slash_eq -> operator node
        | S_times -> operator node
        | S_times_eq -> operator node
        | S_vbar -> operator node
        | S_vbar_eq -> operator node
        (* Literals *)
        | S_attribute ->
          let token_type =
            match node#payload with
            | "comment", _ -> SemanticTokenTypes.Comment
            | _ -> SemanticTokenTypes.Decorator
          in
          terminal token_type node
        | S_ctor -> terminal EnumMember node
        | S_field_name -> terminal Property (variable_to_wrap node)
        | S_language -> terminal Macro node
        | S_module_name -> terminal Namespace node
        | S_string_literal -> terminal String node
        | S_type_name -> terminal TypeParameter (variable_to_wrap node)
        | S_type_variable -> terminal TypeParameter (variable_to_wrap node)
        | S_variable -> terminal type_override (variable_to_wrap node)
        | S_verbatim_literal -> terminal String node
        | S_wrap (S_tuple_2 (S_lexeme, S_hex)) -> terminal Number node
        | S_wrap (S_tuple_2 (S_lexeme, S_int64)) -> terminal Number node
        | S_wrap (S_tuple_2 (S_lexeme, S_z)) -> terminal Number node
        (* EOF *)
        | S_eof -> eof node
        (* Code injections should also highlight the [rbracket] like their [language]s. *)
        | S_code_inj ->
          let { language; code; rbracket } = node in
          mk_tokens_wrap Macro language;
          fold_collect Normal code S_expr;
          mk_tokens_wrap Macro rbracket;
          Stop
        (* Functions *)
        | S_let_binding ->
          let { binders; type_params; rhs_type; eq; let_rhs } = node in
          let hd, tl = binders in
          (* If we have more than one binder, it's a function. *)
          (* TODO: distinguish two definitions:
             1. let foo : int = 42
             2. let bar : int -> int = fun x -> x + 42 *)
          fold_ (collect (if List.is_empty tl then Normal else FunDef)) (hd -| S_pattern);
          (* Warning: the recursion into [type_params] must be between [hd] and
             [tl], as something like [let f (type a b) (x : a) : b = failwith x]
             will have [f] as [hd] and [(x : b)] as [[tl]] and the order of the
             recursion matters. *)
          fold_collect Normal type_params (S_option (S_par S_type_params));
          fold_collect FunArg tl (S_list S_pattern);
          fold_collect Normal rhs_type (S_option S_type_annotation);
          fold_collect Normal eq S_equal;
          fold_collect Normal let_rhs S_expr;
          Stop
        | S_fun_expr ->
          let { kwd_fun; type_params; binders; rhs_type; arrow; body } = node in
          fold_collect Normal kwd_fun S_kwd_fun;
          fold_collect Normal type_params (S_option (S_par S_type_params));
          fold_collect FunArg binders (S_nseq S_pattern);
          fold_collect Normal rhs_type (S_option S_type_annotation);
          fold_collect Normal arrow S_arrow;
          fold_collect Normal body S_expr;
          Stop
        | S_tuple_2 (S_expr, S_nseq S_expr) ->
          (match fst node with
          | E_ModPath path ->
            let { module_path; selector; field } = path.value in
            fold_collect Normal module_path (S_nsepseq (S_module_name, S_dot));
            fold_collect Normal selector S_dot;
            fold_collect FunApp field S_expr
          | E_Par expr ->
            let { lpar; inside; rpar } = expr.value in
            fold_collect Normal lpar S_lpar;
            fold_collect FunApp inside S_expr;
            fold_collect Normal rpar S_rpar
          | E_Proj proj ->
            let { record_or_tuple; selector; field_path } = proj.value in
            fold_collect Normal record_or_tuple S_expr;
            fold_collect Normal selector S_dot;
            let unsnoc xs = Option.both (List.drop_last xs) (List.last xs) in
            let hd, tl = field_path in
            (* Highlight the last selection as a function. *)
            (match unsnoc tl with
            | None -> fold_collect FunApp hd S_selection
            | Some (init, last) ->
              fold_collect Normal hd S_selection;
              fold_collect Normal init (S_list (S_tuple_2 (S_dot, S_selection)));
              fold_collect FunApp last (S_tuple_2 (S_dot, S_selection)))
          | expr -> fold_collect FunApp expr S_expr);
          fold_collect Normal (snd node) (S_nseq S_expr);
          Stop
        (* Type applications are swapped; need to iterate in reverse order *)
        | S_reg (S_tuple_2 (S_type_expr, S_type_ctor_arg)) ->
          let type_expr, type_ctor_arg = node.value in
          fold_collect Normal type_ctor_arg S_type_ctor_arg;
          fold_collect Normal type_expr S_type_expr;
          Stop
        (* In sequence expressions, the order is wrong; need to fix the
           iteration order *)
        | S_sequence_expr ->
          let { compound; elements } = node in
          (match compound with
          | None -> fold_collect Normal elements (S_sepseq (S_expr, S_semi))
          | Some (BeginEnd (node_1, node_2)) ->
            fold_collect Normal node_1 S_kwd_begin;
            fold_collect Normal elements (S_sepseq (S_expr, S_semi));
            fold_collect Normal node_2 S_kwd_end
          | Some (Parens (node_1, node_2)) ->
            fold_collect Normal node_1 S_lpar;
            fold_collect Normal elements (S_sepseq (S_expr, S_semi));
            fold_collect Normal node_2 S_rpar);
          Stop
        (* Directives *)
        | S_directive ->
          directive node;
          Stop
        (* Do nothing special for other nodes *)
        | _ -> Skip
      in
      fold_cst' () (fun () -> collect Normal) cst
    | JsLIGO cst ->
      let open Cst_jsligo.CST in
      let open Cst_jsligo.Fold in
      let variable_to_wrap = function
        | Var v -> v
        | Esc v -> v
      in
      let fold_ f = fold' () (fun () -> f) in
      let rec collect (type_override : env) (Some_node (node, sing)) : unit fold_control =
        let fold_collect type_override node sing =
          fold_ (collect type_override) (node -| sing)
        in
        let type_override : SemanticTokenTypes.t =
          match type_override with
          | Normal -> Variable
          | FunApp -> Function
          | FunArg -> Parameter
          | FunDef -> Function
        in
        match sing with
        (* Don't create tokens which fall outside the input [range]. *)
        | S_reg _ when outside node.region -> Stop
        | S_wrap _ when outside node#region -> Stop
        (* Keywords *)
        | S_kwd_as -> keyword node
        | S_kwd_break -> keyword node
        | S_kwd_case -> keyword node
        | S_kwd_const -> keyword node
        | S_kwd_continue -> keyword node
        | S_kwd_contract_of -> keyword node
        | S_kwd_default -> keyword node
        | S_kwd_do -> keyword node
        | S_kwd_else -> keyword node
        | S_kwd_export -> keyword node
        | S_kwd_extends -> keyword node
        | S_kwd_false -> keyword node
        | S_kwd_for -> keyword node
        | S_kwd_from -> keyword node
        | S_kwd_function -> keyword node
        | S_kwd_if -> keyword node
        | S_kwd_implements -> keyword node
        | S_kwd_import -> keyword node
        | S_kwd_interface -> keyword node
        | S_kwd_let -> keyword node
        | S_kwd_match -> keyword node
        | S_kwd_namespace -> keyword node
        | S_kwd_of -> keyword node
        | S_kwd_parameter_of -> keyword node
        | S_kwd_return -> keyword node
        | S_kwd_switch -> keyword node
        | S_kwd_true -> keyword node
        | S_kwd_type -> keyword node
        | S_kwd_when -> keyword node
        | S_kwd_while -> keyword node
        (* Symbols *)
        | S_arrow -> operator node
        | S_bit_and_eq -> operator node
        | S_bit_and -> operator node
        | S_bit_neg -> operator node
        | S_bit_or_eq -> operator node
        | S_bit_or -> operator node
        | S_bit_sl_eq -> operator node
        | S_bit_sl -> operator node
        | S_bit_sr_eq -> operator node
        | S_bit_sr -> operator node
        | S_bit_xor_eq -> operator node
        | S_bit_xor -> operator node
        | S_bool_and -> operator node
        | S_bool_neg -> operator node
        | S_bool_or -> operator node
        | S_bool_xor -> operator node
        | S_colon -> operator node
        | S_comma -> operator node
        | S_decrement -> operator node
        | S_div_eq -> operator node
        | S_dot -> operator node
        | S_ellipsis -> operator node
        | S_equal_cmp -> operator node
        | S_equal -> operator node
        | S_geq -> operator node
        | S_gt -> operator node
        | S_increment -> operator node
        | S_lbrace -> operator node
        | S_lbracket -> operator node
        | S_leq -> operator node
        | S_lpar -> operator node
        | S_lt -> operator node
        | S_minus_eq -> operator node
        | S_minus -> operator node
        | S_neq -> operator node
        | S_plus_eq -> operator node
        | S_plus -> operator node
        | S_property_sep -> operator node
        | S_qmark -> operator node
        | S_rbrace -> operator node
        | S_rbracket -> operator node
        | S_remainder -> operator node
        | S_rem_eq -> operator node
        | S_rpar -> operator node
        | S_semi -> operator node
        | S_sharp -> operator node
        | S_slash -> operator node
        | S_times_eq -> operator node
        | S_times -> operator node
        | S_vbar -> operator node
        | S_wild -> operator node
        (* Literals *)
        | S_attribute ->
          let token_type =
            match node#payload with
            | "comment", _ -> SemanticTokenTypes.Comment
            | _ -> SemanticTokenTypes.Decorator
          in
          terminal token_type node
        | S_ctor -> terminal EnumMember node
        | S_file_path -> terminal String node
        | S_fun_name -> terminal Function (variable_to_wrap node)
        | S_intf_name -> terminal Interface node
        | S_language -> terminal Macro node
        | S_namespace_name -> terminal Namespace node
        | S_property_name -> terminal Property (variable_to_wrap node)
        | S_type_ctor -> terminal TypeParameter (variable_to_wrap node)
        | S_type_name -> terminal TypeParameter (variable_to_wrap node)
        | S_type_var -> terminal TypeParameter (variable_to_wrap node)
        | S_variable -> terminal type_override (variable_to_wrap node)
        (* Other literals *)
        | S_bytes_literal -> terminal String node
        | S_int_literal -> terminal Number node
        | S_mutez_literal -> terminal Number node
        | S_nat_literal -> terminal Number node
        | S_string_literal -> terminal String node
        | S_verbatim_literal -> terminal String node
        (* EOF *)
        | S_eof -> eof node
        (* Functions *)
        | S_value_decl ->
          let { kind; bindings } = node in
          fold_collect Normal kind S_var_kind;
          fold_collect Normal bindings (S_nsepseq (S_reg S_val_binding, S_comma));
          Stop
        | S_arrow_fun_expr ->
          let { generics; parameters; rhs_type; arrow; fun_body } = node in
          fold_collect Normal generics (S_option S_generics);
          (* There is no way to tell if the literals in this expression are
             parameters looking alone at the CST, so we need this. *)
          fold_collect FunArg parameters S_arrow_fun_params;
          fold_collect Normal rhs_type (S_option (S_array_2 (S_colon, S_type_expr)));
          fold_collect Normal arrow S_arrow;
          fold_collect Normal fun_body S_fun_body;
          Stop
        | S_function_expr ->
          let ({ kwd_function; generics; parameters; rhs_type; fun_body } : function_expr)
            =
            node
          in
          fold_collect Normal kwd_function S_kwd_function;
          fold_collect Normal generics (S_option S_generics);
          (* There is no way to tell if the literals in this expression are
             parameters looking alone at the CST, so we need this. *)
          fold_collect FunArg parameters S_arrow_fun_params;
          fold_collect Normal rhs_type (S_option (S_array_2 (S_colon, S_type_expr)));
          fold_collect Normal fun_body S_fun_body;
          Stop
        (* Highlight the function app (LHS) as a function. *)
        | S_array_2 (S_expr, S_arguments) ->
          (match fst node with
          | E_NamePath path ->
            let { namespace_path; selector; property } = path.value in
            fold_collect Normal namespace_path (S_nsepseq (S_namespace_name, S_dot));
            fold_collect Normal selector S_dot;
            fold_collect FunApp property S_expr
          | E_Par expr ->
            let { lpar; inside; rpar } = expr.value in
            fold_collect Normal lpar S_lpar;
            fold_collect FunApp inside S_expr;
            fold_collect Normal rpar S_rpar
          | E_Proj proj ->
            let { object_or_array; property_path } = proj.value in
            fold_collect Normal object_or_array S_expr;
            fold_collect FunApp property_path (S_nseq S_selection)
          | expr -> fold_collect FunApp expr S_expr);
          fold_collect Normal (snd node) S_arguments;
          Stop
        (* Fix iteration order for the postfix operators. *)
        | S_un_op S_decrement ->
          let { op; arg } = node in
          if Region.lt op#region (expr_to_region arg)
          then (
            fold_collect Normal op S_decrement;
            fold_collect Normal arg S_expr)
          else (
            fold_collect Normal arg S_expr;
            fold_collect Normal op S_decrement);
          Stop
        | S_un_op S_increment ->
          let { op; arg } = node in
          if Region.lt op#region (expr_to_region arg)
          then (
            fold_collect Normal op S_increment;
            fold_collect Normal arg S_expr)
          else (
            fold_collect Normal arg S_expr;
            fold_collect Normal op S_increment);
          Stop
        (* Directives *)
        | S_directive ->
          directive node;
          Stop
        (* Do nothing special for other nodes *)
        | _ -> Skip
      in
      fold_cst' () (fun () -> collect Normal) cst
  in
  Vector.to_array data


let on_req_semantic_tokens_range (path : Path.t) (range : Range.t)
    : SemanticTokens.t option Handler.t
  =
  let open Handler in
  let@ () =
    send_debug_msg
    @@ Format.asprintf "On request: semantic tokens range (at %a)" Range.pp range
  in
  with_cst path ~default:None
  @@ fun cst ->
  let data = semantic_tokens cst range in
  let@ () = mk_diff data in
  return (Some (SemanticTokens.create ~data ()))


let on_req_semantic_tokens_full (path : Path.t) : SemanticTokens.t option Handler.t =
  let open Handler in
  let@ () = send_debug_msg "On request: semantic tokens full" in
  on_req_semantic_tokens_range path Range.whole_file
