module Core = SyntaxHighlighting.Core

let let_binding_match1: Core.regexp = {
  emacs    = "\\\\b\\\\(let\\\\)\\\\b";
  textmate = "\\b(let)\\b";
  vim      = "\\<\\(let\\)\\>";
}

let let_binding_match2: Core.regexp = {
  emacs    = "\\\\(\\\\brec\\\\b\\\\|\\\\)";
  textmate = "(\\brec\\b|)";
  vim      = "\\<rec\\>\\|";
}

let let_binding_match3: Core.regexp = {
  emacs    = "\\\\b\\\\([a-zA-Z$_][a-zA-Z0-9$_]*\\\\)\\\\b";
  textmate = "\\b([a-zA-Z$_][a-zA-Z0-9$_]*)\\b";
  vim      = "\\<\\([a-zA-Z$_][a-zA-Z0-9$_]*\\)\\>";
}

let let_binding_match1_ligo: Core.regexp = {
  emacs    = "\\\\b\\\\(function\\\\)\\\\b";
  textmate = "\\b(function)\\b";
  vim      = "\\<function\\>";
}

let let_binding_match2_ligo: Core.regexp = {
  emacs    = "\\\\b\\\\([a-zA-Z$_][a-zA-Z0-9$_]*\\\\)"; 
  textmate = "\\b([a-zA-Z$_][a-zA-Z0-9$_]*)";
  vim      = "\\<[a-zA-Z$_][a-zA-Z0-9$_]*\\>";
}

let lambda_begin: Core.regexp = {
  emacs    = "\\\\b\\\\(fun\\\\)\\\\b";
  textmate = "\\b(fun)\\b";
  vim      = "\\<fun\\>"
}

let lambda_end: Core.regexp = {
  emacs    = "(->)";
  textmate = "(->)";
  vim      = "\\(->\\)"
}

let of_keyword_match: Core.regexp = {
  emacs    = "\\\\b\\\\(of)\\\\b";
  textmate = "\\b(of)\\b";
  vim      = "\\<\\(of\\)\\>";
}

let is_keyword_match: Core.regexp = {
  emacs    = "\\\\b\\\\(is)\\\\b";
  textmate = "\\b(is)\\b";
  vim      = "\\<\\(is\\)\\>";
}

let record_keyword_match: Core.regexp = {
  emacs    = "\\\\brecord\\\\b";
  textmate = "\\b(record)\\b";
  vim      = "\\<record\\>";
}

let control_keywords_match: Core.regexp = {
  emacs    = "\\\\b\\\\(match\\\\|with\\\\|if\\\\|then\\\\|else\\\\|assert\\\\|failwith\\\\|begin\\\\)\\\\b";
  textmate = "\\b(match|with|if|then|else|assert|failwith|begin)\\b";
  vim      = "\\<\\(match\\|with\\|if\\|then\\|else\\|assert\\|failwith\\|begin\\)\\>"
}

let structure_keywords_match: Core.regexp = {
  emacs    = "\\\\b\\\\(struct\\\\|end\\\\|in\\\\)\\\\b";
  textmate = "\\b(struct|end|in)\\b";
  vim      = "\\<\\(struct\\|end\\|in\\)\\>";
}

let control_keywords_match_ligo: Core.regexp = {
  emacs    = "\\\\b\\\\(case\\\\|with\\\\|if\\\\|then\\\\|else\\\\|assert\\\\|failwith\\\\|begin\\\\|end\\\\|in\\\\|is\\\\|from\\\\|skip\\\\|block\\\\|contains\\\\|to\\\\|step\\\\|of\\\\|while\\\\|for\\\\|remove\\\\)\\\\b";
  textmate = "\\b(case|with|if|then|else|assert|failwith|begin|end|in|is|from|skip|block|contains|to|step|of|while|for|remove)\\b";
  vim      = "\\<\\(case\\|with\\|if\\|then\\|else\\|assert\\|failwith\\|begin\\|end\\|in\\|is\\|from\\|skip\\|block\\|contains\\|to\\|step\\|of\\|while\\|for\\|remove\\)\\>"
}

let operators_match: Core.regexp = {
  emacs    = "::\\\\|-\\\\|+\\\\|/\\\\|\\\\b\\\\(mod\\\\|land\\\\|lor\\\\|lxor\\\\|lsl\\\\|lsr\\\\)\\\\b\\\\|&&\\\\|||\\\\|<\\\\|>\\\\|<>\\\\|<=\\\\|>=";
  textmate = "::|\\-|\\+|\\b(mod|land|lor|lxor|lsl|lsr)\\b|&&|\\|\\||>|<>|<=|=>|<|>";
  vim      = "::\\|-\\|+\\|/\\|\\<\\(mod\\|land\\|lor\\|lxor\\|lsl\\|lsr\\)\\>\\|&&\\|||\\|<\\|>\\|<>\\|<=\\|>="
}


let operators_match_ligo: Core.regexp = {
  emacs    = "\\\\b\\\\(-\\\\|+\\\\|/\\\\|mod\\\\|land\\\\|lor\\\\|lxor\\\\|lsl\\\\|lsr\\\\|&&\\\\|||\\\\|<\\\\|>\\\\|=/=\\\\|<=\\\\|>=\\\\)\\\\b";
  textmate = "\\b(\\-|\\+|mod|land|lor|lxor|lsl|lsr|&&|\\|\\||>|=/=|<=|=>|<|>)\\b";
  vim      = "\\<\\(-\\|+\\|/\\|mod\\|land\\|lor\\|lxor\\|lsl\\|lsr\\|&&\\|||\\|<\\|>\\|=/=\\|<=\\|>=\\)\\>"
}

let module_match1: Core.regexp = {
  emacs    = "\\\\b\\\\([A-Z][a-zA-Z0-9_$]*\\\\)\\\\.";
  textmate = "\\b([A-Z][a-zA-Z0-9_$]*)\\.";
  vim      = "\\<[A-Z][a-zA-Z0-9_$]*\\.";
}

let module_match2: Core.regexp = {
  emacs    = "\\\\b\\\\([a-z_][a-zA-Z0-9_$]*\\\\)\\\\b";
  textmate = "\\b([a-z][a-zA-Z0-9_$]*)\\b";
  vim      = "\\<[a-z_][a-zA-Z0-9_$]*\\>";
}

let module_keyword_match: Core.regexp = {
  emacs    = "\\\\bmodule\\\\b";
  textmate = "\\b(module)\\b";
  vim      = "\\<module\\>";
}

let identifier_match: Core.regexp = {
  emacs    = "\\\\b\\\\([a-z$_][a-zA-Z0-9$_]*\\\\)\\\\b";
  textmate = "\\b([a-z$_][a-zA-Z0-9$_]*)\\b";
  vim      = "\\<[a-z$_][a-zA-Z0-9$_]*\\>";
}

let identifier_constructor_match: Core.regexp = {
  emacs    = "\\\\b\\\\([A-Z][a-zA-Z0-9_$]*\\\\)\\\\b";
  textmate = "\\b([A-Z][a-zA-Z0-9_$]*)\\b";
  vim      = "\\<[A-Z][a-zA-Z0-9_$]*\\>";
}

let parentheses_begin: Core.regexp = {
  emacs    = "\\\\(";
  textmate = "(\\()";
  vim      = "(";
}

let parentheses_end: Core.regexp = {
  emacs    = "\\\\)";
  textmate = "(\\))";
  vim      = ")";
}

let brackets_begin: Core.regexp = {
  emacs    = "\\\\[";
  textmate = "(\\[)";
  vim      = "\\[";
}

let brackets_end: Core.regexp = {
  emacs    = "\\\\]";
  textmate = "(\\])";
  vim      = "\\]";
}

let braces_begin: Core.regexp = {
  emacs    = "{";
  textmate = "({)";
  vim      = "{";
}

let braces_end: Core.regexp = {
  emacs    = "}";
  textmate = "(})";
  vim      = "}";
}

let chevron_begin: Core.regexp = {
  emacs    = "<";
  textmate = "(<)";
  vim      = "<";
}

let chevron_end: Core.regexp = {
  emacs    = ">";
  textmate = "(>)";
  vim      = ">";
}

let semicolon_match: Core.regexp = {
  emacs    = ";";
  textmate = "(;)";
  vim      = ";";
}

let comma_match: Core.regexp = {
  emacs    = ",";
  textmate = "(,)";
  vim      = ",";
}

let colon_match: Core.regexp = {
  emacs    = ":";
  textmate = "(:)";
  vim      = ":";
}

let multiplication_match: Core.regexp = {
  emacs    = "\\\\(*\\\\)";
  textmate = "(*)";
  vim      = "*";
}

let const_or_var: Core.regexp = {
  emacs    = "\\\\b\\\\(const\\\\|var\\\\)\\\\b";
  textmate = "\\b(const|var)\\b";
  vim      = "\\<\\(const\\|var\\)\\>"
}

(* TODO: add regexps for emacs & vim later *)

let attributes_match_jsligo: Core.regexp = {
  emacs    = "";
  textmate = "(@[a-zA-Z][a-zA-Z0-9_:.@%]*)";
  vim      = ""
}

let let_binding_match1_jsligo: Core.regexp = {
  emacs    = "\\\\b\\\\(let\\\\|const\\\\)\\\\b";
  textmate = "\\b(let|const)\\b\\s*";
  vim      = "\\<\\(let\\|const\\)\\>";
}

let let_binding_match2_jsligo: Core.regexp = let_binding_match3

let identifier_annotation_positive_lookahead: Core.regexp = {
  (* FIXME: Emacs doesn't support positive look-ahead *)
  emacs    = "\\\\b\\\\([a-zA-Z$_][a-zA-Z0-9$_]*\\\\)\\\\b[:space:]*:";
  textmate = "\\b([a-zA-Z$_][a-zA-Z0-9$_]*)\\b\\s*(?=:)";
  vim      = "\\<\\([a-zA-Z$_][a-zA-Z0-9$_]*\\)\\>\\s*:\\@=";
}

let control_keywords_match_jsligo: Core.regexp = {
  emacs    = "";
  textmate = "\\b(switch|case|default|if|else|for|of|while|return|break|export)\\b";
  vim      = ""
}

let operators_match_jsligo: Core.regexp = {
  emacs    = "";
  textmate = "\\b(\\-|\\+|%|&&|\\|\\||==|!=|<=|>=|<|>|\\*|/|=|!|\\*=|/=|%=|\\+=|\\-=)\\b";
  vim      = ""
}

let module_alias_match1_jsligo: Core.regexp = {
  emacs    = "";
  textmate = "\\b(import)\\b";
  vim      = ""
}

let module_alias_match2_jsligo: Core.regexp = {
  emacs    = "";
  textmate = "\\b([A-Z][a-zA-Z0-9_$]*)\\b";
  vim      = ""
}

let module_declaration_match1_jsligo: Core.regexp = {
  emacs    = "";
  textmate = "\\b(namespace)\\b";
  vim      = ""
}

let module_declaration_match2_jsligo: Core.regexp = {
  emacs    = "";
  textmate = "\\b([A-Z][a-zA-Z0-9_$]*)\\b";
  vim      = ""
}

let module_match1_jsligo: Core.regexp = {
  emacs    = "";
  textmate = "\\b([A-Z][\\.a-zA-Z0-9_$]*)\\.";
  vim      = ""
}

let module_match2_jsligo: Core.regexp = {
  emacs    = "";
  textmate = "\\b([a-zA-Z0-9_$]*)\\b";
  vim      = ""
}

(* follow(property) = RBRACE COMMA *)
let property_expr_begin_jsligo: Core.regexp = colon_match

let property_expr_end_jsligo: Core.regexp = {
  (* FIXME: Emacs doesn't support positive look-ahead *)
  emacs    = ",\\\\|}";
  textmate = "(?=,|})";
  vim      = "\\(,\\|})\\@=";
}

let int_literal_match: Core.regexp = {
  emacs    = "\\\\b\\\\([0-9]+\\\\)\\\\b";
  textmate = "\\b([0-9]+)\\b";
  vim      = "\\<[0-9]+\\>";
}

let string_literal_match: Core.regexp = {
  emacs    = "\\\\\\\"\\\\.*\\\\\\\"";
  textmate = "(\\\".*\\\")";
  vim      = "\\\".*\\\"";
}

(* Types *)
let type_definition_match: Core.regexp = {
  emacs    = "\\\\btype\\\\b";
  textmate = "\\b(type)\\b";
  vim      = "\\<type\\>";
}

(*
  A type declaration may appear in the following places:
  * At top-level or in a module, immediately before a "let", "#" (directive),
    "module", "type", "end", "[%" (attribute) or EOF.
  * Locally inside a "let", immediately before an "in".

  follow(type_decl) = Type Module Let In End EOF Directive Attr
  Heads-up for an extra token: ")". This is for parametric types.
*)
let type_definition_begin: Core.regexp = type_definition_match

let type_definition_end: Core.regexp = {
  (* FIXME: Emacs doesn't support positive look-ahead... too bad! *)
  emacs    = "^#\\\\|\\\\[%\\\\|\\\\b\\\\(let\\\\|in\\\\|type\\\\|end\\\\|module\\\\)\\\\|)\\\\b";
  textmate = "(?=^#|\\[%|\\b(let|in|type|end|module)\\b|\\))";
  vim      = "\\(^#\\|\\[%\\|\\<\\(let\\|in\\|type\\|end\\|module\\)\\>\\|)\\)\\@=";
}

let type_name_match: Core.regexp = {
  emacs    = "\\\\b[a-z_][a-zA-Z0-9]\\\\*\\\\b";
  textmate = "\\b([a-z_][a-zA-Z0-9_]*)\\b";
  vim      = "\\<[a-z_][a-zA-Z0-9_]*\\>";
}

let type_var_match: Core.regexp = {
  emacs    = "'" ^ type_name_match.emacs;
  textmate = "'" ^ type_name_match.textmate;
  vim      = "'" ^ type_name_match.vim;
}

let type_operator_match: Core.regexp = {
  emacs    = "\\\\(->\\\\|\\\\.\\\\|\\\\*\\\\||\\\\)";
  textmate = "(->|\\.|\\*|\\|)";
  vim      = "\\(->\\|\\.\\|\\*\\||\\)";
}

(*
  A type annotation may appear in the following places:
  * In the return type of a declaration, immediately before a =.
  * In a function parameter, immediately before a ).
  * In a field of a record, immediately before a ; or a }.
  * In an arbitrary pattern, immediately before a ).

  follow(field_decl) = SEMI RBRACE
  follow(type_annotation(type_expr)) = RPAR EQ
  follow(type_annotation(lambda_app_type)) = ARROW
*)
let type_annotation_begin: Core.regexp = colon_match

let type_annotation_end: Core.regexp = {
  (* FIXME: Emacs doesn't support positive look-ahead *)
  emacs    = ")\\\\|=\\\\|;\\\\|}";
  textmate = "(?=\\)|=|;|})";
  vim      = "\\()\\|=\\|;\\|}\\)\\@=";
}

let type_annotation_begin_lambda: Core.regexp = type_annotation_begin

let type_annotation_end_lambda: Core.regexp = {
  (* FIXME: Emacs doesn't support positive look-ahead *)
  emacs    = ")\\\\|=\\\\|;\\\\|}\\\\|->";
  textmate = "(?=\\)|=|;|}|->)";
  vim      = "\\()\\|=\\|;\\|}\\|->\\)\\@=";
}

let type_field_annotation_begin: Core.regexp = type_annotation_begin

let type_field_annotation_end: Core.regexp = {
  (* FIXME: Emacs doesn't support positive look-ahead *)
  emacs    = "\\\\(}\\\\|;\\\\)";
  textmate = "(?=}|;)";
  vim      = "\\(}\\|;\\)\\@=";
}

(* follow(type_decl) = SEMI RBRACE Else EOF Default Case *)
let type_definition_begin_jsligo: Core.regexp = type_definition_match

let type_definition_end_jsligo: Core.regexp = {
  (* FIXME: Emacs doesn't support positive look-ahead *)
  emacs    = ";\\\\|}\\\\|\\\\b\\\\(else\\\\|default\\\\|case\\\\)\\\\b";
  textmate = "(?=;|}|\\b(else|default|case)\\b)";
  vim      = "\\(;\\|}\\|\\<\\(else\\|default\\|case\\)\\>\\)\\@="
}

let type_name_match_jsligo: Core.regexp = {
  emacs    = "\\\\b[a-zA-Z_][a-zA-Z0-9]\\\\*\\\\b";
  textmate = "\\b([a-zA-Z_][a-zA-Z0-9_]*)\\b";
  vim      = "\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\>";
}

let type_operator_match_jsligo: Core.regexp = {
  emacs    = "\\\\(=>\\\\|\\\\.\\\\||\\\\)";
  textmate = "(=>|\\.|\\|)";
  vim      = "\\(=>\\|\\.\\||\\)";
}

(*
  follow(type_annotation) = RPAR RBRACE COMMA ARROW
  follow(binding_type) = EQ
*)
let type_annotation_begin_jsligo: Core.regexp = type_annotation_begin

let type_annotation_end_jsligo: Core.regexp = {
  (* FIXME: Emacs doesn't support positive look-ahead *)
  emacs    = ")\\\\|=>\\\\|,\\\\|}\\\\|=";
  textmate = "(?=\\)|=>|,|}|=)";
  vim      = "\\()\\|=>\\|,\\}\\|=\\)\\@=";
}

(*
  follow(field_decl) = RBRACE COMMA
  n.b.: For JsLIGO, both the FIRST and FOLLOW for both the type_annotation in
  a property and the "expression field assignment" are the same.
*)
let type_annotation_field_begin_jsligo: Core.regexp = property_expr_begin_jsligo
let type_annotation_field_end_jsligo: Core.regexp = property_expr_end_jsligo

(*
  follow(as_expr_level) = SEMI RPAR REM_EQ RBRACKET RBRACE PLUS_EQ MULT_EQ MINUS_EQ    Else EQ EOF Default DIV_EQ Case COMMA COLON As
  follow(type_expr)     = SEMI RPAR REM_EQ RBRACKET RBRACE PLUS_EQ MULT_EQ MINUS_EQ GT Else EQ EOF Default DIV_EQ Case COMMA COLON As ARROW
  Since `as_expr_level` also includes `disj_expr_level` as an alternative, we
  likely want the intersection of those two. Therefore:
  follow(as_expr_level) ∩ follow(type_expr) = follow(as_expr_level)
*)
let type_as_begin_jsligo: Core.regexp = {
  emacs    = "\\\\bas\\\\b";
  textmate = "\\b(as)\\b";
  vim      = "\\<as\\>";
}

let type_as_end_jsligo: Core.regexp = {
  (* FIXME: Emacs doesn't support positive look-ahead *)
  emacs    = "";
  textmate = "(?=;|\\)|%=|\\]|}|\\+=|\\*=|-=|=|/=|,|:|\\b(else|default|case|as)\\b)";
  vim      = "\\(;\\|)\\|%=\\|\\]\\|}\\|+=\\|\\*=\\|-=\\|=\\|/=\\|,\\|:\\|\\(else\\|default\\|case\\|as\\)\\)\\@=";
}

let type_binder_positive_lookahead_ligo: Core.regexp = {
  (* FIXME: Emacs doesn't support positive look-ahead *)
  emacs    = "";
  textmate = "(?=([a-zA-Z0-9_,]+|\\s)+>)";
  vim      = "\\([a-zA-Z0-9_,]\\|\\s\\)\\+>\\@=";
}

(*
  follow(type_annotation) = SEMI RPAR RBRACKET Is EQ ASS
  n.b.: Remove the `%inline` from `type_annotation` before running Menhir to see
  its FOLLOW set.
*)
let type_annotation_begin_ligo: Core.regexp = type_annotation_begin

let type_annotation_end_ligo: Core.regexp = {
  (* FIXME: Emacs doesn't support positive look-ahead *)
  emacs    = ";\\\\|)\\\\|}\\\\|\\\\bis\\\\b\\\\|=\\\\|:=";
  textmate = "(?=;|\\)|\\]|\\bis\\b|=|:=)";
  vim      = "\\(;\\|)\\|}\\|\\<is\\>\\|=\\|:=\\)\\@=";
}

(* follow(type_decl) = Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr *)
let type_definition_begin_ligo: Core.regexp = type_definition_begin

let type_definition_end_ligo: Core.regexp = {
  (* FIXME: Emacs doesn't support positive look-ahead *)
  emacs    = "\\\\b\\\\(type\\\\|recursive\\\\|module\\\\|function\\\\|end\\\\|const\\\\)\\\\b\\\\|;\\\\|{\\\\|^#\\\\|\\\\[@";
  textmate = "(?=\\b(type|recursive|module|function|end|const)\\b|;|{|^#|\\[@)";
  vim      = "\\(\\<\\(type\\|recursive\\|module\\|function\\|end\\|const\\)\\>\\|;\\|{\\|^#\\|\\[@\\)\\@=";
}

let type_operator_match_ligo: Core.regexp = {
  emacs    = "\\\\(->\\\\|\\\\.\\\\||\\\\|\\\\*\\\\)";
  textmate = "(->|\\.|\\||\\*)";
  vim      = "\\(->\\|\\.\\||\\|\\*\\)";
}

(* follow(field_decl) = SEMI RBRACKET *)
let type_annotation_field_begin_ligo: Core.regexp = type_annotation_begin

let type_annotation_field_end_ligo: Core.regexp = {
  (* FIXME: Emacs doesn't support positive look-ahead *)
  emacs    = ";\\\\|\\\\]";
  textmate = "(?=;|\\])";
  vim      = "\\(;\\|\\]\\)\\@=";
}
