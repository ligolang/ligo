module Core = SyntaxHighlighting.Core

let let_binding_match1: Core.regexp = {
  emacs    = "\\\\b\\\\(let\\\\)\\\\b[ ]*"; 
  textmate = "\\b(let)\\b\\s*";
  vim      = "\\(let\\)\\W";
}

let let_binding_match2: Core.regexp = {
  emacs    = "\\\\b\\\\(rec\\\\|\\\\)\\\\b[ ]*"; 
  textmate = "\\b(rec|)\\s*";
  vim      = "rec\\W\\|";
}

let let_binding_match3: Core.regexp = {
  emacs    = "\\\\b\\\\([a-zA-Z$_][a-zA-Z0-9$_]*\\\\|\\\\)"; 
  textmate = "\\b([a-zA-Z$_][a-zA-Z0-9$_]*)";
  vim      = "\\<[a-zA-Z$_][a-zA-Z0-9$_]*\\>";
}
let let_binding_match1_ligo: Core.regexp = {
  emacs    = "\\\\b\\\\(function\\\\)\\\\b[ ]*"; 
  textmate = "\\b(function)\\b\\s*";
  vim      = "\\(function\\)\\W";
}

let let_binding_match2_ligo: Core.regexp = {
  emacs    = "\\\\b\\\\([a-zA-Z$_][a-zA-Z0-9$_]*\\\\|\\\\)"; 
  textmate = "\\b([a-zA-Z$_][a-zA-Z0-9$_]*)";
  vim      = "\\<[a-zA-Z$_][a-zA-Z0-9$_]*\\>";
}

let lambda_begin: Core.regexp = {
  emacs    = "\\\\b\\\\(fun\\\\)\\\\b";
  textmate = "\\b(fun)\\b";
  vim      = "\\(fun\\)\\W"
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

let control_keywords_match: Core.regexp = {
  emacs    = "\\\\b\\\\(match\\\\|with\\\\|if\\\\|then\\\\|else\\\\|assert\\\\|failwith\\\\|begin\\\\|end\\\\|in\\\\)\\\\b";
  textmate = "\\b(match|with|if|then|else|assert|failwith|begin|end|in)\\b";
  vim      = "\\<\\(match\\|with\\|if\\|then\\|else\\|assert\\|failwith\\|begin\\|end\\|in\\)\\>"
}

let control_keywords_match_reasonligo: Core.regexp = {
  emacs    = "\\\\b\\\\(switch\\\\|if\\\\|else\\\\|assert\\\\|failwith\\\\)\\\\b";
  textmate = "\\b(switch|if|else|assert|failwith)\\b";
  vim      = "\\<\\(switch\\|if\\|else\\|assert\\|failwith\\)\\>"
}

let control_keywords_match_ligo: Core.regexp = {
  emacs    = "\\\\b\\\\(case\\\\|with\\\\|if\\\\|then\\\\|else\\\\|assert\\\\|failwith\\\\|begin\\\\|end\\\\|in\\\\|is\\\\|from\\\\|skip\\\\|block\\\\|contains\\\\|to\\\\|step\\\\|of\\\\|while\\\\|for\\\\|remove\\\\)\\\\b";
  textmate = "\\b(case|with|if|then|else|assert|failwith|begin|end|in|is|from|skip|block|contains|to|step|of|while|for|remove)\\b";
  vim      = "\\<\\(case\\|with\\|if\\|then\\|else\\|assert\\|failwith\\|begin\\|end\\|in\\|is\\|from\\|skip\\|block\\|contains\\|to\\|step\\|of\\|while\\|for\\|remove\\)\\>"
}

let operators_match: Core.regexp = {
  emacs    = "[ ]*\\\\(::\\\\|-\\\\|+\\\\|/\\\\|mod\\\\|land\\\\|lor\\\\|lxor\\\\|lsl\\\\|lsr\\\\|&&\\\\|||\\\\|<\\\\|>\\\\|<>\\\\|<=\\\\|>=\\\\)[ ]*";
  textmate = "\\s+(::|\\-|\\+|mod|land|lor|lxor|lsl|lsr|&&|\\|\\||>|<>|<=|=>|<|>)\\s+";
  vim      = "\\<\\(::\\|-\\|+\\|/\\|mod\\|land\\|lor\\|lxor\\|lsl\\|lsr\\|&&\\|||\\|<\\|>\\|<>\\|<=\\|>=\\)\\>"
}

let operators_match_reasonligo: Core.regexp = {
  emacs    = "[ ]*\\\\(-\\\\|+\\\\|/\\\\|mod\\\\|land\\\\|lor\\\\|lxor\\\\|lsl\\\\|lsr\\\\|&&\\\\|||\\\\|<\\\\|>\\\\|!=\\\\|<=\\\\|>=\\\\)[ ]*";
  textmate = "\\s+(\\-|\\+|mod|land|lor|lxor|lsl|lsr|&&|\\|\\||>|!=|<=|=>|<|>)\\s+";
  vim      = "\\<\\(-\\|+\\|/\\|mod\\|land\\|lor\\|lxor\\|lsl\\|lsr\\|&&\\|||\\|<\\|>\\|!=\\|<=\\|>=\\)\\>"
}

let operators_match_ligo: Core.regexp = {
  emacs    = "[ ]*\\\\(-\\\\|+\\\\|/\\\\|mod\\\\|land\\\\|lor\\\\|lxor\\\\|lsl\\\\|lsr\\\\|&&\\\\|||\\\\|<\\\\|>\\\\|=/=\\\\|<=\\\\|>=\\\\)[ ]*";
  textmate = "\\s+(\\-|\\+|mod|land|lor|lxor|lsl|lsr|&&|\\|\\||>|=/=|<=|=>|<|>)\\s+";
  vim      = "\\<\\(-\\|+\\|/\\|mod\\|land\\|lor\\|lxor\\|lsl\\|lsr\\|&&\\|||\\|<\\|>\\|=/=\\|<=\\|>=\\)\\>"
}


let module_match1: Core.regexp = {
  emacs    = "\\\\b\\\\([A-Z][a-zA-Z0-9_$]*\\\\)\\\\.";
  textmate = "\\b([A-Z][a-zA-Z0-9_$]*)\\.";
  vim      = "\\<\\([A-Z][a-zA-Z0-9_$]*\\)\\."
}

let module_match2: Core.regexp = {
  emacs    = "\\\\([a-z_][a-zA-Z0-9_$]*\\\\)\\\\b";
  textmate = "([a-z][a-zA-Z0-9_$]*)";
  vim      = "[a-z_][a-zA-Z0-9_$]*"
}


let identifier_constructor_match: Core.regexp = {
  emacs    = "\\\\b\\\\([A-Z][a-zA-Z0-9_$]*\\\\)\\\\b";
  textmate = "\\b([A-Z][a-zA-Z0-9_$]*)\\b";
  vim      = "\\<\\([A-Z][a-zA-Z0-9_$]*\\)\\>";
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
  emacs    = "{";
  textmate = "({)";
  vim      = "{";
}

let brackets_end: Core.regexp = {
  emacs    = "}";
  textmate = "(})";
  vim      = "}";
}

let semicolon_match: Core.regexp = {
  emacs    = ";";
  textmate = "(;)";
  vim      = ";";
}

let type_annotation_match: Core.regexp = {
  emacs    = "\\\\(:[ ]*[^]=;\\\\):]*\\\\)";
  textmate = "(:[ ]*[^\\]=;\\):]*)";
  vim      = "\\(:[^]=;\\):]*\\)"
}

let type_annotation_match_reasonligo: Core.regexp = {
  emacs    = "\\\\(:[ ]*[^,=\n]*\\\\)";
  textmate = "(:[ ]*[^\\]=;\\):]*)";
  vim      = "\\(:[^]=;\\):]*\\)"
}

let multiplication_match: Core.regexp = {
  emacs    = "\\\\(*\\\\)";
  textmate = "(*)";
  vim      = "\\(*\\)"
}

let const_or_var: Core.regexp = {
  emacs    = "\\\\b\\\\(const\\\\|var\\\\)\\\\b";
  textmate = "\\b(const|var)\\b";
  vim      = "\\<\\(const\\|var\\)\\>"
}

(* TODO: add regexps for emacs & vim later *)

let attributes_match_jsligo: Core.regexp = {
  emacs    = "";
  textmate = "(/\\s*@.*\\s*|/\\*\\s*@.*\\*/)";
  vim      = ""
}

let let_binding_match1_jsligo: Core.regexp = {
  emacs    = ""; 
  textmate = "\\b(let|const)\\b\\s*";
  vim      = "";
}

let let_binding_match2_jsligo: Core.regexp = {
  emacs    = ""; 
  textmate = "\\b([a-zA-Z$_][a-zA-Z0-9$_]*)";
  vim      = "";
}

let control_keywords_match_jsligo: Core.regexp = {
  emacs    = "";
  textmate = "\\b(switch|case|default|if|else|for|of|while|return|break|export)\\b";
  vim      = ""
}

let operators_match_jsligo: Core.regexp = {
  emacs    = "";
  textmate = "\\s+(\\-|\\+|%|&&|\\|\\||==|!=|<=|>=|<|>|\\*|/|=|!|\\*=|/=|%=|\\+=|\\-=)\\s+";
  vim      = ""
}

let module_alias_match1_jsligo: Core.regexp = {
  emacs    = "";
  textmate = "\\b(import)\\b\\s*";
  vim      = ""
}

let module_alias_match2_jsligo: Core.regexp = {
  emacs    = "";
  textmate = "\\b([A-Z][a-zA-Z0-9_$]*)";
  vim      = ""
}

let module_declaration_match1_jsligo: Core.regexp = {
  emacs    = "";
  textmate = "\\b(namespace)\\b\\s*";
  vim      = ""
}

let module_declaration_match2_jsligo: Core.regexp = {
  emacs    = "";
  textmate = "\\b([A-Z][a-zA-Z0-9_$]*)";
  vim      = ""
}

let module_match1_jsligo: Core.regexp = {
  emacs    = "";
  textmate = "\\b([A-Z][\\.a-zA-Z0-9_$]*)\\.";
  vim      = ""
}

let module_match2_jsligo: Core.regexp = {
  emacs    = "";
  textmate = "([a-zA-Z0-9_$]*)";
  vim      = ""
}

let int_literal_match: Core.regexp = {
  emacs    = "\\\\b\\\\([0-9]+\\\\)\\\\b";
  textmate = "\\b([0-9]+)\\b";
  vim      = "\\<[0-9]+\\>";
}

(* CameLIGO Types *)
let type_definition_match: Core.regexp = {
  emacs    = "\\\\b\\\\(type\\\\)\\\\b";
  textmate = "\\b(type)\\b";
  vim      = "\\<\\(type\\)\\>"
}

(*
  A type declaration may appear in the following places:
  * At top-level or in a module, immediately before a "let", "#" (directive),
    "module", "type", "end", "[%" (attribute) or EOF.
  * Locally inside a "let", immediately before an "in".

  follow(type_decl) = Type Module Let In End EOF Directive Attr
*)
let type_definition_begin: Core.regexp = type_definition_match

let type_definition_end: Core.regexp = {
  (* FIXME: Emacs doesn't support negative look-ahead... too bad! *)
  emacs    = "^#\\\\|\\\\[%\\\\|\\\\b\\\\(let\\\\|in\\\\|type\\\\|end\\\\|module\\\\)\\\\b";
  textmate = "(?=^#|\\[%|\\b(let|in|type|end|module)\\b)";
  vim      = "\\(^#\\|\\[%\\|\\<\\(let\\|in\\|type\\|end\\|module\\)\\>\\)\\@!"
}

let type_name_match: Core.regexp = {
  emacs    = "";
  textmate = "\\b([a-z_][a-zA-Z0-9_]*)\\b";
  vim      = "\\<\\([a-z_][a-zA-Z0-9_]*\\)\\>";
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
let type_annotation_begin: Core.regexp = {
  emacs    = "\\\\(:\\\\)";
  textmate = "(:)";
  vim      = "\\(:\\)";
}

let type_annotation_end: Core.regexp = {
  (* FIXME: Emacs doesn't support negative look-ahead *)
  emacs    = ")\\\\|=\\\\|;\\\\|}";
  textmate = "(?=\\)|=|;|})";
  vim      = "\\()\\|=\\|;\\|}\\)\\@!";
}

let type_annotation_begin_lambda: Core.regexp = type_annotation_begin

let type_annotation_end_lambda: Core.regexp = {
  (* FIXME: Emacs doesn't support negative look-ahead *)
  emacs    = ")\\\\|=\\\\|;\\\\|}\\\\|->";
  textmate = "(?=\\)|=|;|}|->)";
  vim      = "\\()\\|=\\|;\\|}\\|->\\)\\@!";
}

let type_field_annotation_begin: Core.regexp = type_annotation_begin

let type_field_annotation_end: Core.regexp = {
  (* FIXME: Emacs doesn't support negative look-ahead *)
  emacs    = "\\\\(}\\\\|;\\\\)";
  textmate = "(?=}|;)";
  vim      = "\\(}\\|;\\)\\@!";
}
