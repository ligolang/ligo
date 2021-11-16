module Core = SyntaxHighlighting.Core

let let_binding_match1: Core.regexp = {
  emacs    = "\\\\b\\\\(let\\\\)\\\\b[ ]*"; 
  textmate = "";
  vim      = "\\(let\\)\\W";
}

let let_binding_match2: Core.regexp = {
  emacs    = "\\\\b\\\\(rec\\\\|\\\\)\\\\b[ ]*"; 
  textmate = "";
  vim      = "rec\\W\\|";
}

let let_binding_match3: Core.regexp = {
  emacs    = "\\\\b\\\\([a-zA-Z$_][a-zA-Z0-9$_]*\\\\|\\\\)"; 
  textmate = "";
  vim      = "[a-zA-Z$_][a-zA-Z0-9$_]*";
}

let lambda_begin: Core.regexp = {
  emacs    = "\\\\b\\\\(fun\\\\)\\\\b";
  textmate = "";
  vim      = "\\(fun\\)\\W"
}

let lambda_end: Core.regexp = {
  emacs    = "(->)";
  textmate = "";
  vim      = "\\(->\\)"
}

let control_keywords_match: Core.regexp = {
  emacs    = "\\\\b\\\\(match\\\\|with\\\\|if\\\\|then\\\\|else\\\\|assert\\\\|failwith\\\\|begin\\\\|end\\\\|in\\\\)\\\\b";
  textmate = "";
  vim      = "\\<\\(match\\|with\\|if\\|then\\|else\\|assert\\|failwith\\|begin\\|end\\|in\\)\\>"
}

let operators_match: Core.regexp = {
  emacs    = "[ ]*\\\\(::\\\\|-\\\\|+\\\\|/\\\\|mod\\\\|land\\\\|lor\\\\|lxor\\\\|lsl\\\\|lsr\\\\|&&\\\\|||\\\\|<\\\\|>\\\\|<>\\\\|<=\\\\|>=\\\\)[ ]*";
  textmate = "";
  vim      = "\\<\\(::\\|-\\|+\\|/\\|mod\\|land\\|lor\\|lxor\\|lsl\\|lsr\\|&&\\|||\\|<\\|>\\|<>\\|<=\\|>=\\)\\>"
}

let module_match1: Core.regexp = {
  emacs    = "\\\\b\\\\([A-Z][a-zA-Z0-9_$]*\\\\)\\\\.";
  textmate = "";
  vim      = "\\<\\([A-Z][a-zA-Z0-9_$]*\\)\\."
}

let module_match2: Core.regexp = {
  emacs    = "\\\\([a-z_][a-zA-Z0-9_$]*\\\\)\\\\b";
  textmate = "";
  vim      = "[a-z_][a-zA-Z0-9_$]*"
}


let identifier_constructor_match: Core.regexp = {
  emacs    = "\\\\b\\\\([A-Z][a-zA-Z0-9_$]*\\\\)\\\\b";
  textmate = "";
  vim      = "\\<\\([A-Z][a-zA-Z0-9_$]*\\)\\s\\+"
}

let type_definition_match: Core.regexp = {
  emacs    = "\\\\b\\\\(type\\\\)\\\\b";
  textmate = "";
  vim      = "\\(type\\)\\>"
}

let type_annotation_match: Core.regexp = {
  emacs    = "\\\\(:[ ]*[^]=;\\\\):]*\\\\)";
  textmate = "";
  vim      = "\\(:[^]=;\\):]*\\)"
}

let multiplication_match: Core.regexp = {
  emacs    = "\\\\(*\\\\)";
  textmate = "";
  vim      = "\\(*\\)"
}