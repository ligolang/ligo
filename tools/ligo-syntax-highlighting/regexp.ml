module Core = SyntaxHighlighting.Core

let identifier_match : Core.regexp =
  { emacs = {|\\b\\([a-z$_][a-zA-Z0-9$_]*\\)\\b|}
  ; textmate = {|\b([a-z$_][a-zA-Z0-9$_]*)\b|}
  ; vim = {|\<[a-z$_][a-zA-Z0-9$_]*\>|}
  }

let identifier_constructor_match : Core.regexp =
  { emacs = {|\\b\\([A-Z][a-zA-Z0-9_$]*\\)\\b|}
  ; textmate = {|\b([A-Z][a-zA-Z0-9_$]*)\b|}
  ; vim = {|\<[A-Z][a-zA-Z0-9_$]*\>|}
  }

let parentheses_begin : Core.regexp =
  { emacs = {|\\(|}; textmate = {|(\()|}; vim = {|(|} }

let parentheses_end : Core.regexp = { emacs = {|\\)|}; textmate = {|(\))|}; vim = {|)|} }
let brackets_begin : Core.regexp = { emacs = {|\\[|}; textmate = {|(\[)|}; vim = {|\[|} }
let brackets_end : Core.regexp = { emacs = {|\\]|}; textmate = {|(\])|}; vim = {|\]|} }
let braces_begin : Core.regexp = { emacs = {|{|}; textmate = {|({)|}; vim = {|{|} }
let braces_end : Core.regexp = { emacs = {|}|}; textmate = {|(})|}; vim = {|}|} }
let chevron_begin : Core.regexp = { emacs = {|<|}; textmate = {|(<)|}; vim = {|<|} }
let chevron_end : Core.regexp = { emacs = {|>|}; textmate = {|(>)|}; vim = {|>|} }
let semicolon_match : Core.regexp = { emacs = {|;|}; textmate = {|(;)|}; vim = {|;|} }
let comma_match : Core.regexp = { emacs = {|,|}; textmate = {|(,)|}; vim = {|,|} }
let colon_match : Core.regexp = { emacs = {|:|}; textmate = {|(:)|}; vim = {|:|} }

let int_literal_match : Core.regexp =
  { emacs = {|\\b\\([0-9]+\\)\\b|}; textmate = {|\b([0-9]+)\b|}; vim = {|\<[0-9]+\>|} }
