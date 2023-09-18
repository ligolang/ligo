module Core = SyntaxHighlighting.Core
module Helpers = SyntaxHighlighting.Helpers

module Regexp = struct
  include Regexp

  let module_keyword_match : Core.regexp =
    { emacs = {|\\bmodule\\b|}; textmate = {|\b(module)\b|}; vim = {|\<module\>|} }

  (** Keywords that will be highlighted with [Keyword]. *)
  let keywords_match : Core.regexp =
    { emacs = {|\\b\\(struct\\|end\\|let\\|in\\|mut\\|rec\\)\\b|}
    ; textmate = {|\b(struct|end|let|in|mut|rec)\b|}
    ; vim = {|\<\(struct\|end\|let\|in\|mut\|rec\)\>|}
    }

  let operators_match : Core.regexp =
    { emacs =
        {|::\\|-\\|+\\|/\\|\\b\\(mod\\|land\\|lor\\|lxor\\|lsl\\|lsr\\)\\b\\|&&\\|||\\|<\\|>\\|<>\\|<=\\|>=\\||>|}
    ; textmate = {|::|\-|\+|\b(mod|land|lor|lxor|lsl|lsr)\b|&&|\|\||>|<>|<=|=>|<|>|\|>|}
    ; vim =
        {|::\|-\|+\|/\|\<\(mod\|land\|lor\|lxor\|lsl\|lsr\)\>\|&&\|||\|<\|>\|<>\|<=\|>=\||>|}
    }

  (* Types *)

  (* A type declaration may appear in the following places:
     * At top-level or in a module, immediately before a {|let|}, {|#|} (directive),
       {|module|}, {|type|}, {|end|}, {|[@|} (attribute) or EOF.
     * Locally inside a {|let|}, immediately before an {|in|}.

     follow(type_decl) = Type Module Let In End EOF Directive Attr
     Heads-up for an extra token: {|)|}. This is for parametric types. *)
  let type_definition_begin : Core.regexp =
    { emacs = {|\\btype\\b|}; textmate = {|\b(type)\b|}; vim = {|\<type\>|} }

  let type_definition_end : Core.regexp =
    { (* FIXME: Emacs doesn't support positive look-ahead... too bad! *)
      emacs = {|^#\\|\\[@\\|\\b\\(let\\|in\\|type\\|end\\|module\\)\\|)\\b|}
    ; textmate = {|(?=^#|\[@|\b(let|in|type|end|module)\b|\))|}
    ; vim = {|\(^#\|\[@\|\<\(let\|in\|type\|end\|module\)\>\|)\)\@=|}
    }

  let type_name_match : Core.regexp =
    { emacs = {|\\b[a-z_][a-zA-Z0-9]\\*\\b|}
    ; textmate = {|\b([a-z_][a-zA-Z0-9_]*)\b|}
    ; vim = {|\<[a-z_][a-zA-Z0-9_]*\>|}
    }

  let type_var_match : Core.regexp =
    { emacs = {|'|} ^ type_name_match.emacs
    ; textmate = {|'|} ^ type_name_match.textmate
    ; vim = {|'|} ^ type_name_match.vim
    }

  let type_operator_match : Core.regexp =
    { emacs = {|\\(->\\|\\.\\|\\*\\||\\)|}
    ; textmate = {|(->|\.|\*|\|)|}
    ; vim = {|\(->\|\.\|\*\||\)|}
    }

  (* A type annotation may appear in the following places:
     * In the return type of a declaration, immediately before a =.
     * In a function parameter, immediately before a ).
     * In a field of a record, immediately before a ; or a }.
     * In an arbitrary pattern, immediately before a ).

     follow(field_decl) = SEMI RBRACE
     follow(type_annotation(type_expr)) = RPAR EQ
     follow(type_annotation(lambda_app_type)) = ARROW *)
  let type_annotation_begin : Core.regexp = colon_match

  let type_annotation_end : Core.regexp =
    { (* FIXME: Emacs doesn't support positive look-ahead *)
      emacs = {r|)\\|=\\|;\\|}|r}
    ; textmate = {r|(?=\)|=|;|})|r}
    ; vim = {r|\()\|=\|;\|}\)\@=|r}
    }

  let type_annotation_begin_lambda : Core.regexp = type_annotation_begin

  let type_annotation_end_lambda : Core.regexp =
    { (* FIXME: Emacs doesn't support positive look-ahead *)
      emacs = {r|)\\|=\\|;\\|}\\|->|r}
    ; textmate = {r|(?=\)|=|;|}|->)|r}
    ; vim = {r|\()\|=\|;\|}\|->\)\@=|r}
    }

  let type_field_annotation_begin : Core.regexp = type_annotation_begin

  let type_field_annotation_end : Core.regexp =
    { (* FIXME: Emacs doesn't support positive look-ahead *)
      emacs = {|\\(}\\|;\\)|}
    ; textmate = {|(?=}|;)|}
    ; vim = {|\(}\|;\)\@=|}
    }

  let lambda_begin : Core.regexp =
    { emacs = {|\\b\\(fun\\)\\b|}; textmate = {|\b(fun)\b|}; vim = {|\<fun\>|} }

  let lambda_end : Core.regexp =
    { emacs = {|(->)|}; textmate = {|(->)|}; vim = {|\(->\)|} }

  let of_keyword_match : Core.regexp =
    { emacs = {|\\b\\(of\\)\\b|}; textmate = {|\b(of)\b|}; vim = {|\<\(of\)\>|} }

  (** Keywords that will be highlighted with [Conditional]. *)
  let control_keywords_match : Core.regexp =
    { emacs =
        {|\\b\\(match\\|with\\|if\\|then\\|else\\|assert\\|failwith\\|begin\\|for\\|upto\\|downto\\|do\\|while\\|done\\)\\b|}
    ; textmate =
        {|\b(match|with|if|then|else|assert|failwith|begin|for|upto|downto|do|while|done)\b|}
    ; vim =
        {|\<\(match\|with\|if\|then\|else\|assert\|failwith\|begin\|for\|upto\|downto\|do\|while\|done\)\>|}
    }
end

module Name = struct
  let macro = "macro"
  let lambda = "lambda"
  let control_keywords = "controlkeywords"
  let keywords = "keywords"
  let numeric_literals = "numericliterals"
  let operators = "operators"
  let semicolon = "semicolon"
  let of_keyword = "ofkeyword"
  let lowercase_identifier = "lowercaseidentifier"
  let uppercase_identifier = "uppercaseidentifier"
  let module_declaration = "moduledeclaration"
  let attribute = "attribute"

  (* Types *)
  let type_definition = "typedefinition"
  let type_annotation = "typeannotation"
  let type_annotation_lambda = "typeannotationlambda"
  let type_name = "typename"
  let type_var = "typevar"
  let type_parentheses = "typeparentheses"
  let type_operator = "typeoperator"
  let type_int = "typeint"
  let type_product = "typeproduct"
end

let syntax_highlighting =
  let open Core in
  let type_core_patterns =
    [ Name_ref Name.uppercase_identifier
    ; (* Sum type *)
      Name_ref Name.of_keyword
    ; Name_ref Name.type_operator
    ; Name_ref Name.type_name
    ; Name_ref Name.type_var
    ; Name_ref Name.type_parentheses
    ; Name_ref Name.type_int
    ; Name_ref Name.type_product
    ; String_ref
    ]
  in
  { syntax_name = "mligo"
  ; alt_name = "caml"
  ; scope_name = "source.mligo"
  ; file_types = [ "mligo" ]
  ; folding_start_marker = None
  ; folding_stop_marker = None
  ; language_features =
      { operators =
          [ "not"
          ; "mod"
          ; "ediv"
          ; "+"
          ; "-"
          ; "*"
          ; "/"
          ; "=="
          ; "<>"
          ; ">="
          ; "<="
          ; ">"
          ; "<"
          ; "#"
          ; "or"
          ; "and"
          ; "^"
          ; "|>"
          ]
      ; string_delimiters = [ { emacs = {|\"|}; textmate = {|\"|}; vim = {|\"|} } ]
      ; comments =
          { line_comment = { emacs = {|//|}; textmate = {|\/\/|}; vim = {|\/\/|} }
          ; block_comment =
              ( { emacs = {|(*|}; textmate = {|\(\*|}; vim = {|(\*|} }
              , { emacs = {|*)|}; textmate = {|\*\)|}; vim = {|\*)|} } )
          }
      ; comments_insertion = { line_comment = "//"; block_comment = "(*", "*)" }
      ; extra_patterns =
          { in_line_comments = []; in_block_comments = []; in_strings = [] }
      ; brackets = [ "{", "}"; "[", "]"; "(", ")" ]
      ; auto_closing_pairs = [ "{", "}"; "[", "]"; "(", ")"; "\"", "\""; "'", "'" ]
      ; surrounding_pairs = [ "{", "}"; "[", "]"; "(", ")"; "\"", "\""; "'", "'" ]
      ; syntax_table =
          [ "\n", "> b"; "/", ". 12b"; "*", ". 23"; "\\(", "()1n"; "\\)", ")(4n" ]
      }
  ; syntax_patterns =
      [ (* TODO: Name.lowercase_identifier; *)
        Name_ref Name.uppercase_identifier
      ; Name_ref Name.attribute
      ; Name_ref Name.macro
      ; Name_ref Name.lambda
      ; Name_ref Name.type_definition
      ; Name_ref Name.control_keywords
      ; Name_ref Name.module_declaration
      ; Name_ref Name.keywords
      ; Name_ref Name.numeric_literals
      ; Name_ref Name.operators
      ; Name_ref Name.type_annotation
      ]
  ; repository =
      [ Helpers.attribute
      ; Helpers.macro
      ; { name = Name.control_keywords
        ; kind =
            Match
              { match_name = Some Conditional
              ; match_ = [ Regexp.control_keywords_match, None ]
              }
        }
      ; { name = Name.keywords
        ; kind =
            Match { match_name = Some Keyword; match_ = [ Regexp.keywords_match, None ] }
        }
      ; { name = Name.module_declaration
        ; kind =
            Match
              { match_name = None
              ; match_ = [ Regexp.module_keyword_match, Some Keyword ]
              }
        }
      ; Helpers.numeric_literals
      ; { name = Name.operators
        ; kind =
            Match
              { match_name = Some Operator; match_ = [ Regexp.operators_match, None ] }
        }
      ; { name = Name.semicolon
        ; kind = Match { match_name = None; match_ = [ Regexp.semicolon_match, None ] }
        }
      ; { name = Name.of_keyword
        ; kind =
            Match
              { match_name = None; match_ = [ Regexp.of_keyword_match, Some Keyword ] }
        }
      ; { name = Name.lambda
        ; kind =
            Begin_end
              { meta_name = None
              ; begin_ = [ Regexp.lambda_begin, Some Statement ]
              ; end_ = [ Regexp.lambda_end, Some Operator ]
              ; patterns = [ Name_ref Name.type_annotation_lambda ]
              }
        }
      ; { name = Name.uppercase_identifier
        ; kind =
            Match
              { match_name = None
              ; match_ = [ Regexp.identifier_constructor_match, Some Structure ]
              }
        }
      ; { name = Name.lowercase_identifier
        ; kind =
            Match
              { match_name = None; match_ = [ Regexp.identifier_match, Some Identifier ] }
        }
      ; (* Types *)
        { name = Name.type_definition
        ; kind =
            Begin_end
              { meta_name = None
              ; begin_ = [ Regexp.type_definition_begin, Some Keyword ]
              ; end_ = [ Regexp.type_definition_end, None ]
              ; patterns = type_core_patterns
              }
        }
      ; { name = Name.type_annotation
        ; kind =
            Begin_end
              { meta_name = None
              ; begin_ = [ Regexp.type_annotation_begin, Some Operator ]
              ; end_ = [ Regexp.type_annotation_end, None ]
              ; patterns = type_core_patterns
              }
        }
      ; { name = Name.type_annotation_lambda
        ; kind =
            Begin_end
              { meta_name = None
              ; begin_ = [ Regexp.type_annotation_begin_lambda, Some Operator ]
              ; end_ = [ Regexp.type_annotation_end_lambda, None ]
              ; patterns = type_core_patterns
              }
        }
      ; { name = Name.type_operator
        ; kind =
            Match
              { match_name = Some Operator
              ; match_ = [ Regexp.type_operator_match, None ]
              }
        }
      ; { name = Name.type_name
        ; kind =
            Match { match_name = Some Type; match_ = [ Regexp.type_name_match, None ] }
        }
      ; { name = Name.type_var
        ; kind =
            Match { match_name = Some Type_var; match_ = [ Regexp.type_var_match, None ] }
        }
      ; { name = Name.type_parentheses
        ; kind =
            Begin_end
              { meta_name = None
              ; begin_ = [ Regexp.parentheses_begin, None ]
              ; end_ = [ Regexp.parentheses_end, None ]
              ; patterns = type_core_patterns
              }
        }
      ; { name = Name.type_int
        ; kind =
            Match
              { match_name = Some Number; match_ = [ Regexp.int_literal_match, None ] }
        }
      ; { name = Name.type_product
        ; kind =
            Begin_end
              { meta_name = None
              ; begin_ = [ Regexp.braces_begin, None ]
              ; end_ = [ Regexp.braces_end, None ]
              ; patterns =
                  [ Name_ref Name.uppercase_identifier
                  ; Name_ref Name.type_annotation
                  ; Name_ref Name.semicolon
                  ]
              }
        }
      ]
  }
