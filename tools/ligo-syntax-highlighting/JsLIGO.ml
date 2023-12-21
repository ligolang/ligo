module Core = SyntaxHighlighting.Core
module Helpers = SyntaxHighlighting.Helpers

module Regexp = struct
  include Regexp

  (* follow(property) = RBRACE COMMA *)
  let property_expr_begin : Core.regexp = colon_match

  let property_expr_end : Core.regexp =
    { (* FIXME: Emacs doesn't support positive look-ahead *)
      emacs = {r|,\\|;\\|}|r}
    ; textmate = {r|(?=,|;|})|r}
    ; vim = {r|\(,\|;\|}\)\@=|r}
    }

  (* follow(type_decl) = SEMI RBRACE Else EOF Default Case
     I added a few other cases that break because of ASI:
     Type Let Const Namespace Interface Export Import Attr *)
  let type_definition_begin : Core.regexp =
    { emacs = {|\\btype\\b|}; textmate = {|\b(type)\b|}; vim = {|\<type\>|} }

  let type_definition_end : Core.regexp =
    { (* FIXME: Emacs doesn't support positive look-ahead *)
      emacs = ""
    ; textmate =
        {r|(?=;|}|@|\b(else|default|case|type|let|const|namespace|interface|export|import|function)\b)|r}
    ; vim =
        {r|\(;\|}\|@\|\<\(else\|default\|case\|type\|let\|const\|namespace\|interface\|export\|import\|function\)\>\)\@=|r}
    }

  let type_name_match : Core.regexp =
    { emacs = {|\\b[a-zA-Z_][a-zA-Z0-9]\\*\\b|}
    ; textmate = {|\b([a-zA-Z_][a-zA-Z0-9_]*)\b|}
    ; vim = {|\<\([a-zA-Z_][a-zA-Z0-9_]*\)\>|}
    }

  let type_operator_match : Core.regexp =
    { emacs = {|\\(=>\\|\\.\\||\\)|}; textmate = {|(=>|\.|\|)|}; vim = {|\(=>\|\.\||\)|} }

  (* follow(type_annotation(type_expr)) = SEMI RPAR RBRACE PARAMS COMMA
     follow(type_annotation(__anonymous_6)) = LBRACE ARROW
     follow(type_annotation(__anonymous_2)) = EQ
     follow(ret_type) = LBRACE ARROW
     follow(binding_type) = EQ *)
  let type_annotation_begin : Core.regexp = colon_match

  let type_annotation_end : Core.regexp =
    { (* FIXME: Emacs doesn't support positive look-ahead *)
      emacs = {r|)\\|=>\\|,\\|{\\|}\\|=\\|;|r}
    ; textmate = {r|(?=\)|=>|,|{|}|=|;)|r}
    ; vim = {r|\()\|=>\|,\|{\|}\|=\|;\)\@=|r}
    }

  (* follow(field_decl) = RBRACE COMMA
     n.b.: For JsLIGO, both the FIRST and FOLLOW for both the type_annotation in
     a property and the "expression field assignment" are the same. *)
  let type_annotation_field_begin : Core.regexp = property_expr_begin
  let type_annotation_field_end : Core.regexp = property_expr_end

  (* follow(as_expr_level) = SEMI RPAR REM_EQ RBRACKET RBRACE PLUS_EQ MULT_EQ MINUS_EQ    Else EQ EOF Default DIV_EQ Case COMMA COLON As
     follow(type_expr)     = SEMI RPAR REM_EQ RBRACKET RBRACE PLUS_EQ MULT_EQ MINUS_EQ GT Else EQ EOF Default DIV_EQ Case COMMA COLON As ARROW
     Since `as_expr_level` also includes `disj_expr_level` as an alternative, we
     likely want the intersection of those two. Therefore:
     follow(as_expr_level) ∩ follow(type_expr) = follow(as_expr_level) *)
  let type_as_begin : Core.regexp =
    { emacs = {|\\bas\\b|}; textmate = {|\b(as)\b|}; vim = {|\<as\>|} }

  let type_as_end : Core.regexp =
    { (* FIXME: Emacs doesn't support positive look-ahead *)
      emacs = ""
    ; textmate = {r|(?=;|%=|\]|}|\+=|\*=|-=|=|/=|,|:|\b(else|default|case|as)\b)|r}
    ; vim =
        {r|\(;\|%=\|]\|}\|+=\|\*=\|-=\|=\|\/=\|,\|:\|\<\(else\|default\|case\|as\)\>\)\@=|r}
    }

  let attributes_match : Core.regexp =
    { emacs = {|\\(@[a-zA-Z][a-zA-Z0-9_:.@%]*\\)|}
    ; textmate = {|(@[a-zA-Z][a-zA-Z0-9_:.@%]*)|}
    ; vim = {|\(@[a-zA-Z][a-zA-Z0-9_:.@%]*\)|}
    }

  let value_binding_match : Core.regexp =
    { emacs = {|\\b\\(let\\|const\\)\\b|}
    ; textmate = {|\b(let|const)\b\s*|}
    ; vim = {|\<\(let\|const\)\>|}
    }

  let case_begin : Core.regexp =
    { emacs = {|\\b\\(case\\|default\\)\\b|}
    ; textmate = {|\b(case|default)\b|}
    ; vim = {|\<\(case\|default\)\>|}
    }

  let case_end : Core.regexp = colon_match

  let identifier_annotation_positive_lookahead : Core.regexp =
    { (* FIXME: Emacs doesn't support positive look-ahead *)
      emacs = {|\\b\\([a-zA-Z$_][a-zA-Z0-9$_]*\\)\\b[:space:]*:|}
    ; textmate = {|\b([a-zA-Z$_][a-zA-Z0-9$_]*)\b\s*(?=:)|}
    ; vim = {|\<\([a-zA-Z$_][a-zA-Z0-9$_]*\)\>\s*:\@=|}
    }

  (** Keywords that will be highlighted with [Keyword]. *)
  let keywords_match : Core.regexp =
    { emacs =
        {|\\b\\(export\\|import\\|from\\|contract_of\\|parameter_of\\|function\\|do\\|namespace\\|interface\\|implements\\|extends\\|false\\|true\\)\\b|}
    ; textmate =
        {|\b(export|import|from|contract_of|parameter_of|function|do|namespace|interface|implements|extends|false|true)\b|}
    ; vim =
        {|\<\(export\|import\|from\|contract_of\|parameter_of\|function\|do\|namespace\|interface\|implements\|extends\|false\|true\)\>|}
    }

  (** Keywords that will be highlighted with [Conditional]. *)
  let control_keywords_match : Core.regexp =
    { emacs =
        {|\\b\\(switch\\|if\\|else\\|for\\|of\\|while\\|return\\|break\\|continue\\|match\\)\\b|}
    ; textmate = {|\b(switch|if|else|for|of|while|return|break|continue|match)\b|}
    ; vim = {|\<\(switch\|if\|else\|for\|of\|while\|return\|break\|continue\|match\)\>|}
    }

  let operators_match : Core.regexp =
    { emacs =
        {|\\b\\(-\\|+\\|%\\|&&\\||\\||==\\|!=\\|<=\\|>=\\|<\\|>\\|\\*\\|/\\|=\\|!\\|\\*=\\|/=\\|%=\\|+=\\|-=\\)\\b|}
    ; textmate = {|\b(-|\+|%|&&|\|\||==|!=|<=|>=|<|>|\*|\/|=|!|\*=|\/=|%=|\+=|-=)\b|}
    ; vim =
        {|\<\(-\|+\|%\|&&\||\||==\|!=\|<=\|>=\|<\|>\|\*\|/\|=\|!\|\*=\|/=\|%=\|+=\|-=\)\>|}
    }

  let module_alias_match1 : Core.regexp =
    { emacs = {|\\b\\(import\\)\\b|}
    ; textmate = {|\b(import)\b|}
    ; vim = {|\<\(import\)\>|}
    }

  let module_alias_match2 : Core.regexp =
    { emacs = {|\\b\\([A-Z][a-zA-Z0-9_$]*\\)\\b|}
    ; textmate = {|\b([A-Z][a-zA-Z0-9_$]*)\b|}
    ; vim = {|\<\([A-Z][a-zA-Z0-9_$]*\)\>|}
    }

  let module_match1 : Core.regexp =
    { emacs = {|\\b\\([A-Z][a-zA-Z0-9_$]*\\)\\.|}
    ; textmate = {|\b([A-Z][a-zA-Z0-9_$]*)\.|}
    ; vim = {|\<[A-Z][a-zA-Z0-9_$]*\.|}
    }

  let module_match2 : Core.regexp =
    { emacs = {|\\b\\([a-zA-Z0-9_$]*\\)\\b|}
    ; textmate = {|\b([a-zA-Z0-9_$]*)\b|}
    ; vim = {|\<\([a-zA-Z0-9_$]*\)\>|}
    }

  let string_literal_match : Core.regexp =
    { emacs = {|\\\"\\.*\\\"|}; textmate = {|(\".*\")|}; vim = {|\".*\"|} }

  let ternary_begin : Core.regexp = { emacs = {|?|}; textmate = {|(\?)|}; vim = {|?|} }
  let ternary_end : Core.regexp = colon_match

  let when_begin : Core.regexp =
    { emacs = {|\\bwhen\\b|}; textmate = {|\b(when)\b|}; vim = {|\<when\>|} }

  let when_end : Core.regexp = colon_match

  let property_ctor_match : Core.regexp =
    { (* FIXME: Emacs doesn't support positive look-ahead *)
      emacs = ""
    ; textmate = identifier_constructor_match.textmate ^ {|(?=\s*:)|}
    ; vim = identifier_constructor_match.vim ^ {|\(\s*:\)\@=|}
    }

  let property_int_match : Core.regexp =
    { (* FIXME: Emacs doesn't support positive look-ahead *)
      emacs = ""
    ; textmate = int_literal_match.textmate ^ {|(?=\s*:)|}
    ; vim = int_literal_match.vim ^ {|\(\s*:\)\@=|}
    }

  let property_string_match : Core.regexp =
    { (* FIXME: Emacs doesn't support positive look-ahead *)
      emacs = ""
    ; textmate = string_literal_match.textmate ^ {|(?=\s*:)|}
    ; vim = string_literal_match.vim ^ {|\(\s*:\)\@=|}
    }

  let property_match : Core.regexp =
    { (* FIXME: Emacs doesn't support positive look-ahead *)
      emacs = ""
    ; textmate = {|\b([a-zA-Z$_][a-zA-Z0-9$_]*)\b(?=\s*:)|}
    ; vim = {|\<\([a-zA-Z$_][a-zA-Z0-9$_]*\)\>\(\s*:\)\@=|}
    }
end

module Name = struct
  let macro = "macro"
  let let_binding = "letbinding"
  let keywords = "keywords"
  let control_keywords = "controlkeywords"
  let numeric_literals = "numericliterals"
  let operators = "operators"
  let semicolon = "semicolon"
  let comma = "comma"
  let lowercase_identifier = "lowercaseidentifier"
  let uppercase_identifier = "uppercaseidentifier"
  let module_access = "moduleaccess"
  let module_alias = "modulealias"
  let object_or_block = "objectorblock"
  let object_property = "objectproperty"
  let object_property_ctor = "objectpropertyctor"
  let object_property_int = "objectpropertyint"
  let object_property_string = "objectpropertystring"
  let attribute = "attribute"
  let parentheses = "parentheses"
  let case = "case"
  let ternary = "ternary"
  let when_clause = "whenclause"

  (* Types *)
  let type_binder = "typebinder"
  let type_definition = "typedefinition"
  let type_annotation = "typeannotation"
  let type_annotation_field = "typeannotationfield"
  let type_as = "typeas"
  let type_name = "typename"
  let type_generic = "typegeneric"
  let type_parentheses = "typeparentheses"
  let type_operator = "typeoperator"
  let type_int = "typeint"
  let type_variant = "typevariant"
  let type_product = "typeproduct"
  let type_fun_param = "typefunparam"
end

let syntax_highlighting =
  let open Core in
  let type_core_patterns =
    [ Name_ref Name.keywords
    ; Name_ref Name.uppercase_identifier
    ; Name_ref Name.type_operator
    ; Name_ref Name.type_name
    ; Name_ref Name.type_parentheses
    ; Name_ref Name.type_int
    ; Name_ref Name.type_variant
    ; Name_ref Name.type_product
    ; Name_ref Name.type_binder
    ; Name_ref Name.type_generic
    ; String_ref
    ]
  in
  { syntax_name = "JsLIGO"
  ; alt_name = "javascript"
  ; scope_name = "source.jsligo"
  ; file_types = [ "jsligo" ]
  ; folding_start_marker = Some "{"
  ; folding_stop_marker = Some "}"
  ; language_features =
      { operators =
          [ "!"
          ; "%"
          ; "+"
          ; "-"
          ; "*"
          ; "/"
          ; "=="
          ; "!="
          ; ">="
          ; "<="
          ; ">"
          ; "<"
          ; "#"
          ; "||"
          ; "&&"
          ; "="
          ; "*="
          ; "/="
          ; "%="
          ; "+="
          ; "-="
          ]
      ; string_delimiters =
          [ { emacs = "\\\""; textmate = "\\\""; vim = "\\\"" }
          ; { emacs = "`"; textmate = "`"; vim = "`" }
          ]
      ; comments =
          { line_comment = { emacs = "//"; textmate = "\\/\\/"; vim = "\\/\\/" }
          ; block_comment =
              ( { emacs = "\\/*"; textmate = "/\\*"; vim = "/\\*" }
              , { emacs = "*\\/"; textmate = "\\*\\/"; vim = "\\*/" } )
          }
      ; comments_insertion = { line_comment = "//"; block_comment = "/*", "*/" }
      ; extra_patterns =
          { (* FIXME: The highlighting for attributes is a bit more liberal, and may
           catch false negatives. We preferred this for the simplicity of the
           solution for now.
         *)
            in_line_comments = [ Name_ref Name.attribute ]
          ; in_block_comments = [ Name_ref Name.attribute ]
          ; in_strings = []
          }
      ; brackets = [ "{", "}"; "[", "]"; "(", ")" ]
      ; auto_closing_pairs =
          [ "{", "}"; "[", "]"; "(", ")"; "\"", "\""; "'", "'"; "`", "`" ]
      ; surrounding_pairs =
          [ "{", "}"; "[", "]"; "(", ")"; "\"", "\""; "'", "'"; "`", "`" ]
      ; syntax_table = [ "*", ". 23"; "\n", "> b"; "/", ". 124b" ]
      }
  ; syntax_patterns =
      [ (* TODO: Name_ref Name.lowercase_identifier; *)
        Name_ref Name.attribute
      ; Name_ref Name.uppercase_identifier
      ; Name_ref Name.macro
      ; Name_ref Name.let_binding
      ; Name_ref Name.type_definition
      ; Name_ref Name.keywords
      ; Name_ref Name.control_keywords
      ; Name_ref Name.numeric_literals
      ; Name_ref Name.operators
      ; Name_ref Name.module_alias
      ; Name_ref Name.type_annotation
      ; Name_ref Name.type_as
      ; Name_ref Name.object_or_block
      ; Name_ref Name.parentheses
      ; Name_ref Name.case
      ; Name_ref Name.ternary
      ; Name_ref Name.when_clause
      ]
  ; repository =
      [ { name = Name.attribute
        ; kind =
            Match
              { match_name = Some Attribute; match_ = [ Regexp.attributes_match, None ] }
        }
      ; Helpers.macro
      ; { name = Name.let_binding
        ; kind =
            Match
              { match_name = None; match_ = [ Regexp.value_binding_match, Some Keyword ] }
        }
      ; { name = Name.keywords
        ; kind =
            Match { match_name = None; match_ = [ Regexp.keywords_match, Some Keyword ] }
        }
      ; { name = Name.control_keywords
        ; kind =
            Match
              { match_name = Some Conditional
              ; match_ = [ Regexp.control_keywords_match, None ]
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
      ; { name = Name.comma
        ; kind = Match { match_name = None; match_ = [ Regexp.comma_match, None ] }
        }
      ; { (* Otherwise [:] is interpreted as a type annotation. *)
          name = Name.ternary
        ; kind =
            Begin_end
              { meta_name = None
              ; begin_ = [ Regexp.ternary_begin, Some Operator ]
              ; end_ = [ Regexp.ternary_end, Some Operator ]
              ; patterns = [ Self_ref ]
              }
        }
      ; { (* Otherwise [when ...:] is interpreted as a type annotation. *)
          name = Name.when_clause
        ; kind =
            Begin_end
              { meta_name = None
              ; begin_ = [ Regexp.when_begin, Some Conditional ]
              ; end_ = [ Regexp.when_end, Some Operator ]
              ; patterns = [ Self_ref ]
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
      ; { name = Name.module_access
        ; kind =
            Match
              { match_ =
                  [ Regexp.module_match1, Some Structure
                  ; Regexp.module_match2, Some Identifier
                  ]
              ; match_name = None
              }
        }
      ; { name = Name.module_alias
        ; kind =
            Match
              { match_ =
                  [ Regexp.module_alias_match1, Some Conditional
                  ; Regexp.module_alias_match2, Some Structure
                  ]
              ; match_name = None
              }
        }
      ; { name = Name.object_or_block
        ; kind =
            Begin_end
              { meta_name = None
              ; begin_ = [ Regexp.braces_begin, None ]
              ; end_ = [ Regexp.braces_end, None ]
              ; patterns =
                  [ Name_ref Name.object_property_ctor
                  ; Name_ref Name.object_property_int
                  ; Name_ref Name.object_property_string
                  ; Name_ref Name.object_property
                  ; Name_ref Name.comma
                  ; Self_ref
                  ]
              }
        }
      ; { name = Name.parentheses
        ; kind =
            Begin_end
              { meta_name = None
              ; begin_ = [ Regexp.parentheses_begin, None ]
              ; end_ = [ Regexp.parentheses_end, None ]
              ; patterns = [ Name_ref Name.type_fun_param; Name_ref Name.comma; Self_ref ]
              }
        }
      ; { name = Name.case
        ; kind =
            Begin_end
              { meta_name = None
              ; begin_ = [ Regexp.case_begin, Some Conditional ]
              ; end_ = [ Regexp.case_end, Some Operator ]
              ; patterns = [ Self_ref ]
              }
        }
      ; { name = Name.object_property_ctor
        ; kind =
            Begin_end
              { meta_name = None
              ; begin_ =
                  [ Regexp.property_ctor_match, Some Label
                  ; Regexp.property_expr_begin, Some Operator
                  ]
              ; end_ = [ Regexp.property_expr_end, None ]
              ; patterns = [ Self_ref ]
              }
        }
      ; { name = Name.object_property_int
        ; kind =
            Begin_end
              { meta_name = None
              ; begin_ =
                  [ Regexp.property_int_match, Some Number
                  ; Regexp.property_expr_begin, Some Operator
                  ]
              ; end_ = [ Regexp.property_expr_end, None ]
              ; patterns = [ Self_ref ]
              }
        }
      ; { name = Name.object_property_string
        ; kind =
            Begin_end
              { meta_name = None
              ; begin_ =
                  [ Regexp.property_string_match, Some String
                  ; Regexp.property_expr_begin, Some Operator
                  ]
              ; end_ = [ Regexp.property_expr_end, None ]
              ; patterns = [ Self_ref ]
              }
        }
      ; { name = Name.object_property
        ; kind =
            Begin_end
              { meta_name = None
              ; begin_ =
                  [ Regexp.property_match, Some Identifier
                  ; Regexp.property_expr_begin, Some Operator
                  ]
              ; end_ = [ Regexp.property_expr_end, None ]
              ; patterns = [ Self_ref ]
              }
        }
      ; (* Types *)
        { name = Name.type_binder
        ; kind =
            Begin_end
              { meta_name = None
              ; begin_ = [ Regexp.chevron_begin, None ]
              ; end_ = [ Regexp.chevron_end, None ]
              ; patterns = [ Name_ref Name.type_name ]
              }
        }
      ; { name = Name.type_definition
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
      ; { name = Name.type_annotation_field
        ; kind =
            Begin_end
              { meta_name = None
              ; begin_ = [ Regexp.type_annotation_field_begin, Some Operator ]
              ; end_ = [ Regexp.type_annotation_field_end, None ]
              ; patterns = type_core_patterns
              }
        }
      ; { name = Name.type_as
        ; kind =
            Begin_end
              { meta_name = None
              ; begin_ = [ Regexp.type_as_begin, Some Keyword ]
              ; end_ = [ Regexp.type_as_end, None ]
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
      ; { name = Name.type_generic
        ; kind =
            Begin_end
              { meta_name = None
              ; begin_ = [ Regexp.chevron_begin, None ]
              ; end_ = [ Regexp.chevron_end, None ]
              ; patterns = Name_ref Name.comma :: type_core_patterns
              }
        }
      ; (*
        n.b.: It's difficult to disambiguate `(_)` (`par(type_expr)`) and
        `(_) => _` (`fun_type`), as the latter would cause an identifier in a
        `fun_param` to be highlighted as a type. We use a workaround as follows:
        * For a type function, don't consume a `(`, but check that we have a
          `=>` at the end. Instead, consume a `(` only with a `type_parenteheses`.
        * A function argument _always_ has a `type_annotation`, so require a
          special case of the identifier that expects a `:` after it, with an
          arbitrary number of white space. Note that this doesn't handle
          comments.
        * Have a `type_parentheses` accept identifiers that check for the type
          annotation as a positive lookahead, as well as a `type_annotation`.
        * Let `=>` be handled by `type_operator`.
        This makes `type_parentheses` act as if it was the union of the two
        ambiguous expressions. It's good enough!
      *)
        { name = Name.type_fun_param
        ; kind =
            Match
              { match_name = None
              ; match_ =
                  [ Regexp.identifier_annotation_positive_lookahead, Some Identifier ]
              }
        }
      ; { name = Name.type_name
        ; kind =
            Match
              { (* n.b.: We use Type instead of Type_var since we can't easily
             disambiguate between the two in JsLIGO. *)
                match_name = Some Type
              ; match_ = [ Regexp.type_name_match, None ]
              }
        }
      ; { name = Name.type_parentheses
        ; kind =
            Begin_end
              { meta_name = None
              ; begin_ = [ Regexp.parentheses_begin, None ]
              ; end_ = [ Regexp.parentheses_end, None ]
              ; patterns =
                  Name_ref Name.type_fun_param
                  :: Name_ref Name.type_annotation
                  :: Name_ref Name.comma
                  :: type_core_patterns
              }
        }
      ; { name = Name.type_int
        ; kind =
            Match
              { match_name = Some Number; match_ = [ Regexp.int_literal_match, None ] }
        }
      ; (*
        n.b.: Like `type_parentheses`, we join JsLIGO's `variant` and
        `type_tuple` in one thing, otherwise it would unduly complicate the
        logic to disambiguate the two. The pipe (`|`) for the `sum_type` is
        handled by `type_operator`.
      *)
        { name = Name.type_variant
        ; kind =
            Begin_end
              { meta_name = None
              ; begin_ = [ Regexp.brackets_begin, None ]
              ; end_ = [ Regexp.brackets_end, None ]
              ; patterns = Name_ref Name.comma :: type_core_patterns
              }
        }
      ; { name = Name.type_product
        ; kind =
            Begin_end
              { meta_name = None
              ; begin_ = [ Regexp.braces_begin, None ]
              ; end_ = [ Regexp.braces_end, None ]
              ; patterns =
                  [ Name_ref Name.lowercase_identifier
                  ; Name_ref Name.type_annotation_field
                  ; Name_ref Name.comma
                  ]
              }
        }
      ]
  }
