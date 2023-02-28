module Core     = SyntaxHighlighting.Core
module Helpers  = SyntaxHighlighting.Helpers

module Name = struct
  let attribute              = "attribute"
  let macro                  = "macro"
  let control_keywords       = "controlkeywords"
  let function_              = "function"
  let operators              = "operators"
  let semicolon              = "semicolon"
  let of_keyword             = "ofkeyword"
  let is_keyword             = "iskeyword"
  let lowercase_identifier   = "lowercaseidentifier"
  let uppercase_identifier   = "uppercaseidentifier"
  let module_declaration     = "moduledeclaration"
  let const_or_var           = "constorvar"
  let numeric_literals       = "numericliterals"
  (* Types *)
  let type_binder            = "typebinder"
  let type_definition        = "typedefinition"
  let type_annotation        = "typeannotation"
  let type_annotation_field  = "typeannotationfield"
  let type_name              = "typename"
  let type_parentheses       = "typeparentheses"
  let type_product           = "typeproduct"
  let type_operator          = "typeoperator"
  let type_module            = "typemodule"
  let type_int               = "typeint"
end

let syntax_highlighting =
  let open Core in
  let type_core_patterns = [
    Name.uppercase_identifier;
    (* Sum type *)
    Name.of_keyword;

    Name.type_product;
    Name.type_operator;
    Name.type_name;
    Name.type_parentheses;
    Name.type_int;
    "string";
  ] in
  {
    syntax_name          = "ligo";
    alt_name             = "pascal";
    scope_name           = "source.ligo";
    file_types           = ["ligo"; "pligo"];
    folding_start_marker = None;
    folding_stop_marker = None;
    language_features = {
      operators = [
        "not";
        "mod";
        "ediv";
        "+";
        "-";
        "*";
        "/";
        "==";
        "=/=";
        ">=";
        "<=";
        ">";
        "<";
        "#";
        "or";
        "and";
        "^"
      ];
      string_delimiters = [
        {
          emacs    = "\\\"";
          textmate = "\\\"";
          vim      = "\\\""
        }
      ];
      comments = {
        line_comment = {
          emacs    = "//";
          textmate = "\\/\\/";
          vim      = "\\/\\/"
        };
        block_comment = (
          {
            emacs    = "(*";
            textmate = "\\(\\*";
            vim      = "(\\*"
          },
          {
            emacs    = "*)";
            textmate = "\\*\\)";
            vim      = "\\*)"
          }
        );
      };
      comments_insertion = {
        line_comment = "//";
        block_comment = ("(*", "*)");
      };
      extra_patterns = {
        in_line_comments = [];
        in_block_comments = [];
        in_strings = [];
      };
      brackets = [
        ("{", "}");
        ("[", "]");
        ("(", ")");
      ];
      auto_closing_pairs = [
        ("{", "}");
        ("[", "]");
        ("(", ")");
        ("\"", "\"");
        ("'", "'");
        ("(*", "*)");
        ("begin", "end");
      ];
      surrounding_pairs = [
        ("{", "}");
        ("[", "]");
        ("(", ")");
        ("\"", "\"");
        ("'", "'");
        ("(*", "*)");
        ("begin", "end");
      ];
      syntax_table = [
        ("\\n", "> b");
        ("/", ". 12b");
        ("*", ". 23");
        ("\\(", "()1n");
        ("\\)", ")(4n");
      ]
    };
    syntax_patterns = [
      (* TODO: Name.lowercase_identifier; *)
      Name.type_binder;
      Name.uppercase_identifier;
      Name.attribute;
      Name.macro;
      Name.control_keywords;
      Name.module_declaration;
      Name.function_;
      Name.operators;
      Name.type_definition;
      Name.const_or_var;
      Name.numeric_literals;
      Name.type_annotation;
    ];
    repository = [
      Helpers.attribute;
      Helpers.macro;
      {
        name = Name.control_keywords;
        kind = Match {
          match_name = Some Conditional;
          match_ = [(Regexp.control_keywords_match_ligo, None)]
        }
      };
      {
        name = Name.module_declaration;
        kind = Match {
          match_name = None;
          match_ = [(Regexp.module_keyword_match, Some Keyword)];
        }
      };
      {
        name = Name.function_;
        kind = Match {
          match_ = [
            (Regexp.let_binding_match1_ligo, Some Keyword);
            (Regexp.let_binding_match2_ligo, Some FunctionName)
          ];
          match_name = None
        }
      };
      Helpers.numeric_literals;
      {
        name = Name.operators;
        kind = Match {
          match_name = Some Operator;
          match_ = [(Regexp.operators_match_ligo, None)]
        }
      };
      {
        name = Name.semicolon;
        kind = Match {
          match_name = None;
          match_ = [(Regexp.semicolon_match, None)];
        }
      };
      {
        name = Name.of_keyword;
        kind = Match {
          match_name = None;
          match_ = [(Regexp.of_keyword_match, Some Keyword)];
        }
      };
      {
        name = Name.is_keyword;
        kind = Match {
          match_name = None;
          match_ = [(Regexp.is_keyword_match, Some Keyword)];
        }
      };
      {
        name = Name.uppercase_identifier;
        kind = Match {
          match_name = None;
          match_     = [(Regexp.identifier_constructor_match, Some Structure)];
        }
      };
      {
        name = Name.lowercase_identifier;
        kind = Match {
          match_name = None;
          match_     = [(Regexp.identifier_match, Some Identifier)];
        }
      };
      {
        name = Name.const_or_var;
        kind = Match {
          match_name = None;
          match_     = [(Regexp.const_or_var, Some Keyword)]
        }
      };
      (* Types *)
      {
        name = Name.type_binder;
        kind = Begin_end {
          meta_name = None;
          begin_ = [
            (Regexp.chevron_begin, None);
            (*
              FIXME: Breaks with type binders spanning multiple lines and
              comments.
                Until patterns are supported, it's very difficult to
              disambiguate type params and less than/greater than expressions!
              So we use an approximate solution for now.
                To fix this, add support for patterns, make regions out of
              `const`/`let/`function` declarations (and `function` expressions)
              and expect a type_binder right after a pattern.
            *)
            (Regexp.type_binder_positive_lookahead_ligo, None);
          ];
          end_ = [(Regexp.chevron_end, None)];
          patterns = [Name.type_name; Name.type_name];
        };
      };
      {
        name = Name.type_definition;
        kind = Begin_end {
          meta_name = None;
          begin_ = [(Regexp.type_definition_begin_ligo, Some Keyword)];
          end_ = [(Regexp.type_definition_end_ligo, None)];
          patterns = Name.is_keyword :: type_core_patterns;
        }
      };
      {
        name = Name.type_annotation;
        kind = Begin_end {
          meta_name = None;
          begin_ = [(Regexp.type_annotation_begin_ligo, Some Operator)];
          end_ = [(Regexp.type_annotation_end_ligo, None)];
          patterns = type_core_patterns;
        }
      };
      {
        name = Name.type_annotation_field;
        kind = Begin_end {
          meta_name = None;
          begin_ = [(Regexp.type_annotation_field_begin_ligo, Some Operator)];
          end_ = [(Regexp.type_annotation_field_end_ligo, None)];
          patterns = type_core_patterns;
        }
      };
      {
        name = Name.type_product;
        kind = Begin_end {
          meta_name = None;
          (* FIXME: `record\n`[ will not work properly in VS Code. We likely
             want to make `record` be a keyword, and always interpret a `[` at
             type-level as as a `type_record` regardless. *)
          begin_ = [
            (Regexp.record_keyword_match, Some Keyword);
            (Regexp.brackets_begin, None);
          ];
          end_ = [(Regexp.brackets_end, None)];
          patterns = [Name.lowercase_identifier; Name.type_annotation_field; Name.semicolon];
        }
      };
      {
        name = Name.type_operator;
        kind = Match {
          match_name = Some Operator;
          match_ = [(Regexp.type_operator_match_ligo, None)];
        }
      };
      {
        name = Name.type_name;
        kind = Match {
          match_name = Some Type;
          match_ = [(Regexp.type_name_match, None)];
        }
      };
      {
        name = Name.type_parentheses;
        kind = Begin_end {
          meta_name = None;
          begin_ = [(Regexp.parentheses_begin, None)];
          end_ = [(Regexp.parentheses_end, None)];
          patterns = type_core_patterns;
        }
      };
      {
        name = Name.type_int;
        kind = Match {
          match_name = Some Number;
          match_ = [(Regexp.int_literal_match, None)];
        }
      };
    ]
  }
