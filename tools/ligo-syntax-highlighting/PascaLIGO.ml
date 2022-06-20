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
  let module_                = "module"
  let identifier             = "identifier"
  let identifier_constructor = "identifierconstructor"
  let const_or_var           = "constorvar"
  let numeric_literals       = "numericliterals"
  (* Types *)
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
    (* Sum type *)
    Name.of_keyword;
    Name.identifier_constructor;

    Name.type_product;
    Name.type_operator;
    Name.type_name;
    Name.type_parentheses;
    Name.type_module;
    Name.type_int;
    "string";
  ] in
  {
    syntax_name          = "ligo";
    alt_name             = "pascal";
    scope_name           = "source.ligo";
    file_types           = [];
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
          textmate = "\\/\\/.*$";
          vim      = "\\/\\/.*$"
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
      Name.attribute;
      Name.macro;
      Name.control_keywords;
      Name.function_;
      Name.operators;
      Name.type_definition;
      Name.module_;
      Name.identifier_constructor;
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
        name = Name.module_;
        kind = Match {
          match_     = [
            (Regexp.module_match1, Some Structure);
            (Regexp.module_match2, Some Identifier)
          ];
          match_name = None
        }
      };
      {
        name = Name.identifier;
        kind = Match {
          match_name = None;
          match_ = [(Regexp.let_binding_match2_ligo, None)];
        }
      };
      {
        name = Name.identifier_constructor;
        kind = Match {
          match_name = None;
          match_     = [(Regexp.identifier_constructor_match, Some Label)]
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
          begin_ = [
            (Regexp.record_keyword_match, Some Keyword);
            (Regexp.whitespace_match, None);
            (Regexp.brackets_begin, None);
          ];
          end_ = [(Regexp.brackets_end, None)];
          patterns = [Name.identifier; Name.type_annotation_field; Name.semicolon];
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
        name = Name.type_module;
        kind = Match {
          match_name = Some Identifier;
          match_ = [(Regexp.module_match1, None)];
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
