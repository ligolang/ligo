module Core     = SyntaxHighlighting.Core
module Helpers  = SyntaxHighlighting.Helpers

module Name = struct
  let macro                     = "macro"
  let let_binding               = "letbinding"
  let lambda                    = "lambda"
  let type_definition           = "typedefinition"
  let type_annotation           = "typeannotation"
  let control_keywords          = "controlkeywords"
  let numeric_literals          = "numericliterals"
  let operators                 = "operators"
  let identifier_constructor    = "identifierconstructor"
  let identifier_lower          = "identifierlower"
  let multiplication            = "multiplication"
  let module_                   = "module"
  let attribute                 = "attribute"
end

let syntax_highlighting =
  let open Core in
  {
    syntax_name          = "mligo";
    alt_name             = "caml";
    scope_name           = "source.mligo";
    file_types           = [];
    folding_start_marker = None;
    folding_stop_marker  = None;
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
          textmate = "";
          vim      = "\\\""
        }
      ];
      comments = {
        line_comment = {
          emacs    = "//";
          textmate = "";
          vim      = "\\/\\/.*$"
        };
        block_comment = (
          {
            emacs    = "(*";
            textmate = "";
            vim      = "(\\*"
          },
          {
            emacs    = "*)";
            textmate = "";
            vim      = "\\*)"
          }
        );
      };
      brackets = [
        ("{", "}");
        ("[", "]");
        ("(", ")")
      ];
      auto_closing_pairs = [
        ("{", "}");
        ("[", "]");
        ("(", ")");
        ("\"", "\"");
        ("'", "'");
      ];
      surrounding_pairs = [
        ("{", "}");
        ("[", "]");
        ("(", ")");
        ("\"", "\"");
        ("'", "'");
      ];
      syntax_table = [
        ("\n", "> b");
        ("/", ". 12b");
        ("*", ". 23");
        ("\\(", "()1n");
        ("\\)", ")(4n");
      ]
    };
    syntax_patterns = [
      Name.attribute;
      Name.macro;
      Name.let_binding;
      Name.lambda;
      Name.type_definition;
      Name.type_annotation; 
      Name.control_keywords;
      Name.numeric_literals;
      Name.operators;
      Name.identifier_constructor;
      Name.identifier_lower;
      Name.module_
    ];
    repository = [
      Helpers.attribute;
      Helpers.macro;
      {
        name = Name.control_keywords;
        kind = Match {
          match_name = Some Conditional;
          match_ = [(Regexp.control_keywords_match, None)]
        }
      };
      {
        name = Name.let_binding;
        kind = Match {
          match_ = [
            (Regexp.let_binding_match1, Some Keyword);
            (Regexp.let_binding_match2, Some StorageClass);
            (Regexp.let_binding_match3, Some FunctionName)
          ];
          match_name = None
        }
      };
      Helpers.numeric_literals;
      {
        name = Name.operators;
        kind = Match {
          match_name = Some Operator;
          match_ = [(Regexp.operators_match, None)]
        }
      };
      {
        name = Name.type_annotation;
        kind = Match { 
          match_ = [(Regexp.type_annotation_match, None)];
          match_name = Some Type
        }
      };
      {
        name = Name.multiplication;
        kind = Match { 
          match_ = [(Regexp.multiplication_match, None)];
          match_name = Some Operator
        }
      };
      {
        name = Name.lambda;
        kind = Begin_end {
          meta_name = None;
          begin_ = [(Regexp.lambda_begin, Some Statement)];
          end_ = [(Regexp.lambda_end, Some Operator)];
          patterns = []
        }
      };
      {
        name = Name.type_definition;
        kind = Match {
          match_ = [(Regexp.type_definition_match, None)];
          match_name = Some Type
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
        name = Name.identifier_constructor;
        kind = Match {
          match_name = None;
          match_     = [(Regexp.identifier_constructor_match, Some Label)]
        }
      };
    ]
  }