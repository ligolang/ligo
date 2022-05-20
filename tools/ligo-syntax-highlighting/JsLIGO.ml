module Core     = SyntaxHighlighting.Core
module Helpers  = SyntaxHighlighting.Helpers

module Name = struct
  let macro                     = "macro"
  let let_binding               = "letbinding"
  let type_definition           = "typedefinition"
  let control_keywords          = "controlkeywords"
  let numeric_literals          = "numericliterals"
  let operators                 = "operators"
  let identifier_constructor    = "identifierconstructor"
  let module_access             = "moduleaccess "
  let module_alias              = "modulealias"
  let module_declaration        = "moduledeclaration"
  let attribute                 = "attribute"
end

let syntax_highlighting =
  let open Core in
  {
    syntax_name = "JsLIGO";
    alt_name = "javascript";
    scope_name = "source.jsligo";
    file_types = [ "jsligo" ];
    folding_start_marker = Some "{";
    folding_stop_marker = Some "}";
    language_features = {
      operators = [
        "!";
        "%";
        "+";
        "-";
        "*";
        "/";
        "==";
        "!=";
        ">=";
        "<=";
        ">";
        "<";
        "#";
        "||";
        "&&";
        "=";
        "*=";
        "/=";
        "%=";
        "+=";
        "-=";
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
            emacs    = "\\/*";
            textmate = "/\\*";
            vim      = "/\\*"
          },
          {
            emacs    = "*\\/";
            textmate = "\\*\\/";
            vim      = "\\*/"
          }
        );
      };
      comments_insertion = {
        line_comment = "//";
        block_comment = ("/*", "*/");
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
        ("*", ". 23");  
        ("\n", "> b");
        ("/", ". 124b");
      ]
    };
    syntax_patterns = [
      Name.attribute;
      Name.macro;
      Name.let_binding;
      Name.type_definition;
      Name.control_keywords;
      Name.numeric_literals;
      Name.operators;
      Name.identifier_constructor;
      Name.module_access;
      Name.module_alias;
      Name.module_declaration;
    ];
    repository = [
      {
        name = Name.attribute;
        kind = Match {
          match_name = Some Attribute;
          match_ = [(Regexp.attributes_match_jsligo, None)]
        }
      };
      Helpers.macro;
      {
        name = Name.let_binding;
        kind = Match {
          match_ = [
            (Regexp.let_binding_match1_jsligo, Some Keyword);
            (Regexp.let_binding_match2_jsligo, Some FunctionName)
          ];
          match_name = None
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
        name = Name.control_keywords;
        kind = Match {
          match_name = Some Conditional;
          match_ = [(Regexp.control_keywords_match_jsligo, None)]
        }
      };
      Helpers.numeric_literals;
      {
        name = Name.operators;
        kind = Match {
          match_name = Some Operator;
          match_ = [(Regexp.operators_match_jsligo, None)]
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
        name = Name.module_access;
        kind = Match {
          match_     = [
            (Regexp.module_match1_jsligo, Some Structure);
            (Regexp.module_match2_jsligo, Some Identifier)
          ];
          match_name = None
        }
      };
      {
        name = Name.module_alias;
        kind = Match {
          match_     = [
            (Regexp.module_alias_match1_jsligo, Some Conditional);
            (Regexp.module_alias_match2_jsligo, Some Structure)
          ];
          match_name = None
        }
      };
      {
        name = Name.module_declaration;
        kind = Match {
          match_     = [
            (Regexp.module_declaration_match1_jsligo, Some Keyword);
            (Regexp.module_declaration_match2_jsligo, Some Structure)
          ];
          match_name = None
        }
      };
    ]
  }
