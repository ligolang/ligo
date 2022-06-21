module Core     = SyntaxHighlighting.Core
module Helpers  = SyntaxHighlighting.Helpers

module Name = struct
  let macro                     = "macro"
  let let_binding               = "letbinding"
  (* let lambda                    = "lambda" *)
  let control_keywords          = "controlkeywords"
  let numeric_literals          = "numericliterals"
  let operators                 = "operators"
  let comma                     = "comma"
  let identifier                = "identifier"
  let identifier_constructor    = "identifierconstructor"
  let module_                   = "module"
  let record_or_block           = "recordorblock"
  let record_field              = "recordfield"
  let attribute                 = "attribute"
  (* Types *)
  let type_definition           = "typedefinition"
  let type_annotation           = "typeannotation"
  let type_annotation_lambda    = "typeannotationlambda"
  let type_name                 = "typename"
  let type_var                  = "typevar"
  let type_parentheses          = "typeparentheses"
  let type_operator             = "typeoperator"
  let type_module               = "typemodule"
  let type_int                  = "typeint"
  let type_product              = "typeproduct"
end

let syntax_highlighting =
  let open Core in
  let type_core_patterns = [
    Name.type_module;
    (* Sum type *)
    Name.identifier_constructor;

    Name.type_operator;
    Name.type_name;
    Name.type_var;
    Name.type_parentheses;
    Name.type_int;
    Name.type_product;
    "string";
  ] in
  {
    syntax_name = "ReasonLIGO";
    alt_name = "reason";
    scope_name = "source.religo";
    file_types = [
      "religo";
      "rligo"
    ];
    folding_start_marker = Some "{";
    folding_stop_marker = Some "}";
    language_features = {
      operators = [
        "!";
        "mod";
        "ediv";
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
      (* Name.lambda; *)
      Name.type_definition;
      Name.type_annotation;
      Name.control_keywords;
      Name.numeric_literals;
      Name.operators;
      Name.identifier_constructor;
      Name.module_;
      Name.record_or_block;
    ];
    repository = [
      Helpers.attribute;
      Helpers.macro;
      {
        name = Name.control_keywords;
        kind = Match {
          match_name = Some Conditional;
          match_ = [(Regexp.control_keywords_match_reasonligo, None)]
        }
      };
      (* FIXME: breaks on patterns *)
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
          match_ = [(Regexp.operators_match_reasonligo, None)]
        }
      };
      {
        name = Name.comma;
        kind = Match {
          match_name = None;
          match_ = [(Regexp.comma_match, None)];
        }
      };
      (* {
        name = Name.lambda;
        kind = Begin_end {
          meta_name = None;
          begin_ = [(Regexp.lambda_begin, Some Statement)];
          end_ = [(Regexp.lambda_end, Some Operator)];
          patterns = []
        }
      }; *)
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
          match_ = [(Regexp.let_binding_match3, None)];
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
        name = Name.record_or_block;
        kind = Begin_end {
          meta_name = None;
          begin_ = [(Regexp.braces_begin, None)];
          end_ = [(Regexp.braces_end, None)];
          patterns = [Name.record_field; Name.comma; "$self"];
        }
      };
      {
        name = Name.record_field;
        kind = Begin_end {
          meta_name = None;
          begin_ = [
            (Regexp.let_binding_match3, None);
            (Regexp.whitespace_match, None);
            (Regexp.field_expr_begin_reasonligo, Some Operator);
          ];
          end_ = [(Regexp.field_expr_end_reasonligo, None)];
          patterns = ["$self"];
        }
      };
      (* Types *)
      {
        name = Name.type_definition;
        kind = Begin_end {
          meta_name = None;
          begin_ = [(Regexp.type_definition_begin_reasonligo, Some Keyword)];
          end_ = [(Regexp.type_definition_end_reasonligo, None)];
          patterns = type_core_patterns;
        }
      };
      {
        name = Name.type_annotation;
        kind = Begin_end {
          meta_name = None;
          begin_ = [(Regexp.type_annotation_begin_reasonligo, Some Operator)];
          end_ = [(Regexp.type_annotation_end_reasonligo, None)];
          patterns = type_core_patterns;
        }
      };
      {
        name = Name.type_operator;
        kind = Match {
          match_name = Some Operator;
          match_ = [(Regexp.type_operator_match_reasonligo, None)];
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
        name = Name.type_var;
        kind = Match {
          match_name = Some Type_var;
          match_ = [(Regexp.type_var_match, None)];
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
      {
        name = Name.type_product;
        kind = Begin_end {
          meta_name = None;
          begin_ = [(Regexp.braces_begin, None)];
          end_ = [(Regexp.braces_end, None)];
          patterns = [Name.identifier; Name.type_annotation; Name.comma];
        }
      };
    ]
  }
