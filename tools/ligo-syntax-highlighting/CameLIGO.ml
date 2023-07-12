module Core     = SyntaxHighlighting.Core
module Helpers  = SyntaxHighlighting.Helpers

module Name = struct
  let macro                     = "macro"
  let let_binding               = "letbinding"
  let lambda                    = "lambda"
  let control_keywords          = "controlkeywords"
  let structure_keywords        = "structurekeywords"
  let numeric_literals          = "numericliterals"
  let operators                 = "operators"
  let semicolon                 = "semicolon"
  let of_keyword                = "ofkeyword"
  let lowercase_identifier      = "lowercaseidentifier"
  let uppercase_identifier      = "uppercaseidentifier"
  let module_declaration        = "moduledeclaration"
  let attribute                 = "attribute"
  (* Types *)
  let type_definition           = "typedefinition"
  let type_annotation           = "typeannotation"
  let type_annotation_lambda    = "typeannotationlambda"
  let type_name                 = "typename"
  let type_var                  = "typevar"
  let type_parentheses          = "typeparentheses"
  let type_operator             = "typeoperator"
  let type_int                  = "typeint"
  let type_product              = "typeproduct"
end

let syntax_highlighting =
  let open Core in
  let type_core_patterns = [
    Name_ref Name.uppercase_identifier;

    (* Sum type *)
    Name_ref Name.of_keyword;

    Name_ref Name.type_operator;
    Name_ref Name.type_name;
    Name_ref Name.type_var;
    Name_ref Name.type_parentheses;
    Name_ref Name.type_int;
    Name_ref Name.type_product;
    String_ref;
  ] in
  {
    syntax_name          = "mligo";
    alt_name             = "caml";
    scope_name           = "source.mligo";
    file_types           = ["mligo"];
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
        "<>";
        ">=";
        "<=";
        ">";
        "<";
        "#";
        "or";
        "and";
        "^";
        "|>";
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
      (* TODO: Name.lowercase_identifier; *)
      Name_ref Name.uppercase_identifier;
      Name_ref Name.attribute;
      Name_ref Name.macro;
      Name_ref Name.let_binding;
      Name_ref Name.lambda;
      Name_ref Name.type_definition;
      Name_ref Name.control_keywords;
      Name_ref Name.module_declaration;
      Name_ref Name.structure_keywords;
      Name_ref Name.numeric_literals;
      Name_ref Name.operators;
      Name_ref Name.type_annotation;
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
        name = Name.structure_keywords;
        kind = Match {
          match_name = Some Keyword;
          match_ = [(Regexp.structure_keywords_match, None)]
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
        name = Name.lambda;
        kind = Begin_end {
          meta_name = None;
          begin_ = [(Regexp.lambda_begin, Some Statement)];
          end_ = [(Regexp.lambda_end, Some Operator)];
          patterns = [Name_ref Name.type_annotation_lambda]
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
      (* Types *)
      {
        name = Name.type_definition;
        kind = Begin_end {
          meta_name = None;
          begin_ = [(Regexp.type_definition_begin, Some Keyword)];
          end_ = [(Regexp.type_definition_end, None)];
          patterns = type_core_patterns;
        }
      };
      {
        name = Name.type_annotation;
        kind = Begin_end {
          meta_name = None;
          begin_ = [(Regexp.type_annotation_begin, Some Operator)];
          end_ = [(Regexp.type_annotation_end, None)];
          patterns = type_core_patterns;
        }
      };
      {
        name = Name.type_annotation_lambda;
        kind = Begin_end {
          meta_name = None;
          begin_ = [(Regexp.type_annotation_begin_lambda, Some Operator)];
          end_ = [(Regexp.type_annotation_end_lambda, None)];
          patterns = type_core_patterns;
        }
      };
      {
        name = Name.type_operator;
        kind = Match {
          match_name = Some Operator;
          match_ = [(Regexp.type_operator_match, None)];
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
          patterns = [Name_ref Name.uppercase_identifier; Name_ref Name.type_annotation; Name_ref Name.semicolon];
        }
      };
    ]
  }
