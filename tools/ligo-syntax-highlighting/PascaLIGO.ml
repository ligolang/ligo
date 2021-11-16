module Textmate = SyntaxHighlighting.Textmate
module Helpers = Textmate.Helpers

let re = Rex.Pcre.re

module Name = struct
  let string                = "string"
  let single_quotes         = "single-quotes"
  let line_comment          = "line_comment"
  let block_comment         = "block_comment"
  let macro                 = "macro"
  let function_             = "function"
  let binding               = "binding"
  let type_annotation       = "type-annotation"
  let type_definition       = "type-definition"
  let control_keywords      = "control-keywords"
  let other_keywords        = "other-keywords"
  let operators             = "operators"
  let function_application  = "function-application"
  let identifiers           = "identifiers"
  let struct_type           = "struct-type"
  let sum_type              = "sum-type"
  let type_alias            = "type-alias"
  let type_par              = "type-par"
  (* let type_equals           = "type-equals" *)
end

let syntax_highlighting = 
  let open Textmate in
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
        "\""
      ];
      comments = {
        line_comment = Some "//";
        block_comment = Some ("(*", "*)");
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
      Name.string;
      Name.single_quotes;
      Name.line_comment;
      Name.block_comment;
      Name.macro;
      Name.function_;
      Name.binding;
      Name.type_annotation;
      Name.type_definition;
      Name.control_keywords;
      Name.other_keywords;
      Name.operators;
      Name.function_application;
      Name.identifiers;
    ];
    repository = [
      Helpers.macro;
      {
        name = Name.function_;
        kind = Begin_end {
          meta_name = None;
          begin_ = [
            (* \\b(recursive)?\\s+(function)\\s+[a-zA-Z]+\\b *)
            ("\\b(recursive)?\\s+", Some StorageClass);
            ("(function)\\s+", Some Function); 
            ("([a-zA-Z_])\\b", Some FunctionName)];
          end_ = [("\\b(is)\\b", Some Operator)];
          patterns = [
            Name.line_comment;
            Name.block_comment;
            Name.type_annotation;
            Name.binding;
          ]
        }
      };
      { 
        name = Name.binding;
        kind = Begin_end {
          meta_name = None;
          begin_ = [
            ("\\b(var|const)\\s+", Some Statement);
            ("([a-zA-Z_]\\w*)\\b", Some Identifier)];
          end_ = [
            ("(?=[=),;]|:=)", None)
          ];
          patterns = [
            Name.line_comment;
            Name.block_comment;
            Name.type_annotation
        ]}
      };
      {
        name = Name.type_annotation;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("(:(?!=))\\s*", Some Operator)];
          end_ = [("(?:\\||(?=[;)=}\\]]|\\bis\\b|:=)|$)", None)];
          patterns = [
              Name.line_comment;
              Name.block_comment;
              Name.type_par;
              (* Name.type_equals *)
          ]
        }
      };
      { name = Name.type_par;
        kind = Begin_end {
            meta_name = None;
            begin_ = [("\\(", None)];
            end_ = [("\\)", None)];
            patterns = [
                Name.line_comment;
                Name.block_comment;
                Name.type_par;
                (* Name.type_equals *)
            ]
          }
      };
      (* { name = Name.type_equals;
        kind = Match {
            match_name = None;
            match_ = re "((?:(?!\\bis\\b|:=)[^=()|;}\\/\\]])*)";
            captures = [
                (1, Type)
            ]
          }
      }; *)
      
      {
        name = Name.type_definition;
        kind = Begin_end {
          meta_name = None;
          begin_ = 
            [("\\b(type)", Some Keyword);
            ("\\s+([a-zA-Z_]\\w*)", Some Type);
            ("\\s+(is)\\b", Some Statement);
            ];
          end_ = [("(?=\\b(?:function|type|const|var|recursive)\\b|^\\s*#\\w+)", None)];
          patterns = [
              Name.line_comment;
              Name.block_comment;
              Name.struct_type;
              Name.sum_type;
              Name.type_alias
          ]
        }
      };
      {
        name = Name.struct_type;
        kind = Begin_end {
          meta_name = None;
          begin_ = [
            ("\\b(record)\\s*", Some Statement);
            ("(\\[?)", Some Statement)
          ];
          end_ = [("(\\]|\\bend\\b)", Some Statement)];
          patterns = [
              Name.line_comment;
              Name.block_comment;
              Name.identifiers;
              Name.type_annotation
          ]
        }
      };
      {
        name = Name.sum_type;
        kind = Begin_end {
          meta_name = None;
          begin_ = [
            ("\\b([A-Z]\\w*)\\s+", Some Label); 
            ("(of)?", Some Statement)
          ];
          end_ = [
            ("(\\||(?=\\b(?:function|type|const|var)\\b|^\\s*#\\w+))", None)
          ];
          patterns = [
              Name.line_comment;
              Name.block_comment;
              Name.type_par;
              (* Name.type_equals *)
          ]
        }
      };
      {
        name = Name.type_alias;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("\\G\\s*(?!record\\b)(?=[(a-z])", None)];
          end_ = [("(?=\\b(?:function|type|const|var)\\b|^\\s*#\\w+)", None)];
          patterns = [
              Name.line_comment;
              Name.block_comment;
              Name.type_par;
              (* Name.type_equals *)
          ]
        }
      };
      ]
      @
      Helpers.string
      @
      [
      {
        name = Name.single_quotes;
        kind = Begin_end {
          meta_name = Some String;
          begin_ = [("\\'", None)];
          end_ = [("\\'", None)];
          patterns = []
        }
      }] @
      Helpers.ocaml_comment
      @
      [{
        name = Name.control_keywords;
        kind = Match {
          match_name = Some Conditional;
          match_ = re "\\b(case|of|if|then|else|for|in|step|to|skip|assert|failwith|begin|end|contains)\\b";
          captures = []
        }
      };
      {
        name = Name.other_keywords;
        kind = Match {
          match_name = Some Statement;
          match_ = re "\\b(block|with|record|set|map|list)\\b";
          captures = []
        }
      };
      {
        name = Name.operators;
        kind = Match {
          match_name = Some Operator;
          match_ = re "([-+*/=]|->|:=)";
          captures = []
        }
      };
      {
        name = Name.function_application;
        kind = Match {
          match_name = None;
          match_ = re "\\b([a-zA-Z_]\\w*)\\s+\\(";
          captures = [
              (1, Identifier)
          ]
        }
      };
      {
        name = Name.identifiers;
        kind = Match {
          match_name = None;
          match_ = re "\\b([a-zA-Z_]\\w*)\\b";
          captures = [
              (1, Identifier)
          ]
        }
      }
    ]
  } 
