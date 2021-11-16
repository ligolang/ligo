module Textmate = SyntaxHighlighting.Textmate
module Helpers = Textmate.Helpers

let re = Rex.Pcre.re

module Name = struct
  let macro               = "macro"
  let type_declaration    = "type_decl"
  let let_declaration     = "let_decl"
  let block_comment       = "block_comment"
  let line_comment        = "line_comment"
  let let_name            = "let_name"
  let expr                = "expr"
  let type_annotation     = "type_annotation"
  let operators           = "operators"
  let record_expression   = "record_expr"
  let tuple_record_name   = "tuple_record_name"
  let tuple_arg_annot_type = "tuple_arg_annot_type"
  let if_or_switch_block   = "if_or_switch_block"
  let constructor          = "constructor"
  let string               = "string"
  let builtin_modules      = "builtin_modules"
  let type_identifier      = "type_identifier"
  let type_decl_identifier = "type_decl_identifier"
  let builtin_types        = "builtin_types"
  let pattern_record_item  = "pattern_record_item"
  let pattern_record       = "pattern_record"
  let builtin_big_map      = "builtin_big_map"
  let builtin_bitwise      = "builtin_bitwise"
  let builtin_bytes        = "builtin_bytes"
  let builtin_crypto       = "builtin_crypto"
  let builtin_list         = "builtin_list"
  let builtin_map          = "builtin_map"
  let builtin_set          = "builtin_set"
  let builtin_string       = "builtin_string"
  let builtin_tezos        = "builtin_tezos"
  let builtin_test         = "builtin_test"
  let builtin_toplevel     = "builtin_toplevel"
  let pattern_par          = "pattern_par"
  let pattern_sum          = "pattern_sum"
  let else_block           = "else_block"
  let module_name          = "module_name"
  let variable_name        = "variable_name"
  let tez_numeral          = "tez_numeral"
  let byte_literal         = "byte_literal"
  let boolean              = "boolean"
  let pattern_type_name    = "pattern_type_name"
end

let syntax_highlighting = 
  let open Textmate in
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
        "||";
        "&&";
        "++"
      ];
      string_delimiters = [
        "\\\""
      ];
      comments = {
        line_comment = Some "//";
        block_comment = Some ("/*", "*/");
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
        ("'", "'")
      ];
      surrounding_pairs = [
        ("{", "}");
        ("[", "]");
        ("(", ")");
        ("\"", "\"");
        ("'", "'")
      ];  
      syntax_table = [
        ("*", ". 23");
        ("\\n", "> b");
        ("/", ". 124b")
      ];
    };
    syntax_patterns = [
      Name.macro;
      Name.type_declaration;
      Name.let_declaration;
      Name.line_comment;
      Name.block_comment;
    ];
    repository = [
      Helpers.macro;
      {
        name = Name.let_declaration;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("\\b(let)\\b", Some Function)];
          end_ = [("(?=let|type|\\[@|\\/\\*|\\/\\/)", None)];
          patterns = [
            Name.let_name;
            Name.string;
            Name.line_comment;
            Name.block_comment;
            Name.if_or_switch_block;
            Name.else_block;
            Name.record_expression;
            Name.tuple_record_name;
            Name.tuple_arg_annot_type;
            Name.builtin_big_map;
            Name.builtin_bitwise;
            Name.builtin_bytes;
            Name.builtin_crypto;
            Name.builtin_list;
            Name.builtin_map;
            Name.builtin_set;
            Name.builtin_string;
            Name.builtin_tezos;
            Name.builtin_test;
            Name.builtin_toplevel;
            Name.operators;
            Name.module_name;
            Name.variable_name;
            Name.constructor;
            Name.tez_numeral;
            Name.byte_literal;
            Name.boolean;
          ]
        }
      };
      {
        name = Name.let_name;
        kind = Begin_end {
          meta_name = None;
          begin_ = [
            ("\\G[ ]*(\\b(rec)\\b\\s\\b)?", Some StorageClass);
            ("([a-z_][A-Za-z0-9_]*)\\b", Some Identifier)
          ];          
          end_ = [("(\\=)", Some Operator)];
          patterns = [
            Name.type_annotation;
            Name.line_comment;
            Name.block_comment;
          ]
        };
      };
      {
        name = Name.type_annotation;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("\\G[ ]*(\\:)", None)];
          end_ = [("(?=\\=)", None)];
          patterns = [
            Name.pattern_par;
            Name.pattern_record;
            Name.pattern_sum;
            Name.builtin_types;
            Name.pattern_type_name;
            Name.line_comment;
            Name.block_comment;
          ]
        }
      };
      {
        name = Name.operators;
        kind = Match {
          match_name = Some Operator;
          match_ = re "\\b(mod|ediv)\\b|(\\+|\\-|\\*|\\/|==|\\|\\||\\&\\&)";
          captures = [];
        }
      }] 
      @
      Helpers.string
      @ 
      [
      {
        name = Name.record_expression;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("(?<=\\=)\\s*\\{", None)];
          end_ = [("\\}", None)];
          patterns = [
            Name.tuple_record_name;
            Name.string;
            Name.line_comment;
            Name.block_comment;
            Name.if_or_switch_block;
            Name.else_block;
            Name.record_expression;
            Name.tuple_record_name;
            Name.tuple_arg_annot_type;
            Name.builtin_big_map;
            Name.builtin_bitwise;
            Name.builtin_bytes;
            Name.builtin_crypto;
            Name.builtin_list;
            Name.builtin_map;
            Name.builtin_set;
            Name.builtin_string;
            Name.builtin_tezos;
            Name.builtin_test;
            Name.builtin_toplevel;
            Name.operators;
            Name.module_name;
            Name.variable_name;
            Name.constructor;
            Name.tez_numeral;
            Name.byte_literal;
            Name.boolean
          ]
        }
      };
      {
        name = Name.tuple_record_name;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("(?<=\\(|,|\\{)\\s*([a-z][A-Za-z0-9_]*)\\s*(?=\\,|:|\\)|\\})", Some Identifier)];
          end_ = [("(?!\\,|\\)|\\})", None)];
          patterns = [
            Name.line_comment;
            Name.block_comment;
          ]
        }
      };
      {
        name = Name.tuple_arg_annot_type;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("\\:[ ]*", None)];
          end_ = [("(?=,|\\)|\\=\\>|\\})", None)];
          patterns = [
            Name.pattern_par;
            Name.pattern_record;
            Name.pattern_sum;
            Name.builtin_types;
            Name.pattern_type_name
          ]
        }
      };
      {
        name = Name.if_or_switch_block;
        kind = Begin_end {
          meta_name = None;
          begin_ = [
            ("\\b(if|switch)\\b", Some Conditional);
            ("[ ]*(\\(|[a-z_])", None)
          ];
          end_ = [("\\)", None)];
          patterns = [
            Name.string;
            Name.line_comment;
            Name.block_comment;
            Name.if_or_switch_block;
            Name.else_block;
            Name.record_expression;
            Name.tuple_record_name;
            Name.tuple_arg_annot_type;
            Name.builtin_big_map;
            Name.builtin_bitwise;
            Name.builtin_bytes;
            Name.builtin_crypto;
            Name.builtin_list;
            Name.builtin_map;
            Name.builtin_set;
            Name.builtin_string;
            Name.builtin_tezos;
            Name.builtin_test;
            Name.builtin_toplevel;
            Name.operators;
            Name.module_name;
            Name.variable_name;
            Name.constructor;
            Name.tez_numeral;
            Name.byte_literal;
            Name.boolean
          ]
        }
      };
      {
        name = Name.constructor;
        kind = Match {
          match_ = re "(\\b[A-Z][a-zA-Z0-9_]*(\\b|\\())";
          match_name = Some Label;
          captures = []
        }
      };
      {
        name = Name.else_block;
        kind = Match {
          match_name = Some Conditional;
          match_ = re "\\b(else)\\b";
          captures = []
        };
      };
      {
        name = Name.module_name;
        kind = Match {
          match_name = None;
          match_ = re "\\b([A-Z][a-zA-Z0-9_]+)\\.\\b";
          captures = [
            (1, Structure)
          ]
        };
      };
      { name = Name.variable_name;
        kind = Match {
          match_name = None;
          match_ = re "\\b([a-z_][a-zA-Z0-9_]*)\\b";
          captures = [
            (1, Identifier)
          ]
        }
      };
      { name = Name.tez_numeral;
        kind = Match {
          match_name = Some Number;
          match_ = re "\\b([0-9_]+)(tez|mutez|n)?\\b";
          captures = []
        }
      };
      { name = Name.byte_literal;
        kind = Match {
          match_name = Some Number;
          match_ = re "\\b0x([0-9_]+)?\\b";
          captures = []
        }
      };
      { name = Name.boolean;
        kind = Match {
          match_name = Some Boolean;
          match_ = re "\\b(true|false)\\b";
          captures = []
        }
      };
      {
        name = Name.type_declaration;
        kind = Begin_end {
          meta_name = None; 
          begin_ = [("\\b(type)\\b", Some Type)];
          end_ = [("(?=let|type|\\[@|\\/\\*|\\/\\/)", None)];
          patterns = [
            Name.line_comment;
            Name.block_comment;
            Name.type_identifier;
            Name.type_decl_identifier
          ]
        }
      };
      {
        name = Name.type_decl_identifier;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("(=)", Some Operator)];
          end_ = [("(?=let|type|\\[@|\\/\\*|\\/\\/)", None)];
          patterns = [
            Name.line_comment;
            Name.block_comment;
            Name.pattern_par;
            Name.pattern_record;
            Name.pattern_sum;
            Name.builtin_types;
            Name.pattern_type_name
          ]
        }
      };
      {
        name = Name.builtin_types;
        kind = Match {
          match_name = Some Builtin_type; 
          match_ = re "\\b(int|nat|address|tez|contract|list|option|unit|bool|signature|bytes|big_map|chain_id|key|key_hash|map|operation|set|string|timestamp)\\b";
          captures = [];
        }
      };
      {
        name = Name.builtin_big_map;
        kind = Match {
          match_name = None;
          match_ = re "\\b(Big_map)\\.(empty|literal|find_opt|mem|update|add|remove|get_and_update|identifier)\\b";
          captures = [
            (1, Builtin_module);
            (2, Builtin_function)
          ]
        }
      };
      {
        name = Name.builtin_bitwise;
        kind = Match {
          match_name = None;
          match_ = re "\\b(Bitwise)\\.(and|or|xor|shift_left|shift_right)\\b";
          captures = [
            (1, Builtin_module);
            (2, Builtin_function)
          ]
        }
      };
      {
        name = Name.builtin_bytes;
        kind = Match {
          match_name = None;
          match_ = re "\\b(Bytes)\\.(concat|sub|pack|unpack|length)\\b";
          captures = [
            (1, Builtin_module);
            (2, Builtin_function)
          ]
        }
      };
      {
        name = Name.builtin_crypto;
        kind = Match {
          match_name = None;
          match_ = re "\\b(Crypto)\\.(blake2b|sha256|sha512|hash_key|check)\\b";
          captures = [
            (1, Builtin_module);
            (2, Builtin_function)
          ]
        }
      };
      {
        name = Name.builtin_list;
        kind = Match {
          match_name = None;
          match_ = re "\\b(List)\\.(length|size|head_opt|tail_opt|iter|map|fold|fold_left|fold_right)\\b";
          captures = [
            (1, Builtin_module);
            (2, Builtin_function)
          ]
        }
      };
      {
        name = Name.builtin_map;
        kind = Match {
          match_name = None;
          match_ = re "\\b(Map)\\.(empty|literal|find_opt|update|add|remove|iter|map|fold|size|mem|get_and_update)\\b";
          captures = [
            (1, Builtin_module);
            (2, Builtin_function)
          ]
        }
      };
      {
        name = Name.builtin_set;
        kind = Match {
          match_name = None;
          match_ = re "\\b(Set)\\.(empty|literal|mem|cardinal|add|remove|iter|fold|fold_desc)\\b";
          captures = [
            (1, Builtin_module);
            (2, Builtin_function)
          ]
        }
      };
      {
        name = Name.builtin_string;
        kind = Match {
          match_name = None;
          match_ = re "\\b(String)\\.(length|sub|concat)\\b";
          captures = [
            (1, Builtin_module);
            (2, Builtin_function)
          ]
        }
      };
      {
        name = Name.builtin_tezos;
        kind = Match {
          match_name = None;
          match_ = re "\\b(Tezos)\\.(now|balance|amount|sender|address|self_address|self|source|implicit_account|create_contract|failwith|chain_id|transaction|set_delegate|get_contract_opt|get_entrypoint_opt|level|pairing_check|sapling_empty_state|sapling_verify_update|create_ticket|read_ticket|split_ticket|join_tickets|level|pairing_check|never)\\b";
          captures = [
            (1, Builtin_module);
            (2, Builtin_function)
          ]
        }
      };
      {
        name = Name.builtin_test;
        kind = Match {
          match_name = None;
          match_ = re "\\b(Test)\\.(originate|set_now|set_source|set_baker|transfer|transfer_exn|get_storage|get_balance|michelson_equal|log|reset_state|nth_bootstrap_account|last_originations|compile_expression|compile_expression_subst|compile_value)\\b";
          captures = [
            (1, Builtin_module);
            (2, Builtin_function)
          ]
        }
      };
      {
        name = Name.builtin_toplevel;
        kind = Match {
          match_name = None;
          match_ = re "\\b(is_nat|abs|int|failwith|assert|ediv)\\b";
          captures = [
            (1, Builtin_module);
            (2, Builtin_function)
          ]
        }
      };
      { name = Name.pattern_type_name;
        kind = Match {
          match_name = Some Builtin_type;
          match_ = re "\\b([_a-z][a-zA-Z0-9$_]*)\\b";
          captures = [];
        }
      };
      {
        name = Name.pattern_par;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("\\(", None)];
          end_ = [("\\)", None)];
          patterns = [
            Name.pattern_par;
            Name.pattern_record;
            Name.pattern_sum;
            Name.builtin_types;
            Name.pattern_type_name
          ]
        }
      };
      {
        name = Name.pattern_sum;
        kind = Match {
          match_name = None;
          match_ = re "\\b(\\|?[A-Z][a-zA-Z0-9_]*)+\\b";
          captures = [
            (1, Label)
          ]
        }
      };
      {
        name = Name.pattern_record;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("{", None)];
          end_ = [("}", None)];
          patterns = [
            Name.line_comment;
            Name.block_comment;
            Name.pattern_record_item
          ]
        }
      };
      {
        name = Name.pattern_record_item;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("([a-z_][A-Za-z0-9_]*)", Some Type)];
          end_ = [("(?=\\,|\\})", None)];
          patterns = [
            Name.line_comment;
            Name.block_comment;
            Name.pattern_par;
            Name.pattern_record;
            Name.pattern_sum;
            Name.builtin_types;
            Name.pattern_type_name
          ]
        }
      };
      {
        name = Name.type_identifier;
        kind = Match {
          match_name = Some Type;
          match_ = re "\\b([_a-z][a-zA-Z0-9$_]*)\\b";
          captures = [1, Type]
        }
      }] 
      @
      Helpers.c_comment
  }