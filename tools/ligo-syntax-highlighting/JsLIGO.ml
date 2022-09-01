module Core     = SyntaxHighlighting.Core
module Helpers  = SyntaxHighlighting.Helpers

module Name = struct
  let macro                     = "macro"
  let let_binding               = "letbinding"
  let control_keywords          = "controlkeywords"
  let numeric_literals          = "numericliterals"
  let operators                 = "operators"
  let semicolon                 = "semicolon"
  let comma                     = "comma"
  let lowercase_identifier      = "lowercaseidentifier"
  let uppercase_identifier      = "uppercaseidentifier"
  let module_access             = "moduleaccess "
  let module_alias              = "modulealias"
  let module_declaration        = "moduledeclaration"
  let object_or_block           = "objectorblock"
  let object_property           = "objectproperty"
  let object_property_ctor      = "objectpropertyctor"
  let object_property_int       = "objectpropertyint"
  let object_property_string    = "objectpropertystring"
  let attribute                 = "attribute"
  (* Types *)
  let type_definition           = "typedefinition"
  let type_annotation           = "typeannotation"
  let type_annotation_field     = "typeannotationfield"
  let type_as                   = "typeas"
  let type_name                 = "typename"
  let type_generic_binder       = "typegenericbinder"
  let type_parentheses          = "typeparentheses"
  let type_operator             = "typeoperator"
  let type_int                  = "typeint"
  let type_variant              = "typevariant"
  let type_product              = "typeproduct"
  let type_fun_param            = "typefunparam"
end

let syntax_highlighting =
  let open Core in
  let type_core_patterns = [
    Name.uppercase_identifier;
    Name.type_operator;
    Name.type_name;
    Name.type_parentheses;
    Name.type_int;
    Name.type_variant;
    Name.type_product;
    "string";
  ] in
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
          textmate = "\\/\\/";
          vim      = "\\/\\/"
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
      extra_patterns = {
        (* FIXME: The highlighting for attributes is a bit more liberal, and may
           catch false negatives. We preferred this for the simplicity of the
           solution for now.
         *)
        in_line_comments = [Name.attribute];
        in_block_comments = [Name.attribute];
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
        ("*", ". 23");  
        ("\n", "> b");
        ("/", ". 124b");
      ]
    };
    syntax_patterns = [
      (* TODO: Name.lowercase_identifier; *)
      Name.uppercase_identifier;
      Name.macro;
      Name.let_binding;
      Name.type_definition;
      Name.control_keywords;
      Name.numeric_literals;
      Name.operators;
      Name.module_alias;
      Name.module_declaration;
      Name.type_annotation;
      Name.type_as;
      Name.object_or_block;
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
      (* FIXME: breaks on patterns *)
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
        name = Name.semicolon;
        kind = Match {
          match_name = None;
          match_ = [(Regexp.semicolon_match, None)];
        }
      };
      {
        name = Name.comma;
        kind = Match {
          match_name = None;
          match_ = [(Regexp.comma_match, None)];
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
          match_     = [(Regexp.let_binding_match2_jsligo, Some Identifier)];
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
          match_name = None;
          match_     = [(Regexp.module_declaration_match1_jsligo, Some Keyword)];
        }
      };
      {
        name = Name.object_or_block;
        kind = Begin_end {
          meta_name = None;
          begin_ = [(Regexp.braces_begin, None)];
          end_ = [(Regexp.braces_end, None)];
          patterns = [
            Name.object_property_ctor;
            Name.object_property_int;
            Name.object_property_string;
            Name.object_property;
            Name.comma;
            "$self";
          ];
        }
      };
      {
        name = Name.object_property_ctor;
        kind = Begin_end {
          meta_name = None;
          begin_ = [
            (Regexp.identifier_constructor_match, Some Label);
            (Regexp.property_expr_begin_jsligo, Some Operator);
          ];
          end_ = [(Regexp.property_expr_end_jsligo, None)];
          patterns = ["$self"];
        }
      };
      {
        name = Name.object_property_int;
        kind = Begin_end {
          meta_name = None;
          begin_ = [
            (Regexp.int_literal_match, Some Number);
            (Regexp.property_expr_begin_jsligo, Some Operator);
          ];
          end_ = [(Regexp.property_expr_end_jsligo, None)];
          patterns = ["$self"];
        }
      };
      {
        name = Name.object_property_string;
        kind = Begin_end {
          meta_name = None;
          begin_ = [
            (Regexp.string_literal_match, Some String);
            (Regexp.property_expr_begin_jsligo, Some Operator);
          ];
          end_ = [(Regexp.property_expr_end_jsligo, None)];
          patterns = ["$self"];
        }
      };
      {
        name = Name.object_property;
        kind = Begin_end {
          meta_name = None;
          begin_ = [
            (Regexp.let_binding_match2_jsligo, None);
            (Regexp.property_expr_begin_jsligo, Some Operator);
          ];
          end_ = [(Regexp.property_expr_end_jsligo, None)];
          patterns = ["$self"];
        }
      };
      (* Types *)
      {
        name = Name.type_definition;
        kind = Begin_end {
          meta_name = None;
          begin_ = [(Regexp.type_definition_begin_jsligo, Some Keyword)];
          end_ = [(Regexp.type_definition_end_jsligo, None)];
          patterns = type_core_patterns;
        }
      };
      {
        name = Name.type_annotation;
        kind = Begin_end {
          meta_name = None;
          begin_ = [(Regexp.type_annotation_begin_jsligo, Some Operator)];
          end_ = [(Regexp.type_annotation_end_jsligo, None)];
          patterns = type_core_patterns;
        }
      };
      {
        name = Name.type_annotation_field;
        kind = Begin_end {
          meta_name = None;
          begin_ = [(Regexp.type_annotation_field_begin_jsligo, Some Operator)];
          end_ = [(Regexp.type_annotation_field_end_jsligo, None)];
          patterns = type_core_patterns;
        }
      };
      {
        name = Name.type_as;
        kind = Begin_end {
          meta_name = None;
          begin_ = [(Regexp.type_as_begin_jsligo, Some Keyword)];
          end_ = [(Regexp.type_as_end_jsligo, None)];
          patterns = type_core_patterns;
        }
      };
      {
        name = Name.type_operator;
        kind = Match {
          match_name = Some Operator;
          match_ = [(Regexp.type_operator_match_jsligo, None)];
        }
      };
      {
        name = Name.type_name;
        kind = Match {
          match_name = Some Type;
          match_ = [(Regexp.type_name_match_jsligo, None)];
        }
      };
      {
        name = Name.type_generic_binder;
        kind = Begin_end {
          (*
            n.b.: We use Type instead of Type_var since we can't easily
            disambiguate between the two in JsLIGO.
          *)
          meta_name = None;
          begin_ = [(Regexp.chevron_begin, None)];
          end_ = [(Regexp.chevron_end, None)];
          patterns = [Name.comma; Name.type_name];
        }
      };
      (*
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
      {
        name = Name.type_fun_param;
        kind = Match {
          match_name = None;
          match_ = [(Regexp.identifier_annotation_positive_lookahead, None)];
        }
      };
      {
        name = Name.type_parentheses;
        kind = Begin_end {
          meta_name = None;
          begin_ = [(Regexp.parentheses_begin, None)];
          end_ = [(Regexp.parentheses_end, None)];
          patterns = Name.type_fun_param :: Name.type_annotation :: Name.comma :: type_core_patterns;
        }
      };
      {
        name = Name.type_int;
        kind = Match {
          match_name = Some Number;
          match_ = [(Regexp.int_literal_match, None)];
        }
      };
      (*
        n.b.: Like `type_parentheses`, we join JsLIGO's `variant` and
        `type_tuple` in one thing, otherwise it would unduly complicate the
        logic to disambiguate the two. The pipe (`|`) for the `sum_type` is
        handled by `type_operator`.
      *)
      {
        name = Name.type_variant;
        kind = Begin_end {
          meta_name = None;
          begin_ = [(Regexp.brackets_begin, None)];
          end_ = [(Regexp.brackets_end, None)];
          patterns = Name.comma :: type_core_patterns;
        }
      };
      {
        name = Name.type_product;
        kind = Begin_end {
          meta_name = None;
          begin_ = [(Regexp.braces_begin, None)];
          end_ = [(Regexp.braces_end, None)];
          patterns = [Name.lowercase_identifier; Name.type_annotation_field; Name.comma];
        }
      };
    ]
  }
