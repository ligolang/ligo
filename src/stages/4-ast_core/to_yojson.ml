open Types
open Stage_common.To_yojson

let rec type_expression {type_content=tc;sugar=_;location} =
  `Assoc [
    ("type_content", type_content tc);
    ("location", Location.to_yojson location);
  ]

and type_content = function
  | T_variable t -> `List [ `String "t_variable"; type_variable_to_yojson t]
  | T_sum      t -> `List [ `String "t_sum";      label_map (row_element type_expression) t.fields]
  | T_record   t -> `List [ `String "t_record";   label_map (row_element type_expression) t.fields]
  | T_arrow    t -> `List [ `String "t_arrow";    arrow type_expression t]
  | T_app      t -> `List [ `String "t_app";      t_app type_expression t]

let rec expression {content=ec;sugar;location} =
  `Assoc [
    ("expression_content", expression_content ec);
    ("sugar", option Ast_sugar.Yojson.expression sugar);
    ("location", Location.to_yojson location);
  ]

and expression_content = function
  (* Base *)
  | E_literal     e -> `List [ `String "E_literal"; literal e ]
  | E_constant    e -> `List [ `String "E_constant"; constant expression e ]
  | E_variable    e -> `List [ `String "E_variable"; expression_variable_to_yojson e ]
  | E_application e -> `List [ `String "E_application"; application expression e ]
  | E_lambda      e -> `List [ `String "E_lambda";      lambda      expression type_expression e ]
  | E_recursive   e -> `List [ `String "E_recursive";   recursive   expression type_expression e ]
  | E_let_in      e -> `List [ `String "E_let_in";      let_in e ]
  | E_raw_code    e -> `List [ `String "E_raw_code";    raw_code    expression e ]
  (* Variant *)
  | E_constructor     e -> `List [ `String "E_constructor"; constructor expression e ]
  | E_matching        e -> `List [ `String "E_matching"; matching e ]
  (* Record *)
  | E_record          e -> `List [ `String "E_record"; record expression e ]
  | E_record_accessor e -> `List [ `String "E_record_accessor"; record_accessor expression e ]
  | E_record_update   e -> `List [ `String "E_record_update"; record_update expression e ]
  | E_ascription      e -> `List [ `String "E_ascription"; ascription expression type_expression e ]

and let_in {let_binder;rhs;let_result;inline} =
  `Assoc [
    ("let_binder", binder type_expression let_binder);
    ("rhs", expression rhs);
    ("let_result", expression let_result);
    ("inline", `Bool inline);
  ]


and matching {matchee; cases} =
  `Assoc [
    ("matchee", expression matchee);
    ("cases", matching_expr cases);
  ]

and matching_expr = function
  | Match_list    {match_nil;match_cons} -> `List [ `String "Match_list";
    `Assoc [
      ("match_nil", expression match_nil);
      ("match_cons", matching_cons match_cons);
    ]]
  | Match_option  {match_none;match_some} -> `List [ `String "Match_option";
    `Assoc [
      ("match_none", expression match_none);
      ("match_some", matching_some match_some);
    ]]
  | Match_variant m -> `List [ `String "Match_variant"; list matching_content_case m ]

and matching_cons {hd; tl; body} =
  `Assoc [
    ("hd", expression_variable_to_yojson hd);
    ("tl", expression_variable_to_yojson tl);
    ("body", expression body);
  ]

and matching_some {opt; body} =
  `Assoc [
    ("opt", expression_variable_to_yojson opt);
    ("body", expression body);
  ]

and matching_content_case {constructor; proj; body} =
  `Assoc [
    ("constructor", label_to_yojson constructor);
    ("pattern", expression_variable_to_yojson proj);
    ("body", expression body);
  ]

let declaration_constant {binder=b;expr;attr} =
  `Assoc [
    ("binder",binder type_expression b);
    ("expr", expression expr);
    ("attr", `Bool attr.inline);
  ]
let declaration = function
  | Declaration_type     dt -> `List [ `String "Declaration_type";     declaration_type type_expression dt]
  | Declaration_constant dc -> `List [ `String "Declaration_constant"; declaration_constant dc]

let program =  program declaration
