open Types
open Stage_common.To_yojson
type json = Yojson.Safe.t
type 'a json_printer = 'a -> json

let constant' = Stage_common.To_yojson.constant'

let label = label_to_yojson
let option f o =
  match o with
  | None   -> `List [ `String "None" ; `Null ]
  | Some v -> `List [ `String "Some" ; f v ]

let pair f g (x, y) = `Tuple [ f x ; g y ]
let list f lst = `List (List.map ~f:f lst)
let label_map f lmap =
  let lst = List.sort ~compare:(fun (Label a, _) (Label b, _) -> String.compare a b) (LMap.bindings lmap) in
  let lst' = List.fold_left
      ~f:(fun acc (Label k, v) -> (k , f v)::acc)
      ~init:[] lst
  in
  `Assoc lst'

let layout = function
  | L_comb -> `List [ `String "L_comb"; `Null ]
  | L_tree -> `List [ `String "L_tree"; `Null ]


let rec type_expression {type_content=tc; location; orig_var; source_type = _} =
  `Assoc [
    ("type_content", type_content tc);
    ("location", Location.to_yojson location);
    ("orig_var", option TypeVar.to_yojson orig_var);
  ]

and type_content = function
  | T_variable        t -> `List [ `String "t_variable"; TypeVar.to_yojson t]
  | T_sum             t -> `List [ `String "t_sum"; rows t]
  | T_record          t -> `List [ `String "t_record"; rows t]
  | T_arrow           t -> `List [ `String "t_arrow"; arrow t]
  | T_constant        t -> `List [ `String "t_constant"; type_injection t]
  | T_singleton       t -> `List [ `String "t_singleton" ; literal t ]
  | T_for_all         t -> `List [ `String "t_for_all" ; for_all type_expression t]

and type_injection {language;injection;parameters} =
  `Assoc [
    ("language", `String language);
    ("injection", `String (Stage_common.Constant.to_string injection));
    ("parameters", list type_expression parameters)
  ]

and rows {content; layout = l } =
  `Assoc [
    ("content", label_map row_element content);
    ("layout", layout l);
  ]
and row_element {associated_type; michelson_annotation; decl_pos} =
  `Assoc [
    ("associated_type", type_expression associated_type);
    ("michelson_annotation", option (fun s -> `String s) michelson_annotation);
    ("decl_pos", `Int decl_pos);
  ]

and arrow {type1;type2} =
  `Assoc [
    ("type1", type_expression type1);
    ("type2", type_expression type2);
  ]

let rec expression {expression_content=ec;location;type_expression=te} =
  `Assoc [
    ("expression_content", expression_content ec);
    ("location", Location.to_yojson location);
    ("type_expression", type_expression te)
  ]

and expression_content = function
  (* Base *)
  | E_literal     e -> `List [ `String "E_literal"; Stage_common.To_yojson.literal e ]
  | E_constant    e -> `List [ `String "E_constant"; constant e ]
  | E_variable    e -> `List [ `String "E_variable"; ValueVar.to_yojson e ]
  | E_application e -> `List [ `String "E_application"; application e ]
  | E_lambda      e -> `List [ `String "E_lambda"; lambda e ]
  | E_type_abstraction e -> `List [ `String "E_type_abstraction"; type_abs expression e ]
  | E_recursive   e -> `List [ `String "E_recursive"; recursive e ]
  | E_let_in      e -> `List [ `String "E_let_in"; let_in e ]
  | E_raw_code    e -> `List [ `String "E_raw_code"; raw_code e ]
  (* Variant *)
  | E_constructor     e -> `List [ `String "E_constructor"; constructor e ]
  | E_matching        e -> `List [ `String "E_matching"; matching e ]
  (* Record *)
  | E_record          e -> `List [ `String "E_record"; record e ]
  | E_record_accessor e -> `List [ `String "E_record_accessor"; record_accessor e ]
  | E_record_update   e -> `List [ `String "E_record_update"; record_update e ]
  | E_type_inst       e -> `List [ `String "E_type_inst"; type_inst e ]
  | E_assign          e -> `List [ `String "E_assign";   assign expression type_expression e ]

and constant {cons_name;arguments} =
  `Assoc [
    ("cons_name", constant' cons_name);
    ("arguments", list expression arguments);
  ]

and type_inst {forall;type_} =
  `Assoc [
    ("forall", expression forall);
    ("type_", type_expression type_);
  ]

and application {lamb;args} =
  `Assoc [
    ("lamb", expression lamb);
    ("args", expression args);
  ]

and lambda {binder=b;result} =
  `Assoc [
    ("binder", binder type_expression b);
    ("result", expression result);
  ]

and recursive {fun_name;fun_type;lambda=l} =
  `Assoc [
    ("fun_name", ValueVar.to_yojson fun_name);
    ("fun_type", type_expression fun_type);
    ("lambda", lambda l)
  ]

and attribute {inline;no_mutation;public;view;thunk;hidden} =
  `Assoc [
    ("inline", `Bool inline);
    ("no_mutation", `Bool no_mutation);
    ("view", `Bool view);
    ("public", `Bool public);
    ("thunk", `Bool thunk);
    ("hidden", `Bool hidden);
  ]

and type_attribute ({public}: type_attribute) =
  `Assoc [
    ("public", `Bool public)
  ]

and module_attribute ({public}: module_attribute) =
  `Assoc [
    ("public", `Bool public)
  ]


and let_in {let_binder;rhs;let_result;attr} =
  `Assoc [
    ("let_binder", binder type_expression let_binder);
    ("rhs", expression rhs);
    ("let_result", expression let_result);
    ("attr", attribute attr);
  ]

and type_in {type_binder;rhs;let_result} =
  `Assoc [
    ("let_binder", TypeVar.to_yojson type_binder );
    ("rhs", type_expression rhs);
    ("let_result", expression let_result)
  ]

and raw_code {language;code} =
  `Assoc [
    ("language", `String language);
    ("code", expression code);
  ]

and constructor {constructor;element} =
  `Assoc [
    ("constructor", label constructor);
    ("element", expression element);
  ]

and matching {matchee; cases} =
  `Assoc [
    ("matchee", expression matchee);
    ("cases", matching_expr cases);
  ]

and record r = label_map expression r

and record_accessor {record; path} =
  `Assoc [
    ("record", expression record);
    ("path", label path);
  ]

and record_update {record; path; update} =
  `Assoc [
    ("record", expression record);
    ("path", label path);
    ("update", expression update);
  ]

and matching_expr = function
  | Match_variant m -> `List [ `String "Match_variant"; matching_content_variant m ]
  | Match_record m -> `List [ `String "Match_record"; matching_content_record m ]

and matching_content_variant {cases;tv} =
  `Assoc [
    ("cases", list matching_content_case cases);
    ("tv", type_expression tv);
  ]

and matching_content_case {constructor; pattern; body} =
  `Assoc [
    ("constructor", label constructor);
    ("pattern", ValueVar.to_yojson pattern);
    ("body", expression body);
  ]

and matching_content_record {fields; body; tv} =
  `Assoc [
    ("fields", label_map (binder type_expression) fields);
    ("body", expression body);
    ("record_type", type_expression tv);
  ]
