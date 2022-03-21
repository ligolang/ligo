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


let rec type_expression {type_content=tc;sugar;location} =
  `Assoc [
    ("type_content", type_content tc);
    ("sugar", option Ast_sugar.Yojson.type_expression sugar);
    ("location", Location.to_yojson location);
  ]

and type_content = function
  | T_variable        t -> `List [ `String "t_variable"; TypeVar.to_yojson t]
  | T_sum             t -> `List [ `String "t_sum"; rows t]
  | T_record          t -> `List [ `String "t_record"; rows t]
  | T_arrow           t -> `List [ `String "t_arrow"; arrow t]
  | T_app             t -> `List [ `String "t_app";      t_app type_expression t]
  | T_module_accessor t -> `List [ `String "t_module_accessor"; module_access TypeVar.to_yojson t]
  | T_singleton       t -> `List [ `String "t_singleton" ; literal t ]
  | T_abstraction     t -> `List [ `String "t_abstraction" ; for_all type_expression t ]
  | T_for_all         t -> `List [ `String "t_for_all" ; for_all type_expression t ]


and rows {fields; layout = l } =
  `Assoc [
    ("content", label_map row_element fields);
    ("layout", option layout l);
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

let rec expression {expression_content=ec;location;sugar} =
  `Assoc [
    ("expression_content", expression_content ec);
    ("location", Location.to_yojson location);
    ("core", option Ast_sugar.Yojson.expression sugar);
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
  | E_type_in     e -> `List [ `String "E_type_in"; type_in e ]
  | E_mod_in      e -> `List [ `String "E_mod_in"; mod_in e ]
  | E_raw_code    e -> `List [ `String "E_raw_code"; raw_code e ]
  (* Variant *)
  | E_constructor     e -> `List [ `String "E_constructor"; constructor expression e ]
  | E_matching        e -> `List [ `String "E_matching"; match_exp expression type_expression e ]
  (* Record *)
  | E_record          e -> `List [ `String "E_record"; record e ]
  | E_record_accessor e -> `List [ `String "E_record_accessor"; record_accessor e ]
  | E_record_update   e -> `List [ `String "E_record_update"; record_update e ]
  | E_module_accessor e -> `List [ `String "E_module_accessor"; module_access ValueVar.to_yojson e]
  | E_ascription      e -> `List [ `String "E_module_accessor"; ascription expression type_expression e]

and constant {cons_name;arguments} =
  `Assoc [
    ("cons_name", constant' cons_name);
    ("arguments", list expression arguments);
  ]

and application {lamb;args} =
  `Assoc [
    ("lamb", expression lamb);
    ("args", expression args);
  ]

and lambda {binder=b;output_type;result} =
  `Assoc [
    ("binder", binder type_expression b);
    ("output_type", option type_expression output_type);
    ("result", expression result);
  ]

and recursive {fun_name;fun_type;lambda=l} =
  `Assoc [
    ("fun_name", ValueVar.to_yojson fun_name);
    ("fun_type", type_expression fun_type);
    ("lambda", lambda l)
  ]

and let_in {let_binder;rhs;let_result;attr} =
  `Assoc [
    ("let_binder", binder type_expression let_binder);
    ("rhs", expression rhs);
    ("let_result", expression let_result);
    ("attr", known_attribute attr);
  ]

and type_in {type_binder;rhs;let_result} =
  `Assoc [
    ("type_binder", TypeVar.to_yojson type_binder);
    ("rhs", type_expression rhs);
    ("let_result", expression let_result)
  ]


and known_attribute {inline;no_mutation;public;view} =
  `Assoc [
    ("inline", `Bool inline);
    ("no_mutation", `Bool no_mutation);
    ("public", `Bool public);
    ("view", `Bool view);
  ]

and type_attribute ({public}: type_attribute) =
  `Assoc [
    ("public", `Bool public);
  ]

and module_attribute {public} =
  `Assoc [
    ("public", `Bool public);
  ]


and mod_in m =
  Stage_common.To_yojson.mod_in expression type_expression known_attribute type_attribute module_attribute m

and raw_code {language;code} =
  `Assoc [
    ("language", `String language);
    ("code", expression code);
  ]

and constructor expression {constructor;element} =
  `Assoc [
    ("constructor", label constructor);
    ("element", expression element);
  ]

and matching x = match_exp expression type_expression x

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


and declaration_type x =
  Stage_common.To_yojson.declaration_type type_expression type_attribute x

and declaration_constant x =
  Stage_common.To_yojson.declaration_constant expression type_expression known_attribute x

and declaration_module x =
  Stage_common.To_yojson.declaration_module expression type_expression known_attribute type_attribute module_attribute x

and declaration x =
  Stage_common.To_yojson.declaration expression type_expression known_attribute type_attribute module_attribute x

and declarations x =
  Stage_common.To_yojson.declarations expression type_expression known_attribute type_attribute module_attribute x
(* Environment *)

let environment_element_definition_declaration {expression=e; free_variables} =
  `Assoc [
    ("expression", expression e);
    ("free_variables", list ValueVar.to_yojson free_variables);
  ]

let environment_element_definition = function
  | ED_binder  -> `List [ `String "ED_binder"; `Null]
  | ED_declaration ed -> `List [ `String "ED_declaration"; environment_element_definition_declaration ed]

let rec environment_element {type_value;definition} =
  `Assoc [
    ("type_value", type_expression type_value);
    ("definition", environment_element_definition definition);
  ]

and environment_binding {expr_var;env_elt} =
  `Assoc [
    ("expr_var", ValueVar.to_yojson expr_var);
    ("env_elt", environment_element env_elt);
  ]
and expression_environment e = list environment_binding e

and type_environment_binding {type_variable;type_} =
  `Assoc [
    ("type_variable", TypeVar.to_yojson type_variable);
    ("type_", type_expression type_);
  ]
and type_environment e = list type_environment_binding e

and module_environment_binding {module_variable; module_} =
  `Assoc [
    ("module_name", ModuleVar.to_yojson module_variable);
    ("module_", environment module_)
  ]

and module_environment e = list module_environment_binding e

and environment {expression_environment=ee;type_environment=te;module_environment=me} =
  `Assoc [
    ("expression_environment", expression_environment ee);
    ("type_environment", type_environment te);
    ("module_environment", module_environment me)
  ]
