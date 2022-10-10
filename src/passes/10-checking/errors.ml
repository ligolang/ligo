module Snippet = Simple_utils.Snippet
module Location = Simple_utils.Location
module Var = Simple_utils.Var
module Exists_var = Context.Exists_var
open Simple_utils.Display
open Ligo_prim

let stage = "typer"

let type_improve t =
  let open Ast_typed in
  let make_type (module_path, element) =
    match module_path with
    | [] -> t_variable element ()
    | _ ->
      let open Simple_utils.PP_helpers in
      let x =
        Format.asprintf "%a" (list_sep Module_var.pp (tag ".")) module_path
      in
      let y = Format.asprintf "%a" Type_var.pp element in
      t_variable (Type_var.of_input_var (x ^ "." ^ y)) ()
  in
  match t.type_content with
  | T_constant { parameters; _ } when List.length parameters = 0 -> t
  | _ ->
    (match Context.Hashes.find_type t with
     | Some t ->
       let t = make_type t in
       { t with type_meta = t.type_meta; orig_var = t.orig_var }
     | _ -> t)


let rec type_mapper ~f (t : Ast_typed.type_expression) =
  let open Ast_typed in
  let t = f t in
  match t.type_content with
  | T_arrow { type1; type2 } ->
    let type1 = type_mapper ~f type1 in
    let type2 = type_mapper ~f type2 in
    { t with type_content = T_arrow { type1; type2 } }
  | T_constant { language; injection; parameters } ->
    let parameters = List.map ~f:(type_mapper ~f) parameters in
    { t with type_content = T_constant { language; injection; parameters } }
  | T_record { fields; layout } ->
    let fields =
      Record.LMap.map
        (fun ({ associated_type; michelson_annotation; decl_pos } :
               _ Rows.row_element_mini_c)
           : _ Rows.row_element_mini_c ->
          { associated_type = type_mapper ~f associated_type
          ; michelson_annotation
          ; decl_pos
          })
        fields
    in
    { t with type_content = T_record { fields; layout } }
  | T_sum { fields; layout } ->
    let fields =
      Record.LMap.map
        (fun ({ associated_type; michelson_annotation; decl_pos } :
               _ Rows.row_element_mini_c)
           : _ Rows.row_element_mini_c ->
          { associated_type = type_mapper ~f associated_type
          ; michelson_annotation
          ; decl_pos
          })
        fields
    in
    { t with type_content = T_sum { fields; layout } }
  | _ -> t


let type_improve t =
  Context.Hashes.hash_types ();
  type_mapper ~f:type_improve t


type typer_error =
  [ `Typer_mut_var_captured of Location.t * Value_var.t
  | `Typer_ill_formed_type of Location.t * Ast_typed.type_expression
  | `Typer_existential_found of Location.t * Ast_typed.type_expression
  | `Typer_record_mismatch of
    Location.t * Ast_core.expression * Ast_typed.type_expression
  | `Typer_cannot_subtype of
    Location.t * Ast_typed.type_expression * Ast_typed.type_expression
  | `Typer_missing_funarg_annotation of Ast_typed.expression_variable
  | `Typer_unbound_module_variable of Ast_typed.module_variable * Location.t
  | `Typer_unbound_type_variable of Ast_typed.type_variable * Location.t
  | `Typer_unbound_exists_variable of Location.t * Exists_var.t
  | `Typer_unbound_variable of Ast_typed.expression_variable * Location.t
  | `Typer_match_missing_case of Label.t list * Label.t list * Location.t
  | `Typer_unbound_constructor of Label.t * Location.t
  | `Typer_bad_constructor of Location.t * Label.t * Ast_typed.type_expression
  | `Typer_type_app_wrong_arity of Type_var.t option * int * int * Location.t
  | `Typer_michelson_or_no_annotation of Label.t * Location.t
  | `Typer_constant_declaration_tracer of
    Location.t
    * Value_var.t
    * Ast_core.expression
    * Ast_typed.type_expression option
    * typer_error
  | `Typer_match_error of
    Ast_typed.type_expression * Ast_typed.type_expression * Location.t
  | `Typer_should_be_a_function_type of
    Ast_typed.type_expression * Ast_core.expression
  | `Typer_bad_record_access of Label.t * Location.t
  | `Typer_expression_tracer of Ast_core.expression * typer_error
  | `Typer_assert_equal of
    Location.t * Ast_typed.type_expression * Ast_typed.type_expression
  | `Typer_corner_case of string
  | `Typer_bad_collect_loop of Ast_typed.type_expression * Location.t
  | `Typer_expected_record of Location.t * Ast_typed.type_expression
  | `Typer_expected_variant of Location.t * Ast_typed.type_expression
  | `Typer_expected_map of Location.t * Ast_typed.type_expression
  | `Typer_expected of
    Location.t * Ast_typed.type_expression list * Ast_typed.type_expression list
  | `Typer_wrong_param_number of
    Location.t * string * int * Ast_typed.type_expression list
  | `Typer_expected_option of Location.t * Ast_typed.type_expression
  | `Typer_not_matching of
    Location.t * Ast_typed.type_expression * Ast_typed.type_expression
  | `Typer_not_annotated of Location.t
  | `Typer_typeclass_error of
    Location.t
    * Ast_typed.type_expression list list
    * Ast_typed.type_expression list
  | `Typer_uncomparable_types of
    Location.t * Ast_typed.type_expression * Ast_typed.type_expression
  | `Typer_comparator_composed of Location.t * Ast_typed.type_expression
  | `Typer_pattern_do_not_match of Location.t
  | `Typer_pattern_do_not_conform_type of
    Ast_core.type_expression option Ast_core.Pattern.t * Ast_typed.type_expression
  | `Typer_pattern_missing_cases of
    Location.t
    * Syntax_types.t option
    * Ast_core.type_expression option Ast_typed.Pattern.t list
  | `Typer_pattern_redundant_case of Location.t
  | `Typer_wrong_type_for_unit_pattern of Location.t * Ast_typed.type_expression
  | `Typer_constant_since_protocol of
    Location.t * string * Environment.Protocols.t
  | `Typer_occurs_check_failed of
    Location.t * Exists_var.t * Ast_typed.type_expression
  | `Typer_cannot_unify of
    Location.t * Ast_typed.type_expression * Ast_typed.type_expression
  | `Typer_cannot_unify_diff_layout of
    Location.t
    * Ast_typed.type_expression
    * Ast_typed.type_expression
    * Layout.t
    * Layout.t
  | `Typer_unbound_mut_variable of Value_var.t * Location.t
  | `Typer_mismatching_for_each_collection_type of
    Location.t * For_each_loop.collect_type * Ast_typed.type_expression
  | `Typer_mismatching_for_each_binder_arity of
    Location.t * For_each_loop.collect_type * int
  | `Typer_mut_is_polymorphic of Location.t * Ast_typed.type_expression
  ]
[@@deriving poly_constructor { prefix = "typer_" }]

let match_error
  ~(expected : Ast_typed.type_expression)
  ~(actual : Ast_typed.type_expression)
  (loc : Location.t)
  =
  match_error expected actual loc


let rec error_ppformat
  :  display_format:string display_format -> Format.formatter -> typer_error
  -> unit
  =
 fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev ->
    (match a with
     | `Typer_mut_var_captured (loc, var) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid capture of mutable variable \"%a\"@]"
        Snippet.pp
        loc
        Value_var.pp
        var
     | `Typer_existential_found (loc, type_) ->
       Format.fprintf
         f
         "@[<hv>%a@.Underspecified type %a.@.Please add additional \
          annotations.@]"
         Snippet.pp
         loc
         Ast_typed.PP.type_expression
         type_
     | `Typer_ill_formed_type (loc, type_) ->
       Format.fprintf
         f
         "@[<hv>%a@.Invalid type@.Ill formed type %a.@]"
         Snippet.pp
         loc
         Ast_typed.PP.type_expression
         type_
     | `Typer_record_mismatch (loc, _record, type_) ->
       Format.fprintf
         f
         "@[<hv>%a@.Mismatching record labels. Expected record of type %a.@]"
         Snippet.pp
         loc
         Ast_typed.PP.type_expression
         type_
     | `Typer_cannot_unify (loc, type1, type2) ->
       Format.fprintf
         f
         "@[<hv>%a@.Invalid type(s)@.Cannot unify %a with %a.@]%a"
         Snippet.pp
         loc
         Ast_typed.PP.type_expression
         type1
         Ast_typed.PP.type_expression
         type2
         Typediff.PP.t
         (Typediff.get_diff type1 type2)
     | `Typer_cannot_unify_diff_layout (loc, type1, type2, layout1, layout2) ->
       Format.fprintf
         f
         "@[<hv>%a@.Invalid type(s)@.Cannot unify %a with %a due to differing \
          layouts (%a and %a).@]"
         Snippet.pp
         loc
         Ast_typed.PP.type_expression
         type1
         Ast_typed.PP.type_expression
         type2
         Layout.pp
         layout1
         Layout.pp
         layout2
     | `Typer_bad_constructor (loc, label, type_) ->
       Format.fprintf
         f
         "@[<hv>%a.Expected constructor %a in expected sum type %a.]"
         Snippet.pp
         loc
         Label.pp
         label
         Ast_typed.PP.type_expression
         type_
     | `Typer_cannot_subtype (loc, type1, type2) ->
       Format.fprintf
         f
         "@[<hv>%a@.Expected %a, but received %a. Types are not compatitable.@]"
         Snippet.pp
         loc
         Ast_typed.PP.type_expression
         type2
         Ast_typed.PP.type_expression
         type1
     | `Typer_occurs_check_failed (loc, evar, type_) ->
       Format.fprintf
         f
         "@[<hv>%a@.The type variable %a occurs inside %a.@]"
         Snippet.pp
         loc
         Exists_var.pp
         evar
         Ast_typed.PP.type_expression
         type_
     | `Typer_wrong_type_for_unit_pattern (l, t) ->
       Format.fprintf
         f
         "@[<hv>%a@.Variant pattern argument is expected of type %a but is of \
          type unit.@]"
         Snippet.pp
         l
         Ast_typed.PP.type_expression
         t
     | `Typer_pattern_do_not_match loc ->
       Format.fprintf
         f
         "@[<hv>%a@.Pattern do not match returned expression.@]"
         Snippet.pp
         loc
     | `Typer_missing_funarg_annotation v ->
       Format.fprintf
         f
         "@[<hv>%a@.Missing a type annotation for argument \"%a\".@]"
         Snippet.pp
         (Value_var.get_location v)
         Value_var.pp
         v
     | `Typer_unbound_module_variable (mv, loc) ->
       Format.fprintf
         f
         "@[<hv>%a@.Module \"%a\" not found. @]"
         Snippet.pp
         loc
         Module_var.pp
         mv
     | `Typer_unbound_type_variable (tv, loc) ->
       Format.fprintf
         f
         "@[<hv>%a@.Type \"%a\" not found. @]"
         Snippet.pp
         loc
         Type_var.pp
         tv
     | `Typer_unbound_exists_variable (loc, evar) ->
       Format.fprintf
         f
         "@[<hv>%a@.Existential variable \"%a\" not found. @]"
         Snippet.pp
         loc
         Exists_var.pp
         evar
     | `Typer_unbound_variable (v, loc) ->
       Format.fprintf
         f
         "@[<hv>%a@.Variable \"%a\" not found. @]"
         Snippet.pp
         loc
         Value_var.pp
         v
     | `Typer_unbound_mut_variable (v, loc) ->
       Format.fprintf
         f
         "@[<hv>%a@.Mutable variable \"%a\" not found. @]"
         Snippet.pp
         loc
         Value_var.pp
         v
     | `Typer_mismatching_for_each_collection_type (loc, collection_type, type_)
       ->
       Format.fprintf
         f
         "@[<hv>%a@.Expected collection of type \"%a\", but recieved \
          collection of type %a.@]"
         Snippet.pp
         loc
         For_each_loop.pp_collect_type
         collection_type
         Ast_typed.PP.type_expression
         type_
     | `Typer_mismatching_for_each_binder_arity (loc, collection_type, arity) ->
       let expected_arity =
         match collection_type with
         | List | Set -> 1
         | Map -> 2
         | Any -> assert false
       in
       Format.fprintf
         f
         "@[<hv>%a@.Expected for each loop to bind %d variables, but loop \
          binds %d variables.@]"
         Snippet.pp
         loc
         expected_arity
         arity
     | `Typer_match_missing_case (m, v, loc) ->
       let missing =
         List.fold_left
           ~f:(fun all o ->
             match List.find ~f:(fun f -> Label.compare f o = 0) v with
             | Some _ -> all
             | None ->
               let (Label o) = o in
               o :: all)
           ~init:[]
           m
       in
       let missing = String.concat ~sep:", " missing in
       Format.fprintf
         f
         "@[<hv>%a@.Pattern matching is not exhaustive.@.Cases that are \
          missing: %s. @]"
         Snippet.pp
         loc
         missing
     | `Typer_unbound_constructor (c, loc) ->
       Format.fprintf
         f
         "@[<hv>%a@.Constructor \"%a\" not found. @]"
         Snippet.pp
         loc
         Label.pp
         c
     | `Typer_type_app_wrong_arity (op_opt, e, a, loc) ->
       let aux : Format.formatter -> Type_var.t option -> unit =
        fun ppf operator_opt ->
         match operator_opt with
         | Some v -> Format.fprintf ppf " %a" Type_var.pp v
         | None -> ()
       in
       Format.fprintf
         f
         "@[<hv>%a@ Type%a is applied to a wrong number of arguments, \
          expected: %i got: %i@]"
         Snippet.pp
         loc
         aux
         op_opt
         e
         a
     | `Typer_michelson_or_no_annotation (c, loc) ->
       Format.fprintf
         f
         "@[<hv>%a@.Incorrect usage of type \"michelson_or\".@.The contructor \
          \"%a\" must be annotated with a variant type. @]"
         Snippet.pp
         loc
         Label.pp
         c
     | `Typer_constant_declaration_tracer (_, _, _, _, err) ->
       error_ppformat ~display_format f err
     | `Typer_match_error (expected, actual, loc) ->
       Format.fprintf
         f
         "@[<hv>%a@.Pattern matching over an expression of an incorrect \
          type.@.Type \"%a\" was expected, but got type \"%a\". @]"
         Snippet.pp
         loc
         Ast_typed.PP.type_expression
         (type_improve expected)
         Ast_typed.PP.type_expression
         (type_improve actual)
     | `Typer_should_be_a_function_type (actual, e) ->
       Format.fprintf
         f
         "@[<hv>%a@.Invalid type.@.Expected a function type, but got \"%a\". @]"
         Snippet.pp
         e.location
         Ast_typed.PP.type_expression
         (type_improve actual)
     | `Typer_bad_record_access (field, loc) ->
       Format.fprintf
         f
         "@[<hv>%a@.Invalid record field \"%a\" in record. @]"
         Snippet.pp
         loc
         Label.pp
         field
     | `Typer_corner_case desc ->
       Format.fprintf f "@[<hv>A type system corner case occurred:@.%s@]" desc
     | `Typer_bad_collect_loop (t, loc) ->
       Format.fprintf
         f
         "@[<hv>%a@.Bounded loop over a value with an incorrect \
          type.@.Expected a value with type: \"list\", \"set\" or \"map\", but \
          got a value of type \"%a\". @]"
         Snippet.pp
         loc
         Ast_typed.PP.type_expression
         (type_improve t)
     | `Typer_expression_tracer (_, err) -> error_ppformat ~display_format f err
     | `Typer_not_annotated l ->
       Format.fprintf
         f
         "@[<hv>%a@.Can't infer the type of this value, please add a type \
          annotation.@]"
         Snippet.pp
         l
     | `Typer_comparator_composed (loc, _a) ->
       Format.fprintf
         f
         "@[<hv>%a@.Invalid arguments.@.Only composed types of not more than \
          two element are allowed to be compared. @]"
         Snippet.pp
         loc
     | `Typer_assert_equal (loc, expected, actual) ->
       Format.fprintf
         f
         "@[<hv>%a@.Invalid type(s).@.Expected: \"%a\", but got: \"%a\". @]"
         Snippet.pp
         loc
         Ast_typed.PP.type_expression_orig
         (type_improve expected)
         Ast_typed.PP.type_expression_orig
         (type_improve actual)
     | `Typer_expected_record (loc, t) ->
       Format.fprintf
         f
         "@[<hv>%a@.Invalid argument.@.Expected a record, but got an argument \
          of type \"%a\". @]"
         Snippet.pp
         loc
         Ast_typed.PP.type_expression
         (type_improve t)
     | `Typer_expected_variant (loc, t) ->
       Format.fprintf
         f
         "@[<hv>%a@.Invalid argument.@.Expected a variant, but got an argument \
          of type \"%a\". @]"
         Snippet.pp
         loc
         Ast_typed.PP.type_expression
         (type_improve t)
     | `Typer_wrong_param_number (loc, name, expected, actual) ->
       Format.fprintf
         f
         "@[<hv>%a@.Function \"%s\" called with wrong number of \
          arguments.@.Expected %d arguments, got %d arguments. @]"
         Snippet.pp
         loc
         name
         expected
         (List.length actual)
     | `Typer_expected_map (loc, e) ->
       Format.fprintf
         f
         "@[<hv>%a@.Incorrect argument.@.Expected a map, but got an argument \
          of type \"%a\". @]"
         Snippet.pp
         loc
         Ast_typed.PP.type_expression
         (type_improve e)
     | `Typer_expected_option (loc, e) ->
       Format.fprintf
         f
         "@[<hv>%a@.Incorrect argument.@.Expected an option, but got an \
          argument of type \"%a\". @]"
         Snippet.pp
         loc
         Ast_typed.PP.type_expression
         (type_improve e)
     | `Typer_not_matching (loc, t1, t2) ->
       Format.fprintf
         f
         "@[<hv>%a@.These types are not matching:@. - %a@. - %a@]"
         Snippet.pp
         loc
         Ast_typed.PP.type_expression
         (type_improve t1)
         Ast_typed.PP.type_expression
         (type_improve t2)
     | `Typer_uncomparable_types (loc, a, b) ->
       Format.fprintf
         f
         "@[<hv>%a@.Invalid arguments.@.These types cannot be compared: \"%a\" \
          and \"%a\". @]"
         Snippet.pp
         loc
         Ast_typed.PP.type_expression
         (type_improve a)
         Ast_typed.PP.type_expression
         (type_improve b)
     | `Typer_typeclass_error (loc, exps, acts) ->
       let open Simple_utils.PP_helpers in
       let printl printer ppf args =
         Format.fprintf ppf "(%a)" (list_sep printer (const ", ")) args
       in
       Format.fprintf
         f
         "@[<hv>%a@.Invalid arguments.@.Expected an argument of type %a, but \
          got an argument of type %a. @]"
         Snippet.pp
         loc
         (list_sep (printl Ast_typed.PP.type_expression) (const " or "))
         exps
         (list_sep Ast_typed.PP.type_expression (const ", "))
         acts
     | `Typer_expected (loc, exps, acts) ->
       let open Simple_utils.PP_helpers in
       Format.fprintf
         f
         "@[<hv>%a@.Cannot match arguments for operation.@.Expected arguments \
          with types:%a@.but got arguments with types:%a. @]"
         Snippet.pp
         loc
         (list_sep_prep Ast_typed.PP.type_expression (tag "@.- "))
         exps
         (list_sep_prep Ast_typed.PP.type_expression (tag "@.- "))
         acts
     | `Typer_pattern_do_not_conform_type (p, t) ->
       let pf ppf value =
         match p.location with
         | Virtual _ ->
           Format.fprintf
             ppf
             "%a "
             (Ast_core.Pattern.pp Ast_core.PP.type_expression_option)
             value
         | File _ -> ()
       in
       Format.fprintf
         f
         "@[<hv>%a@.Pattern %anot of the expected type %a @]"
         Snippet.pp
         p.location
         pf
         p
         Ast_typed.PP.type_expression
         (type_improve t)
     | `Typer_pattern_missing_cases (loc, syntax, ps) ->
       let ps =
         List.fold ps ~init:"" ~f:(fun s p ->
           let s' =
             let p = Untyper.untype_pattern p in
             Desugaring.Decompiler.decompile_pattern_to_string ~syntax p
           in
           s ^ "- " ^ s' ^ "\n")
       in
       Format.fprintf
         f
         "@[<hv>%a@.Error : this pattern-matching is not exhaustive.@.Here are \
          examples of cases that are not matched:@.%s@]"
         Snippet.pp
         loc
         ps
     | `Typer_pattern_redundant_case loc ->
       Format.fprintf
         f
         "@[<hv>%a@.Error : this match case is unused.@]"
         Snippet.pp
         loc
     | `Typer_constant_since_protocol (loc, constant, protocol) ->
       let protocol_name = Environment.Protocols.variant_to_string protocol in
       Format.fprintf
         f
         "@[<hv>%a@.%s is supported in protocol %s onwards.@.Hint: pass the \
          compiler option `--protocol %s`.@]"
         Snippet.pp
         loc
         constant
         (String.capitalize protocol_name)
         protocol_name
     | `Typer_mut_is_polymorphic (loc, type_) ->
       Format.fprintf
         f
         "@[<hv>%a@.Mutable binding has the polymorphic type %a@.Hint: Add an \
          annotation.@]"
         Snippet.pp
         loc
         Ast_typed.PP.type_expression
         type_)

let rec error_json : typer_error -> Simple_utils.Error.t = fun e ->
  let open Simple_utils.Error in
  match e with
  | `Typer_mut_var_captured (location, var) ->
    let message = Format.asprintf "Invalid capture of mutable variable \"%a\"" Value_var.pp var in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_existential_found (location, type_) ->
    let message = Format.asprintf "Underspecified type %a.@.Please add additional \
      annotations." Ast_typed.PP.type_expression type_ 
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_ill_formed_type (location, type_) ->
    let message = Format.asprintf "Invalid type@.Ill formed type %a." Ast_typed.PP.type_expression type_ in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_record_mismatch (location, _record, type_) ->
    let message = Format.asprintf "Mismatching record labels. Expected record of type %a." Ast_typed.PP.type_expression type_ in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_cannot_unify (location, type1, type2) ->
    let message = Format.asprintf "Invalid type(s)@.Cannot unify %a with %a." Ast_typed.PP.type_expression type1 Ast_typed.PP.type_expression type2 in 
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_cannot_unify_diff_layout (location, type1, type2, layout1, layout2) ->
    let message = Format.asprintf
      "Invalid type(s)@.Cannot unify %a with %a due to differing \
        layouts (%a and %a)."
      Ast_typed.PP.type_expression
      type1
      Ast_typed.PP.type_expression
      type2
      Layout.pp
      layout1
      Layout.pp
      layout2 in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_bad_constructor (location, label, type_) ->
    let message = Format.asprintf
      "Expected constructor %a in expected sum type %a."
      Label.pp
      label
      Ast_typed.PP.type_expression
      type_ in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_cannot_subtype (location, type1, type2) ->
    let message = Format.asprintf
      "Expected %a, but received %a. Types are not compatitable."
      Ast_typed.PP.type_expression
      type2
      Ast_typed.PP.type_expression
      type1 in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_occurs_check_failed (location, evar, type_) ->
    let message = Format.asprintf
      "The type variable %a occurs inside %a."
      Exists_var.pp
      evar
      Ast_typed.PP.type_expression
      type_ in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_wrong_type_for_unit_pattern (location, t) ->
    let message = Format.asprintf
      "Variant pattern argument is expected of type %a but is of \
        type unit."
      Ast_typed.PP.type_expression
      t in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_pattern_do_not_match location ->
    let message = Format.asprintf
      "Pattern do not match returned expression." in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_missing_funarg_annotation v ->
    let message = Format.asprintf
      "Missing a type annotation for argument \"%a\"." Value_var.pp v in
    let location = Value_var.get_location v in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_unbound_module_variable (mv, location) ->
    let message = Format.asprintf "Module \"%a\" not found." Module_var.pp mv in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_unbound_type_variable (tv, location) ->
    let message = Format.asprintf "Type \"%a\" not found." Type_var.pp tv in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_unbound_exists_variable (location, evar) ->
    let message = Format.asprintf "Existential variable \"%a\" not found." Exists_var.pp evar in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_unbound_variable (v, location) ->
    let message = Format.asprintf "Variable \"%a\" not found." Value_var.pp v in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_unbound_mut_variable (v, location) ->
    let message = Format.asprintf "Mutable variable \"%a\" not found." Value_var.pp v in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_mismatching_for_each_collection_type (location, collection_type, type_)
    ->
    let message = Format.asprintf
      "Expected collection of type \"%a\", but recieved \
        collection of type %a."
      For_each_loop.pp_collect_type
      collection_type
      Ast_typed.PP.type_expression
      type_ in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_mismatching_for_each_binder_arity (location, collection_type, arity) ->
    let expected_arity =
      match collection_type with
      | List | Set -> 1
      | Map -> 2
      | Any -> assert false
    in
    let message = Format.asprintf
      "Expected for each loop to bind %d variables, but loop \
        binds %d variables." expected_arity arity in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_match_missing_case (m, v, location) ->
    let missing =
      List.fold_left
        ~f:(fun all o ->
          match List.find ~f:(fun f -> Label.compare f o = 0) v with
          | Some _ -> all
          | None ->
            let (Label o) = o in
            o :: all)
        ~init:[]
        m
    in
    let missing = String.concat ~sep:", " missing in
    let message = Format.asprintf
      "Pattern matching is not exhaustive.@.Cases that are \
        missing: %s." missing in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_unbound_constructor (c, location) ->
    let message = Format.asprintf "Constructor \"%a\" not found." Label.pp c in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_type_app_wrong_arity (op_opt, e, a, location) ->
    let aux : Format.formatter -> Type_var.t option -> unit =
      fun ppf operator_opt ->
      match operator_opt with
      | Some v -> Format.fprintf ppf " %a" Type_var.pp v
      | None -> ()
    in
    let message = Format.asprintf
      "Type%a is applied to a wrong number of arguments, \
        expected: %i got: %i"
      aux
      op_opt
      e
      a in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_michelson_or_no_annotation (c, location) ->
    let message = Format.asprintf
      "Incorrect usage of type \"michelson_or\".@.The contructor \
        \"%a\" must be annotated with a variant type."
      Label.pp c in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_constant_declaration_tracer (_, _, _, _, err) ->
    let children = error_json err in
    let message = "Typer constant declaration tracer" in
    let content = make_content ~message ~children () in
    make ~stage ~content
  | `Typer_match_error (expected, actual, location) ->
    let message = Format.asprintf
      "Pattern matching over an expression of an incorrect \
        type.@.Type \"%a\" was expected, but got type \"%a\"."
      Ast_typed.PP.type_expression
      (type_improve expected)
      Ast_typed.PP.type_expression
      (type_improve actual) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_should_be_a_function_type (actual, e) ->
    let message = Format.asprintf
      "Invalid type.@.Expected a function type, but got \"%a\"."
      Ast_typed.PP.type_expression
      (type_improve actual) in
    let location = e.location in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_bad_record_access (field, location) ->
    let message = Format.asprintf
      "Invalid record field \"%a\" in record."
      Label.pp
      field in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_corner_case desc ->
    let message = Format.asprintf "A type system corner case occurred:@.%s" desc in
    let content = make_content ~message () in
    make ~stage ~content
  | `Typer_bad_collect_loop (t, location) ->
    let message = Format.asprintf
      "Bounded loop over a value with an incorrect \
        type.@.Expected a value with type: \"list\", \"set\" or \"map\", but \
        got a value of type \"%a\"."
      Ast_typed.PP.type_expression
      (type_improve t) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_expression_tracer (_, err) -> 
    let children = error_json err in
    let message = "Typer expression tracer" in
    let content = make_content ~message ~children () in
    make ~stage ~content
  | `Typer_not_annotated location ->
    let message = Format.asprintf
      "Can't infer the type of this value, please add a type \
        annotation."
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_comparator_composed (location, _a) ->
    let message = Format.asprintf
      "Invalid arguments.@.Only composed types of not more than \
        two element are allowed to be compared." in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_assert_equal (location, expected, actual) ->
    let message = Format.asprintf
      ".Invalid type(s).@.Expected: \"%a\", but got: \"%a\"."
      Ast_typed.PP.type_expression_orig
      (type_improve expected)
      Ast_typed.PP.type_expression_orig
      (type_improve actual) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_expected_record (location, t) ->
    let message = Format.asprintf
      "Invalid argument.@.Expected a record, but got an argument \
        of type \"%a\". @]"
      Ast_typed.PP.type_expression
      (type_improve t) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_expected_variant (location, t) ->
    let message = Format.asprintf
      "Invalid argument.@.Expected a variant, but got an argument \
        of type \"%a\"."
      Ast_typed.PP.type_expression
      (type_improve t) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_wrong_param_number (location, name, expected, actual) ->
    let message = Format.asprintf
      "Function \"%s\" called with wrong number of \
        arguments.@.Expected %d arguments, got %d arguments."
      name
      expected
      (List.length actual) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_expected_map (location, e) ->
    let message = Format.asprintf
      "Incorrect argument.@.Expected a map, but got an argument \
        of type \"%a\"."
      Ast_typed.PP.type_expression
      (type_improve e) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_expected_option (location, e) ->
    let message = Format.asprintf
      "Incorrect argument.@.Expected an option, but got an \
        argument of type \"%a\"."
      Ast_typed.PP.type_expression
      (type_improve e) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_not_matching (location, t1, t2) ->
    let message = Format.asprintf
      "These types are not matching:@. - %a@. - %a"
      Ast_typed.PP.type_expression
      (type_improve t1)
      Ast_typed.PP.type_expression
      (type_improve t2) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_uncomparable_types (location, a, b) ->
    let message = Format.asprintf
      "Invalid arguments.@.These types cannot be compared: \"%a\" \
        and \"%a\"."
      Ast_typed.PP.type_expression
      (type_improve a)
      Ast_typed.PP.type_expression
      (type_improve b) in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_typeclass_error (location, exps, acts) ->
    let open Simple_utils.PP_helpers in
    let printl printer ppf args =
      Format.fprintf ppf "(%a)" (list_sep printer (const ", ")) args
    in
    let message = Format.asprintf
      "Invalid arguments.@.Expected an argument of type %a, but \
        got an argument of type %a."
      (list_sep (printl Ast_typed.PP.type_expression) (const " or "))
      exps
      (list_sep Ast_typed.PP.type_expression (const ", "))
      acts in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_expected (location, exps, acts) ->
    let open Simple_utils.PP_helpers in
    let message = Format.asprintf
      "Cannot match arguments for operation.@.Expected arguments \
        with types:%a@.but got arguments with types:%a."
      (list_sep_prep Ast_typed.PP.type_expression (tag "@.- "))
      exps
      (list_sep_prep Ast_typed.PP.type_expression (tag "@.- "))
      acts in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_pattern_do_not_conform_type (p, t) ->
    let pf ppf value =
      match p.location with
      | Virtual _ ->
        Format.fprintf
          ppf
          "%a "
          (Ast_core.Pattern.pp Ast_core.PP.type_expression_option)
          value
      | File _ -> ()
    in
    let message = Format.asprintf
      "Pattern %anot of the expected type %a"
      pf
      p
      Ast_typed.PP.type_expression
      (type_improve t) in
    let location = p.location in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_pattern_missing_cases (location, syntax, ps) ->
    let ps =
      List.fold ps ~init:"" ~f:(fun s p ->
        let s' =
          let p = Untyper.untype_pattern p in
          Desugaring.Decompiler.decompile_pattern_to_string ~syntax p
        in
        s ^ "- " ^ s' ^ "\n")
    in
    let message = Format.asprintf
      "Error : this pattern-matching is not exhaustive.@.Here are \
        examples of cases that are not matched:@.%s" ps in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_pattern_redundant_case location ->
    let message = Format.asprintf "Error : this match case is unused." in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_constant_since_protocol (location, constant, protocol) ->
    let protocol_name = Environment.Protocols.variant_to_string protocol in
    let message = Format.asprintf
      "%s is supported in protocol %s onwards.@.Hint: pass the \
        compiler option `--protocol %s`."
      constant
      (String.capitalize protocol_name)
      protocol_name in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Typer_mut_is_polymorphic (location, type_) ->
    let message = Format.asprintf
      "Mutable binding has the polymorphic type %a@.Hint: Add an \
        annotation."
      Ast_typed.PP.type_expression
      type_ in
    let content = make_content ~message ~location () in
    make ~stage ~content
