module Snippet = Simple_utils.Snippet
module Location = Simple_utils.Location
module List = Simple_utils.List
open Simple_utils.Display
open Ligo_prim

type 'err with_loc = Location.t -> 'err

let stage = "typer"

let type_improve t =
  let open Type in
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
  match t.content with
  | T_construct { parameters; _ } when List.length parameters = 0 -> t
  | _ ->
    (match Context.Hashes.find_type t with
    | Some t ->
      let t = make_type t in
      { t with meta = t.meta; orig_var = t.orig_var }
    | _ -> t)


let rec type_mapper ~f (t : Type.t) =
  let open Type in
  let t = f t in
  let return content = { t with content } in
  match t.content with
  | T_arrow arr ->
    let arr = Arrow.map (type_mapper ~f) arr in
    return @@ T_arrow arr
  | T_construct { language; constructor; parameters } ->
    let parameters = List.map ~f:(type_mapper ~f) parameters in
    return @@ T_construct { language; constructor; parameters }
  | T_record row ->
    let row = row_mapper ~f row in
    return @@ T_record row
  | T_sum row ->
    let row = row_mapper ~f row in
    return @@ T_sum row
  | _ -> t


and row_mapper ~f ({ fields; layout } : Type.row) : Type.row =
  let fields =
    Record.map ~f:(Rows.map_row_element_mini_c (type_mapper ~f)) fields
  in
  { fields; layout }


let type_improve t =
  Context.Hashes.hash_types ();
  type_mapper ~f:type_improve t


type typer_error =
  [ `Typer_mut_var_captured of Value_var.t * Location.t
  | `Typer_ill_formed_type of Type.t * Location.t
  | `Typer_record_mismatch of Ast_core.expression * Type.t * Location.t
  | `Typer_cannot_subtype of Type.t * Type.t * Location.t
  | `Typer_corner_case of string * Location.t
  | `Typer_occurs_check_failed of Type_var.t * Type.t * Location.t
  | `Typer_pattern_missing_cases of
    Syntax_types.t option
    * Ast_core.type_expression option Ast_typed.Pattern.t list
    * Location.t
  | `Typer_pattern_redundant_case of Location.t
  | `Typer_cannot_unify_diff_layout of
    Type.t * Type.t * Type.layout * Type.layout * Location.t
  | `Typer_cannot_unify of Type.t * Type.t * Location.t
  | `Typer_assert_equal of
    Ast_typed.type_expression * Ast_typed.type_expression * Location.t
  | `Typer_unbound_module of Module_var.t list * Location.t
  | `Typer_unbound_texists_var of Type_var.t * Location.t
  | `Typer_unbound_type_variable of Type_var.t * Location.t
  | `Typer_unbound_module_variable of Module_var.t * Location.t
  | `Typer_type_app_wrong_arity of Type_var.t option * int * int * Location.t
  | `Typer_literal_type_mismatch of Type.t * Type.t * Location.t
  | `Typer_bad_record_access of Label.t * Location.t
  | `Typer_bad_constructor of Label.t * Type.t * Location.t
  | `Typer_unbound_variable of Value_var.t * Location.t
  | `Typer_not_annotated of Location.t
  | `Typer_expected_record of Type.t * Location.t
  | `Typer_michelson_or_no_annotation of Label.t * Location.t
  | `Typer_unbound_constructor of Label.t * Location.t
  | `Typer_unbound_mut_variable of Value_var.t * Location.t
  | `Typer_mut_is_polymorphic of Type.t * Location.t
  | `Typer_mismatching_for_each_collection_type of
    For_each_loop.collect_type * Type.t * Location.t
  | `Typer_mismatching_for_each_binder_arity of int * int * Location.t
  | `Typer_should_be_a_function_type of
    Type.t * Ast_core.expression * Location.t
  | `Typer_pattern_do_not_match of Location.t
  | `Typer_pattern_do_not_conform_type of
    Ast_core.type_expression option Ast_core.Pattern.t * Type.t * Location.t
  | `Typer_uncomparable_types of Type.t * Type.t * Location.t
  | `Typer_comparator_composed of Type.t * Location.t
  | `Typer_cannot_decode_texists of Type.t * Location.t
  ]
[@@deriving poly_constructor { prefix = "typer_" }]

let error_ppformat
    :  display_format:string display_format -> Format.formatter -> typer_error
    -> unit
  =
 fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev ->
    (match a with
    | `Typer_mut_var_captured (var, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid capture of mutable variable \"%a\"@]"
        Snippet.pp
        loc
        Value_var.pp
        var
    | `Typer_ill_formed_type (type_, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid type@.Ill formed type %a.@]"
        Snippet.pp
        loc
        Type.pp
        type_
    | `Typer_record_mismatch (_record, type_, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Mismatching record labels. Expected record of type %a.@]"
        Snippet.pp
        loc
        Type.pp
        type_
    | `Typer_cannot_subtype (type1, type2, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Expected %a, but received %a. Types are not compatitable.@]"
        Snippet.pp
        loc
        Type.pp
        type2
        Type.pp
        type1
    | `Typer_corner_case (desc, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.A type system corner case occurred:@.%s@]"
        Snippet.pp
        loc
        desc
    | `Typer_occurs_check_failed (tvar, type_, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.The type variable ^%a occurs inside %a.@]"
        Snippet.pp
        loc
        Type_var.pp
        tvar
        Type.pp
        type_
    | `Typer_pattern_missing_cases (syntax, ps, loc) ->
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
    | `Typer_cannot_unify (type1, type2, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid type(s)@.Cannot unify %a with %a.@]"
        Snippet.pp
        loc
        Type.pp
        type1
        Type.pp
        type2
    | `Typer_cannot_unify_diff_layout (type1, type2, layout1, layout2, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid type(s)@.Cannot unify %a with %a due to differing \
         layouts (%a and %a).@]"
        Snippet.pp
        loc
        Type.pp
        type1
        Type.pp
        type2
        Type.pp_layout
        layout1
        Type.pp_layout
        layout2
    | `Typer_bad_constructor (label, type_, loc) ->
      Format.fprintf
        f
        "@[<hv>%a.Expected constructor %a in expected sum type %a.]"
        Snippet.pp
        loc
        Label.pp
        label
        Type.pp
        type_
    | `Typer_pattern_do_not_match loc ->
      Format.fprintf
        f
        "@[<hv>%a@.Pattern do not match returned expression.@]"
        Snippet.pp
        loc
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
    | `Typer_unbound_texists_var (tvar, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Existential variable \"^%a\" not found. @]"
        Snippet.pp
        loc
        Type_var.pp
        tvar
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
    | `Typer_mismatching_for_each_collection_type (collection_type, type_, loc)
      ->
      Format.fprintf
        f
        "@[<hv>%a@.Expected collection of type \"%a\", but recieved collection \
         of type %a.@]"
        Snippet.pp
        loc
        For_each_loop.pp_collect_type
        collection_type
        Type.pp
        type_
    | `Typer_mismatching_for_each_binder_arity
        (expected_arity, recieved_arity, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Expected for each loop to bind %d variables, but loop binds \
         %d variables.@]"
        Snippet.pp
        loc
        expected_arity
        recieved_arity
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
        "@[<hv>%a@ Type%a is applied to a wrong number of arguments, expected: \
         %i got: %i@]"
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
    | `Typer_should_be_a_function_type (lamb_type, _args, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid type.@.Expected a function type, but got \"%a\". @]"
        Snippet.pp
        loc
        Type.pp
        (type_improve lamb_type)
    | `Typer_bad_record_access (field, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid record field \"%a\" in record. @]"
        Snippet.pp
        loc
        Label.pp
        field
    | `Typer_not_annotated l ->
      Format.fprintf
        f
        "@[<hv>%a@.Can't infer the type of this value, please add a type \
         annotation.@]"
        Snippet.pp
        l
    | `Typer_comparator_composed (_a, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid arguments.@.Only composed types of not more than \
         two element are allowed to be compared. @]"
        Snippet.pp
        loc
    | `Typer_assert_equal (expected, actual, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid type(s).@.Expected: \"%a\", but got: \"%a\". @]"
        Snippet.pp
        loc
        Ast_typed.PP.type_expression_orig
        expected
        Ast_typed.PP.type_expression_orig
        actual
    | `Typer_expected_record (type_, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid argument.@.Expected a record, but got an argument \
         of type \"%a\". @]"
        Snippet.pp
        loc
        Type.pp
        (type_improve type_)
    | `Typer_uncomparable_types (type1, type2, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid arguments.@.These types cannot be compared: \"%a\" \
         and \"%a\". @]"
        Snippet.pp
        loc
        Type.pp
        (type_improve type1)
        Type.pp
        (type_improve type2)
    | `Typer_pattern_do_not_conform_type (pat, type_, _loc) ->
      let pf ppf value =
        match pat.location with
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
        pat.location
        pf
        pat
        Type.pp
        (type_improve type_)
    | `Typer_mut_is_polymorphic (type_, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Mutable binding has the polymorphic type %a@.Hint: Add an \
         annotation.@]"
        Snippet.pp
        loc
        Type.pp
        type_
    | `Typer_unbound_module (path, loc) ->
      let rec pp_path ppf path =
        match path with
        | [] -> failwith "Empty path"
        | [ mvar ] -> Format.fprintf ppf "%a" Module_var.pp mvar
        | mvar :: path ->
          Format.fprintf ppf "%a.%a" Module_var.pp mvar pp_path path
      in
      Format.fprintf
        f
        "@[<hv>%a@. Module \"%a\" not found.@]"
        Snippet.pp
        loc
        pp_path
        path
    | `Typer_cannot_decode_texists (type_, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Underspecified type %a.@.Please add additional \
         annotations.@]"
        Snippet.pp
        loc
        Type.pp
        type_
    | `Typer_literal_type_mismatch (lit_type, expected_type, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Expected \"%a\", but got: \"%a\".@]"
        Snippet.pp
        loc
        Type.pp
        expected_type
        Type.pp
        lit_type)

let error_jsonformat : typer_error -> Yojson.Safe.t =
  fun _ -> assert false
(* 
let error_jsonformat : typer_error -> Yojson.Safe.t =
 fun a ->
  let json_error ~stage ~content =
    `Assoc
      [ "status", `String "error"; "stage", `String stage; "content", content ]
  in
  match a with
  | `Typer_mut_var_captured (loc, var) ->
    let message = "Invalid capture of mutable variable" in
    let content =
      `Assoc
        [ "message", `String message
        ; "location", Location.to_yojson loc
        ; "var", Value_var.to_yojson var
        ]
    in
    json_error ~stage ~content
  | `Typer_existential_found (loc, type_) ->
    let message = "Existential found" in
    let content =
      `Assoc
        [ "message", `String message
        ; "type", Type.to_yojson type_
        ; "location", Location.to_yojson loc
        ]
    in
    json_error ~stage ~content
  | `Typer_record_mismatch (loc, record, type_) ->
    let message = "Record mismatch" in
    let content =
      `Assoc
        [ "message", `String message
        ; "record", Ast_core.expression_to_yojson record
        ; "type", Type.to_yojson type_
        ; "location", Location.to_yojson loc
        ]
    in
    json_error ~stage ~content
  | `Typer_ill_formed_type (loc, type_) ->
    let message = "Ill formed type" in
    let content =
      `Assoc
        [ "message", `String message
        ; "type", Type.to_yojson type_
        ; "location", Location.to_yojson loc
        ]
    in
    json_error ~stage ~content
  | `Typer_cannot_subtype (loc, type1, type2) ->
    let message = "Cannot subtype" in
    let content =
      `Assoc
        [ "message", `String message
        ; "type1", Type.to_yojson type1
        ; "type2", Type.to_yojson type2
        ; "location", Location.to_yojson loc
        ]
    in
    json_error ~stage ~content
  | `Typer_corner_case desc ->
    let message = `String desc in
    let content = `Assoc [ "message", message ] in
    json_error ~stage ~content
  | `Typer_occurs_check_failed (loc, evar, type_) ->
    let message = "Occurs check failed" in
    let content =
      `Assoc
        [ "message", `String message
        ; "evar", Type_var.to_yojson evar
        ; "type", Type.to_yojson type_
        ; "location", Location.to_yojson loc
        ]
    in
    json_error ~stage ~content
  | `Typer_pattern_missing_cases (loc, syntax, ps) ->
    let message = `String "pattern-matching is not exhaustive." in
    let patterns =
      List.map ps ~f:(fun p ->
          let p = Untyper.untype_pattern p in
          `String (Desugaring.Decompiler.decompile_pattern_to_string ~syntax p))
    in
    let content =
      `Assoc
        [ "message", message
        ; "patterns", `List patterns
        ; "location", Location.to_yojson loc
        ]
    in
    json_error ~stage ~content
  | `Typer_pattern_redundant_case loc ->
    let message = `String "redundant case in pattern-matching" in
    let content =
      `Assoc [ "message", message; "location", Location.to_yojson loc ]
    in
    json_error ~stage ~content *)
(* | `Typer_cannot_unify (loc, type1, type2) ->
    let message = "Cannot unify" in
    let content =
      `Assoc
        [ "message", `String message
        ; "type1", Ast_typed.type_expression_to_yojson type1
        ; "type2", Ast_typed.type_expression_to_yojson type2
        ; "location", Location.to_yojson loc
        ]
    in
    json_error ~stage ~content
  | `Typer_cannot_unify_diff_layout (loc, type1, type2, layout1, layout2) ->
    let message = "Cannot unify" in
    let content =
      `Assoc
        [ "message", `String message
        ; "type1", Ast_typed.type_expression_to_yojson type1
        ; "type2", Ast_typed.type_expression_to_yojson type2
        ; "layout1", Layout.to_yojson layout1
        ; "layout2", Layout.to_yojson layout2
        ; "location", Location.to_yojson loc
        ]
    in
    json_error ~stage ~content
  | `Typer_occurs_check_failed (loc, evar, type_) ->
    let message = "Occurs check failed" in
    let content =
      `Assoc
        [ "message", `String message
        ; "evar", Exists_var.yojson_of_t evar
        ; "type", Ast_typed.type_expression_to_yojson type_
        ; "location", Location.to_yojson loc
        ]
    in
    json_error ~stage ~content
  | `Typer_wrong_type_for_unit_pattern (l, t) ->
    let message = "Variant pattern argument is unit" in
    let content =
      `Assoc
        [ "message", `String message
        ; "expected", Ast_typed.type_expression_to_yojson t
        ; "location", Location.to_yojson l
        ]
    in
    json_error ~stage ~content
  | `Typer_pattern_do_not_match loc ->
    let message = Format.asprintf "Pattern do not match returned expression" in
    let content =
      `Assoc [ "message", `String message; "location", Location.to_yojson loc ]
    in
    json_error ~stage ~content
  | `Typer_missing_funarg_annotation v ->
    let message = Format.asprintf "Missing type annotation for argument" in
    let content =
      `Assoc
        [ "value", Value_var.to_yojson v
        ; "message", `String message
        ; "location", Location.to_yojson @@ Value_var.get_location v
        ]
    in
    json_error ~stage ~content
  | `Typer_unbound_module_variable (mv, loc) ->
    let message = `String "unbound module" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let value = Format.asprintf "%a" Module_var.pp mv in
    let content =
      `Assoc
        [ "message", message; "location", `String loc; "value", `String value ]
    in
    json_error ~stage ~content
  | `Typer_unbound_type_variable (tv, loc) ->
    let message = `String "unbound type variable" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let value = Format.asprintf "%a" Type_var.pp tv in
    let content =
      `Assoc
        [ "message", message; "location", `String loc; "value", `String value ]
    in
    json_error ~stage ~content
  | `Typer_unbound_exists_variable (loc, evar) ->
    let message = `String "unbound existential variable" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let value = Format.asprintf "%a" Exists_var.pp evar in
    let content =
      `Assoc
        [ "message", message; "location", `String loc; "value", `String value ]
    in
    json_error ~stage ~content
  | `Typer_unbound_mut_variable (v, loc) ->
    let message = `String "unbound mut variable" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let value = Format.asprintf "%a" Value_var.pp v in
    let content =
      `Assoc
        [ "message", message; "location", `String loc; "value", `String value ]
    in
    json_error ~stage ~content
  | `Typer_unbound_variable (v, loc) ->
    let message = `String "unbound variable" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let value = Format.asprintf "%a" Value_var.pp v in
    let content =
      `Assoc
        [ "message", message; "location", `String loc; "value", `String value ]
    in
    json_error ~stage ~content
  | `Typer_match_missing_case (m, v, loc) ->
    let missing =
      List.fold_left
        ~f:(fun all o ->
          match List.find ~f:(fun f -> Label.compare f o = 0) v with
          | Some _ -> all
          | None ->
            let (Label o) = o in
            `String o :: all)
        ~init:[]
        m
    in
    let message = `String "Missing match case" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content =
      `Assoc [ "message", message; "location", loc; "value", `List missing ]
    in
    json_error ~stage ~content
  | `Typer_match_extra_case (v, m, loc) ->
    let open Ast_core in
    let rec extra processed redundant unknown = function
      | Label.Label l :: remaining ->
        (match List.find ~f:(fun f -> Label.compare (Label l) f = 0) m with
        | Some _ ->
          (match List.find ~f:(fun f -> String.equal f l) processed with
          | Some _ -> extra processed (`String l :: redundant) unknown remaining
          | None -> extra (l :: processed) redundant unknown remaining)
        | None -> extra processed redundant (`String l :: unknown) remaining)
      | [] -> List.rev redundant, List.rev unknown
    in
    let redundant, unknown = extra [] [] [] v in
    let message = `String "Redundant case in match cases" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content =
      `Assoc
        [ "message", message
        ; "location", loc
        ; "redundant", `List redundant
        ; "unknown", `List unknown
        ]
    in
    json_error ~stage ~content
  | `Typer_unbound_constructor (c, loc) ->
    let message = `String "unbound type variable" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let value = Format.asprintf "%a" Label.pp c in
    let content =
      `Assoc
        [ "message", message; "location", `String loc; "value", `String value ]
    in
    json_error ~stage ~content
  | `Typer_type_app_wrong_arity (op, e, a, loc) ->
    let message = `String "Wrong arity in type application" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let op = Option.value_map ~default:`Null ~f:Type_var.to_yojson op in
    let content =
      `Assoc
        [ "message", message
        ; "location", `String loc
        ; "type_constant", op
        ; "expected", `Int e
        ; "actuel", `Int a
        ]
    in
    json_error ~stage ~content
  | `Typer_bad_constructor (loc, label, type_) ->
    let message = `String "constructor not in expected type" in
    let content =
      `Assoc
        [ "message", message
        ; "location", Location.to_yojson loc
        ; ( "constructor"
          , let (Label s) = label in
            `String s )
        ; "type", Ast_typed.type_expression_to_yojson type_
        ]
    in
    json_error ~stage ~content
  | `Typer_michelson_or_no_annotation (c, loc) ->
    let message = `String "michelson_or must be annotated with a sum type" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let value = Format.asprintf "%a" Label.pp c in
    let content =
      `Assoc
        [ "message", message; "location", `String loc; "value", `String value ]
    in
    json_error ~stage ~content
  | `Typer_constant_declaration_tracer (loc, name, ae, Some t, err) ->
    let message = `String "Typing constant declaration" in
    let value = `String (Format.asprintf "%a" Ast_core.PP.expression ae) in
    let loc = Location.to_yojson loc in
    let name = `String (Format.asprintf "%a" Value_var.pp name) in
    let expected =
      `String (Format.asprintf "%a" Ast_typed.PP.type_expression t)
    in
    let content =
      `Assoc
        [ "message", message
        ; "location", loc
        ; "name", name
        ; "value", value
        ; "expected", expected
        ; "children", error_jsonformat err
        ]
    in
    json_error ~stage ~content
  | `Typer_constant_declaration_tracer (loc, name, _, None, err) ->
    let message = `String "Typing constant declaration" in
    let loc = Location.to_yojson loc in
    let name = `String (Format.asprintf "%a" Value_var.pp name) in
    let content =
      `Assoc
        [ "message", message
        ; "location", loc
        ; "name", name
        ; "children", error_jsonformat err
        ]
    in
    json_error ~stage ~content
  | `Typer_match_error (expected, actual, loc) ->
    let message = `String "matching over an expression of the wrong type" in
    let loc = Location.to_yojson loc in
    let expected = Ast_typed.type_expression_to_yojson expected in
    let actual = Ast_typed.type_expression_to_yojson actual in
    let content =
      `Assoc
        [ "message", message
        ; "location", loc
        ; "actual", actual
        ; "expected", expected
        ]
    in
    json_error ~stage ~content
  | `Typer_should_be_a_function_type (actual, e) ->
    let message = `String "expected a function type" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let expression = `String (Format.asprintf "%a" Ast_core.PP.expression e) in
    let actual =
      `String (Format.asprintf "%a" Ast_typed.PP.type_expression actual)
    in
    let content =
      `Assoc
        [ "message", message
        ; "location", loc
        ; "expression", expression
        ; "actual", actual
        ]
    in
    json_error ~stage ~content
  | `Typer_bad_record_access (field, loc) ->
    let message = `String "invalid record field" in
    let field = Label.to_yojson field in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content =
      `Assoc [ "message", message; "location", loc; "field", field ]
    in
    json_error ~stage ~content
  | `Typer_expression_tracer (e, err) ->
    let expression = `String (Format.asprintf "%a" Ast_core.PP.expression e) in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let content =
      `Assoc
        [ "location", loc
        ; "expression", expression
        ; "children", error_jsonformat err
        ]
    in
    json_error ~stage ~content
  | `Typer_assert_equal (loc, expected, actual) ->
    let message = `String "bad types" in
    let expected =
      Ast_typed.type_expression_to_yojson (type_improve expected)
    in
    let actual = Ast_typed.type_expression_to_yojson (type_improve actual) in
    let content =
      `Assoc
        [ "location", Location.to_yojson loc
        ; "message", message
        ; "expected", expected
        ; "actual", actual
        ]
    in
    json_error ~stage ~content
  | `Typer_corner_case desc ->
    let message = `String desc in
    let content = `Assoc [ "message", message ] in
    json_error ~stage ~content
  | `Typer_bad_collect_loop (t, loc) ->
    let message = `String "Loops over collections expect lists, sets or maps" in
    let actual =
      `String (Format.asprintf "%a" Ast_typed.PP.type_expression t)
    in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content =
      `Assoc [ "message", message; "location", loc; "actual", actual ]
    in
    json_error ~stage ~content
  | `Typer_expected_record (loc, t) ->
    let message = `String "expected a record" in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content =
      `Assoc
        [ "location", Location.to_yojson loc
        ; "message", message
        ; "value", value
        ]
    in
    json_error ~stage ~content
  | `Typer_expected_variant (loc, t) ->
    let message = `String "expected a record" in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content =
      `Assoc
        [ "message", message
        ; "location", Location.to_yojson loc
        ; "value", value
        ]
    in
    json_error ~stage ~content
  | `Typer_wrong_param_number (loc, name, expected, actual) ->
    let message = `String "constant with a wrong number of parameter" in
    let value = `String name in
    let expected = `Int expected in
    let actual = `Int (List.length actual) in
    let content =
      `Assoc
        [ "message", message
        ; "location", Location.to_yojson loc
        ; "value", value
        ; "actual", actual
        ; "expected", expected
        ]
    in
    json_error ~stage ~content
  | `Typer_expected_map (loc, e) ->
    let message = `String "expected a map" in
    let content =
      `Assoc
        [ "message", message
        ; "location", Location.to_yojson loc
        ; "value", Ast_typed.type_expression_to_yojson e
        ]
    in
    json_error ~stage ~content
  | `Typer_expected_option (loc, e) ->
    let message = `String "expected an option" in
    let content =
      `Assoc
        [ "message", message
        ; "location", Location.to_yojson loc
        ; "value", Ast_typed.type_expression_to_yojson e
        ]
    in
    json_error ~stage ~content
  | `Typer_not_matching (loc, t1, t2) ->
    let message = `String "types not matching" in
    let content =
      `Assoc
        [ "message", message
        ; "location", Location.to_yojson loc
        ; "value_1", Ast_typed.type_expression_to_yojson t1
        ; "value_2", Ast_typed.type_expression_to_yojson t2
        ]
    in
    json_error ~stage ~content
  | `Typer_not_annotated _ ->
    let message = `String "not annotated" in
    let content = `Assoc [ "message", message ] in
    json_error ~stage ~content
  | `Typer_typeclass_error (loc, exps, acts) ->
    let open Simple_utils.PP_helpers in
    let printl printer ppf args =
      Format.fprintf ppf "%a" (list_sep printer (const " , ")) args
    in
    let message = `String "typeclass error" in
    let expected =
      `String
        (Format.asprintf
           "%a"
           (list_sep (printl Ast_typed.PP.type_expression) (const " or "))
           exps)
    in
    let actual =
      `String
        (Format.asprintf
           "%a"
           (list_sep Ast_typed.PP.type_expression (const " or "))
           acts)
    in
    let content =
      `Assoc
        [ "message", message
        ; "location", Location.to_yojson loc
        ; "expected", expected
        ; "actual", actual
        ]
    in
    json_error ~stage ~content
  | `Typer_expected (loc, exps, acts) ->
    let open Simple_utils.PP_helpers in
    let message = `String "expected type" in
    let expected =
      `String
        (Format.asprintf
           "%a"
           (list_sep Ast_typed.PP.type_expression (const ", "))
           exps)
    in
    let actual =
      `String
        (Format.asprintf
           "%a"
           (list_sep Ast_typed.PP.type_expression (const ", "))
           acts)
    in
    let content =
      `Assoc
        [ "message", message
        ; "location", Location.to_yojson loc
        ; "expected", expected
        ; "actual", actual
        ]
    in
    json_error ~stage ~content
  | `Typer_uncomparable_types (loc, a, b) ->
    let message = `String "those two types are not comparable" in
    let t1 = `String (Format.asprintf "%a" Ast_typed.PP.type_expression a) in
    let t2 = `String (Format.asprintf "%a" Ast_typed.PP.type_expression b) in
    let content =
      `Assoc
        [ "message", message
        ; "location", Location.to_yojson loc
        ; "type_1", t1
        ; "type_2", t2
        ]
    in
    json_error ~stage ~content
  | `Typer_comparator_composed (loc, _a) ->
    let message =
      `String
        "Only composed types of not more than two element are allowed to be \
         compared"
    in
    let content =
      `Assoc [ "message", message; "location", Location.to_yojson loc ]
    in
    json_error ~stage ~content
  | `Typer_pattern_do_not_conform_type (p, t) ->
    let message = `String "pattern not of the expected type" in
    let pattern =
      (Ast_core.Pattern.to_yojson Ast_core.type_expression_option_to_yojson) p
    in
    let t = Ast_typed.type_expression_to_yojson t in
    let content =
      `Assoc
        [ "message", message
        ; "type", t
        ; "pattern", pattern
        ; "location", Location.to_yojson p.location
        ]
    in
    json_error ~stage ~content
  
  
  | `Typer_constant_since_protocol (loc, constant, protocol) ->
    let protocol_name = Environment.Protocols.variant_to_string protocol in
    let message =
      `String
        (Format.asprintf
           "%s is supported in protocol %s onwards. Hint: pass the compiler \
            option `--protocol %s`"
           constant
           (String.capitalize protocol_name)
           protocol_name)
    in
    let content =
      `Assoc [ "message", message; "location", Location.to_yojson loc ]
    in
    json_error ~stage ~content
  | `Typer_mismatching_for_each_collection_type (loc, collection_type, type_) ->
    let message = "Mismatching for-each collection type" in
    let content =
      `Assoc
        [ "message", `String message
        ; "location", Location.to_yojson loc
        ; ( "collection_type"
          , For_each_loop.collect_type_to_yojson collection_type )
        ; "type", Ast_typed.type_expression_to_yojson type_
        ]
    in
    json_error ~stage ~content
  | `Typer_mismatching_for_each_binder_arity (loc, collection_type, arity) ->
    let message = "Mismatching for-each collection type" in
    let content =
      `Assoc
        [ "message", `String message
        ; "location", Location.to_yojson loc
        ; ( "collection_type"
          , For_each_loop.collect_type_to_yojson collection_type )
        ; "type", `Int arity
        ]
    in
    json_error ~stage ~content
  | `Typer_mut_is_polymorphic (loc, type_) ->
    let message = "Mutable binding is polymorphic" in
    let content =
      `Assoc
        [ "message", `String message
        ; "location", Location.to_yojson loc
        ; "type", Ast_typed.type_expression_to_yojson type_
        ]
    in
    json_error ~stage ~content *)
