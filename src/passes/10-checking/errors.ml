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
        "@[<hv>%a@.Invalid type(s).@.Expected \"%a\", but got: \"%a\".@]"
        Snippet.pp
        loc
        Type.pp
        expected_type
        Type.pp
        lit_type)


let error_json : typer_error -> Simple_utils.Error.t =
 fun err ->
  let open Simple_utils.Error in
  match err with
  | `Typer_mut_var_captured (var, loc) ->
    let message =
      Format.asprintf
        "@[Invalid capture of mutable variable \"%a\"@]"
        Value_var.pp
        var
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_ill_formed_type (type_, loc) ->
    let message =
      Format.asprintf "@[Invalid type@.Ill formed type %a.@]" Type.pp type_
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_record_mismatch (_record, type_, loc) ->
    let message =
      Format.asprintf
        "@[Mismatching record labels. Expected record of type %a.@]"
        Type.pp
        type_
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_cannot_subtype (type1, type2, loc) ->
    let message =
      Format.asprintf
        "@[Expected %a, but received %a. Types are not compatitable.@]"
        Type.pp
        type2
        Type.pp
        type1
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_corner_case (desc, loc) ->
    let message =
      Format.asprintf "@[A type system corner case occurred:@.%s@]" desc
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_occurs_check_failed (tvar, type_, loc) ->
    let message =
      Format.asprintf
        "@[The type variable ^%a occurs inside %a.@]"
        Type_var.pp
        tvar
        Type.pp
        type_
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_pattern_missing_cases (syntax, ps, loc) ->
    let ps =
      List.fold ps ~init:"" ~f:(fun s p ->
          let s' =
            let p = Untyper.untype_pattern p in
            Desugaring.Decompiler.decompile_pattern_to_string ~syntax p
          in
          s ^ "- " ^ s' ^ "\n")
    in
    let message =
      Format.asprintf
        "@[Error : this pattern-matching is not exhaustive.@.Here are examples \
         of cases that are not matched:@.%s@]"
        ps
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_pattern_redundant_case loc ->
    let message = Format.asprintf "@[Error: this match case is unused.@]" in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_cannot_unify (type1, type2, loc) ->
    let message =
      Format.asprintf
        "@[Invalid type(s)@.Cannot unify %a with %a.@]"
        Type.pp
        type1
        Type.pp
        type2
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_cannot_unify_diff_layout (type1, type2, layout1, layout2, loc) ->
    let message =
      Format.asprintf
        "@[Invalid type(s)@.Cannot unify %a with %a due to differing layouts \
         (%a and %a).@]"
        Type.pp
        type1
        Type.pp
        type2
        Type.pp_layout
        layout1
        Type.pp_layout
        layout2
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_bad_constructor (label, type_, loc) ->
    let message =
      Format.asprintf
        "@[Expected constructor %a in expected sum type %a.]"
        Label.pp
        label
        Type.pp
        type_
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_pattern_do_not_match loc ->
    let message =
      Format.asprintf "@[Pattern do not match returned expression.@]"
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_unbound_module_variable (mv, loc) ->
    let message =
      Format.asprintf "@[Module \"%a\" not found. @]" Module_var.pp mv
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_unbound_type_variable (tv, loc) ->
    let message =
      Format.asprintf "@[Type \"%a\" not found. @]" Type_var.pp tv
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_unbound_texists_var (tvar, loc) ->
    let message =
      Format.asprintf
        "@[Existential variable \"^%a\" not found. @]"
        Type_var.pp
        tvar
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_unbound_variable (v, loc) ->
    let message =
      Format.asprintf "@[Variable \"%a\" not found. @]" Value_var.pp v
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_unbound_mut_variable (v, loc) ->
    let message =
      Format.asprintf "@[Mutable variable \"%a\" not found. @]" Value_var.pp v
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_mismatching_for_each_collection_type (collection_type, type_, loc) ->
    let message =
      Format.asprintf
        "@[Expected collection of type \"%a\", but recieved collection of type \
         %a.@]"
        For_each_loop.pp_collect_type
        collection_type
        Type.pp
        type_
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_mismatching_for_each_binder_arity
      (expected_arity, recieved_arity, loc) ->
    let message =
      Format.asprintf
        "@[Expected for each loop to bind %d variables, but loop binds %d \
         variables.@]"
        expected_arity
        recieved_arity
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_unbound_constructor (c, loc) ->
    let message =
      Format.asprintf "@[Constructor \"%a\" not found. @]" Label.pp c
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_type_app_wrong_arity (op_opt, e, a, loc) ->
    let aux : Format.formatter -> Type_var.t option -> unit =
     fun ppf operator_opt ->
      match operator_opt with
      | Some v -> Format.fprintf ppf " %a" Type_var.pp v
      | None -> ()
    in
    let message =
      Format.asprintf
        "@[Type %a is applied to a wrong number of arguments, expected: %i \
         got: %i@]"
        aux
        op_opt
        e
        a
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_michelson_or_no_annotation (c, loc) ->
    let message =
      Format.asprintf
        "@[Incorrect usage of type \"michelson_or\".@.The contructor \"%a\" \
         must be annotated with a variant type. @]"
        Label.pp
        c
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_should_be_a_function_type (lamb_type, _args, loc) ->
    let message =
      Format.asprintf
        "@[Invalid type.@.Expected a function type, but got \"%a\". @]"
        Type.pp
        (type_improve lamb_type)
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_bad_record_access (field, loc) ->
    let message =
      Format.asprintf
        "@[Invalid record field \"%a\" in record. @]"
        Label.pp
        field
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_not_annotated loc ->
    let message =
      Format.asprintf
        "@[Can't infer the type of this value, please add a type annotation.@]"
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_comparator_composed (_a, loc) ->
    let message =
      Format.asprintf
        "@[Invalid arguments.@.Only composed types of not more than two \
         element are allowed to be compared. @]"
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_assert_equal (expected, actual, loc) ->
    let message =
      Format.asprintf
        "@[Invalid type(s).@.Expected: \"%a\", but got: \"%a\". @]"
        Ast_typed.PP.type_expression_orig
        expected
        Ast_typed.PP.type_expression_orig
        actual
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_expected_record (type_, loc) ->
    let message =
      Format.asprintf
        "@[Invalid argument.@.Expected a record, but got an argument of type \
         \"%a\". @]"
        Type.pp
        (type_improve type_)
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_uncomparable_types (type1, type2, loc) ->
    let message =
      Format.asprintf
        "@[Invalid arguments.@.These types cannot be compared: \"%a\" and \
         \"%a\". @]"
        Type.pp
        (type_improve type1)
        Type.pp
        (type_improve type2)
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_pattern_do_not_conform_type (pat, type_, loc) ->
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
    let message =
      Format.asprintf
        "@[Pattern %anot of the expected type %a @]"
        pf
        pat
        Type.pp
        (type_improve type_)
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_mut_is_polymorphic (type_, loc) ->
    let message =
      Format.asprintf
        "@[Mutable binding has the polymorphic type %a@.Hint: Add an \
         annotation.@]"
        Type.pp
        type_
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_unbound_module (path, loc) ->
    let rec pp_path ppf path =
      match path with
      | [] -> failwith "Empty path"
      | [ mvar ] -> Format.fprintf ppf "%a" Module_var.pp mvar
      | mvar :: path ->
        Format.fprintf ppf "%a.%a" Module_var.pp mvar pp_path path
    in
    let message = Format.asprintf "@[Module \"%a\" not found.@]" pp_path path in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_cannot_decode_texists (type_, loc) ->
    let message =
      Format.asprintf
        "@[Underspecified type %a.@.Please add additional annotations.@]"
        Type.pp
        type_
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
  | `Typer_literal_type_mismatch (lit_type, expected_type, loc) ->
    let message =
      Format.asprintf
        "@[Invalid type(s).@.Expected \"%a\", but got: \"%a\".@]"
        Type.pp
        expected_type
        Type.pp
        lit_type
    in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
