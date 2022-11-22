open Simple_utils
open Ligo_prim
open Display

type all =
[
  | `Self_ast_typed_warning_unused of Location.t * string
  | `Self_ast_typed_warning_muchused of Location.t * string
  | `Self_ast_typed_warning_unused_rec of Location.t * string
  | `Checking_ambiguous_constructor_expr of Ast_core.expression * Type_var.t * Type_var.t * Location.t
  | `Checking_ambiguous_constructor_pat of Ast_core.type_expression option Ast_core.Pattern.t * Type_var.t * Type_var.t * Location.t
  | `Self_ast_imperative_warning_layout of (Location.t * Label.t)
  | `Self_ast_imperative_warning_deprecated_polymorphic_variable of Location.t * Type_var.t
  | `Self_ast_imperative_warning_deprecated_constant of Location.t * Ast_imperative.expression * Ast_imperative.expression * Ast_imperative.type_expression
  | `Main_view_ignored of Location.t
  | `Michelson_typecheck_failed_with_different_protocol of (Environment.Protocols.t * Tezos_error_monad.Error_monad.error list)
  | `Jsligo_deprecated_failwith_no_return of Location.t
  | `Jsligo_deprecated_toplevel_let of Location.t
  | `Jsligo_unreachable_code of Location.t
  | `Use_meta_ligo of Location.t
  | `Self_ast_aggregated_warning_bad_self_type of Ast_aggregated.type_expression * Ast_aggregated.type_expression * Location.t
  | `Deprecated_reasonligo
]

let warn_layout loc lab = `Self_ast_imperative_warning_layout (loc,lab)
let warn_bad_self_type t1 t2 loc = `Self_ast_aggregated_warning_bad_self_type (t1, t2, loc)

let pp : display_format:string display_format ->
  Format.formatter -> all -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Use_meta_ligo loc ->
      Format.fprintf f "@[<hv>%a@ You are using Michelson failwith primitive (loaded from standard library).@.\
      Consider using `Test.failwith` for throwing a testing framework failure.@.@]"
        Snippet.pp loc
    | `Michelson_typecheck_failed_with_different_protocol (user_proto,errs) -> (
      let open Environment.Protocols in
      Format.fprintf f
        "@[<hv>Warning: Error(s) occurred while type checking the produced michelson contract:@.%a@.\
        Note: You compiled your contract with protocol %s although we internally use protocol %s to typecheck the produced Michelson contract@.\
        so you might want to ignore this error if related to a breaking change in protocol %s@.@]"
          (Memory_proto_alpha.Client.Michelson_v1_error_reporter.report_errors ~details:true ~show_source:true ?parsed:(None)) errs
          (variant_to_string user_proto)
          (variant_to_string in_use)
          (variant_to_string in_use)
    )
    | `Checking_ambiguous_constructor_expr (expr, tv_chosen,tv_possible,loc) ->
      Format.fprintf f "@[<hv>%a@ Warning: The type of \"%a\" is ambiguous: Inferred type is %a but could be of type %a.@ Hint: You might want to add a type annotation. @.@]"
      Snippet.pp loc
      Ast_core.PP.expression
      expr
      Type_var.pp tv_chosen
      Type_var.pp tv_possible
    | `Checking_ambiguous_constructor_pat (pat, tv_chosen,tv_possible,loc) ->
      Format.fprintf f "@[<hv>%a@ Warning: The type the pattern of \"%a\" is ambiguous: Inferred type is %a but could be of type %a.@ Hint: You might want to add a type annotation. @.@]"
      Snippet.pp loc
      Ast_core.(Pattern.pp PP.type_expression_option)
      pat
      Type_var.pp tv_chosen
      Type_var.pp tv_possible
    | `Main_view_ignored loc ->
      Format.fprintf f "@[<hv>%a@ Warning: This view will be ignored, command line option override [@ view] annotation@.@]"
      Snippet.pp loc
    | `Self_ast_typed_warning_unused (loc, s) ->
        Format.fprintf f
          "@[<hv>%a:@.Warning: unused variable \"%s\".@.Hint: replace it by \"_%s\" to prevent this warning.\n@]"
          Snippet.pp loc s s
    | `Self_ast_typed_warning_muchused (loc, s) ->
        Format.fprintf f
          "@[<hv>%a:@.Warning: variable \"%s\" cannot be used more than once.\n@]"
          Snippet.pp loc s
    | `Self_ast_typed_warning_unused_rec (loc, s) ->
      Format.fprintf f
        "@[<hv>%a:@.Warning: unused recursion .@.Hint: remove recursion from the function \"%s\" to prevent this warning.\n@]"
        Snippet.pp loc s
    | `Self_ast_imperative_warning_layout (loc,Label s) ->
        Format.fprintf f
          "@[<hv>%a@ Warning: layout attribute only applying to %s, probably ignored.@.@]"
          Snippet.pp loc s
    | `Self_ast_imperative_warning_deprecated_polymorphic_variable (loc, name) ->
        Format.fprintf f
          "@[<hv>%a@ Warning: %a is not recognize as a polymorphic variable anymore. If you want to make a polymorphic function, please consult the online documentation @.@]"
          Snippet.pp loc Type_var.pp name
    | `Self_ast_imperative_warning_deprecated_constant (l, curr, alt, ty) ->
       Format.fprintf f
         "@[<hv>%a@ Warning: the constant %a is soon to be deprecated. Use instead %a : %a. @]"
         Snippet.pp l
         Ast_imperative.PP.expression curr
         Ast_imperative.PP.expression alt
         Ast_imperative.PP.type_expression ty
    | `Jsligo_deprecated_failwith_no_return loc ->
      Format.fprintf f "@[<hv>%a@.Deprecated `failwith` without `return`: `failwith` is just a function.@.Please add an explicit `return` before `failwith` if you meant the built-in `failwith`.@.For now, compilation proceeds adding such `return` automatically.@.@]"
      Snippet.pp loc
    | `Jsligo_deprecated_toplevel_let loc ->
      Format.fprintf f "@[<hv>%a@.Toplevel let declaration are silently change to const declaration.@.@]"
      Snippet.pp loc
    | `Jsligo_unreachable_code loc ->
      Format.fprintf f "@[<hv>%a@ Warning: Unreachable code. @]"
      Snippet.pp loc
    | `Self_ast_aggregated_warning_bad_self_type (got,expected,loc) ->
      Format.fprintf f
        "@[<hv>%a@ Warning: Tezos.self type annotation.@.Annotation \"%a\" was given, but contract being compiled would expect \"%a\".@.Note that \"Tezos.self\" refers to the current contract, so the parameters should be generally the same. @]"
        Snippet.pp loc
        Ast_aggregated.PP.type_expression got
        Ast_aggregated.PP.type_expression expected
    | `Deprecated_reasonligo ->
      Format.fprintf f "@[Reasonligo is depreacted, support will be dropped in a few versions.@.@]"
    
  )
let to_warning : all -> Simple_utils.Warning.t = fun w ->
  let open Simple_utils.Warning in
  match w with
  | `Use_meta_ligo location ->
    let message = Format.asprintf"You are using Michelson failwith primitive (loaded from standard library).@.\
    Consider using `Test.failwith` for throwing a testing framework failure.@."
    in
    let content = make_content ~message ~location () in
    make ~stage:"testing framework" ~content
  | `Michelson_typecheck_failed_with_different_protocol (user_proto,errs) ->
    let open Environment.Protocols in
    let message = Format.asprintf
      "@[<hv>Warning: Error(s) occurred while type checking the produced michelson contract:@.%a@.\
      Note: You compiled your contract with protocol %s although we internally use protocol %s to typecheck the produced Michelson contract@.\
      so you might want to ignore this error if related to a breaking change in protocol %s@.@]"
        (Memory_proto_alpha.Client.Michelson_v1_error_reporter.report_errors ~details:true ~show_source:true ?parsed:(None)) errs
        (variant_to_string user_proto)
        (variant_to_string in_use)
        (variant_to_string in_use)
    in
    let location = Location.dummy in
    let content = make_content ~message ~location () in
    make ~stage:"michelson typecheck" ~content
  | `Checking_ambiguous_constructor_expr (expr, tv_chosen,tv_possible,location) ->
    let message = Format.asprintf "Warning: The type of \"%a\" is ambiguous: Inferred type is %a but could be of type %a.@ Hint: You might want to add a type annotation. @."
      Ast_core.PP.expression expr
      Type_var.pp tv_chosen
      Type_var.pp tv_possible
    in
    let content = make_content ~message ~location () in
    make ~stage:"typer" ~content
  | `Checking_ambiguous_constructor_pat (pat, tv_chosen,tv_possible,location) ->
    let message = Format.asprintf "Warning: The type the pattern of \"%a\" is ambiguous: Inferred type is %a but could be of type %a.@ Hint: You might want to add a type annotation. @."
      Ast_core.(Pattern.pp PP.type_expression_option) pat
      Type_var.pp tv_chosen
      Type_var.pp tv_possible
    in
    let content = make_content ~message ~location () in
    make ~stage:"typer" ~content
  | `Main_view_ignored location ->
    let message = Format.sprintf "Warning: This view will be ignored, command line option override [@ view] annotation@." in
    let content = make_content ~message ~location () in
    make ~stage:"view compilation" ~content
  | `Self_ast_typed_warning_unused (location, variable) ->
    let message = Format.sprintf
      "@.Warning: unused variable \"%s\".@.Hint: replace it by \"_%s\" to prevent this warning.\n"
      variable variable
    in
    let content = make_content ~message ~location ~variable () in
    make ~stage:"parsing command line parameters" ~content
  | `Self_ast_typed_warning_muchused (location, s) ->
    let message = Format.sprintf
        "@.Warning: variable \"%s\" cannot be used more than once.\n@]" s in
    let content = make_content ~message ~location () in
    make ~stage:"typer" ~content
  | `Self_ast_typed_warning_unused_rec (location, s) ->
    let message = Format.sprintf
      "Warning: unused recursion .@.Hint: remove recursion from the function \"%s\" to prevent this warning.\n"
      s 
    in
    let content = make_content ~message ~location () in
    make ~stage:"parsing command line parameters" ~content
  | `Self_ast_imperative_warning_layout (location,Label s) ->
      let message = Format.sprintf
        "Warning: layout attribute only applying to %s, probably ignored.@." s
    in
    let content = make_content ~message ~location () in
    make ~stage:"typer" ~content
  | `Self_ast_imperative_warning_deprecated_polymorphic_variable (location, variable) ->
    let variable = Format.asprintf "%a" Type_var.pp variable in  
    let message = Format.sprintf
      "Warning: %s is not recognize as a polymorphic variable anymore. If you want to make a polymorphic function, please consult the online documentation @." variable
    in
    let content = make_content ~message ~location ~variable () in
    make ~stage:"abstractor" ~content
  | `Self_ast_imperative_warning_deprecated_constant (location, curr, alt, ty) ->
      let message = Format.asprintf
        "Warning: the constant %a is soon to be deprecated. Use instead %a : %a."
        Ast_imperative.PP.expression curr
        Ast_imperative.PP.expression alt
        Ast_imperative.PP.type_expression ty in
      let content = make_content ~message ~location () in
    make ~stage:"abstractor" ~content
  | `Jsligo_deprecated_failwith_no_return location ->
    let message = Format.sprintf "Deprecated `failwith` without `return`: `failwith` is just a function.@.Please add an explicit `return` before `failwith` if you meant the built-in `failwith`.@.For now, compilation proceeds adding such `return` automatically.@" in
    let content = make_content ~message ~location () in
    make ~stage:"abstractor" ~content
  | `Jsligo_deprecated_toplevel_let location ->
    let message = Format.sprintf "Toplevel let declaration are silently change to const declaration.@" in
    let content = make_content ~message ~location () in
    make ~stage:"abstractor" ~content
  | `Jsligo_unreachable_code location ->
    let message = "Warning: Unreachable code." in
    let content = make_content ~message ~location () in
    make ~stage:"abstractor" ~content
  | `Self_ast_aggregated_warning_bad_self_type (got,expected,location) ->
    let message = Format.asprintf
      "Warning: Tezos.self type annotation.@.Annotation \"%a\" was given, but contract being compiled would expect \"%a\".@.Note that \"Tezos.self\" refers to the current contract, so the parameters should be generally the same."
      Ast_aggregated.PP.type_expression got
      Ast_aggregated.PP.type_expression expected in
    let content = make_content ~message ~location () in
    make ~stage:"aggregation" ~content
  | `Deprecated_reasonligo ->
    let message = Format.sprintf "Reasonligo is depreacted, support will be dropped in a few versions.@" in
    let location = Location.dummy in
    let content = make_content ~message ~location () in
    make ~stage:"cli parsing" ~content

let to_json : all -> Yojson.Safe.t = fun w ->
  let warning = to_warning w in
  Simple_utils.Warning.to_yojson warning

let format = {pp;to_json}