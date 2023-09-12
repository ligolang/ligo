open Simple_utils
open Ligo_prim
open Display

type all =
  [ `Self_ast_aggregated_warning_unused of Location.t * string
  | `Self_ast_aggregated_warning_muchused of Location.t * string
  | `Self_ast_aggregated_warning_unused_rec of Location.t * string
  | `Self_ast_aggregated_metadata_invalid_type of Location.t * string
  | `Checking_ambiguous_constructor_expr of
    Ast_core.expression * Type_var.t * Type_var.t * Location.t
  | `Checking_ambiguous_constructor_pat of
    Ast_core.type_expression option Ast_core.Pattern.t
    * Type_var.t
    * Type_var.t
    * Location.t
  | `Nanopasses_attribute_ignored of Location.t
  | `Nanopasses_infinite_for_loop of Location.t
  | `Self_ast_imperative_warning_deprecated_polymorphic_variable of
    Location.t * Type_var.t
  | `Main_view_ignored of Location.t
  | `Main_entry_ignored of Location.t
  | `Michelson_typecheck_failed_with_different_protocol of
    Environment.Protocols.t * Tezos_error_monad.Error_monad.error list
  | `Jsligo_deprecated_failwith_no_return of Location.t
  | `Jsligo_deprecated_toplevel_let of Location.t
  | `Jsligo_unreachable_code of Location.t
  | `Use_meta_ligo of Location.t
  | `Self_ast_aggregated_warning_bad_self_type of
    Ast_aggregated.type_expression * Ast_aggregated.type_expression * Location.t
  | `Metadata_cannot_parse of Location.t
  | `Metadata_no_empty_key of Location.t
  | `Metadata_tezos_storage_not_found of Location.t * string
  | `Metadata_not_valid_URI of Location.t * string
  | `Metadata_slash_not_valid_URI of Location.t * string
  | `Metadata_invalid_JSON of Location.t * string
  | `Metadata_error_JSON_object of Location.t * string
  | `Metadata_hash_fails of Location.t * string * string
  | `Metadata_json_download of Location.t * string
  | `Metadata_error_download of Location.t * string
  ]

let warn_bad_self_type t1 t2 loc = `Self_ast_aggregated_warning_bad_self_type (t1, t2, loc)

let pp
    :  display_format:string display_format -> no_colour:bool -> Format.formatter -> all
    -> unit
  =
 fun ~display_format ~no_colour f a ->
  let snippet_pp = Snippet.pp ~no_colour in
  match display_format with
  | Human_readable | Dev ->
    (match a with
    | `Use_meta_ligo loc ->
      Format.fprintf
        f
        "@[<hv>%a@ You are using Michelson failwith primitive (loaded from standard \
         library).@.Consider using `Test.failwith` for throwing a testing framework \
         failure.@.@]"
        snippet_pp
        loc
    | `Michelson_typecheck_failed_with_different_protocol (user_proto, errs) ->
      let open Environment.Protocols in
      Format.fprintf
        f
        "@[<hv>Warning: Error(s) occurred while type checking the produced michelson \
         contract:@.%a@.Note: You compiled your contract with protocol %s although we \
         internally use protocol %s to typecheck the produced Michelson contract@.so you \
         might want to ignore this error if related to a breaking change in protocol \
         %s@.@]"
        (Memory_proto_alpha.Client.Michelson_v1_error_reporter.report_errors
           ~details:true
           ~show_source:true
           ?parsed:None)
        errs
        (variant_to_string user_proto)
        (variant_to_string in_use)
        (variant_to_string in_use)
    | `Checking_ambiguous_constructor_expr (expr, tv_chosen, tv_possible, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@ Warning: The type of \"%a\" is ambiguous: Inferred type is \"%a\" but \
         could be of type \"%a\".@ Hint: You might want to add a type annotation. @.@]"
        snippet_pp
        loc
        Ast_core.PP.expression
        expr
        Type_var.pp
        tv_chosen
        Type_var.pp
        tv_possible
    | `Checking_ambiguous_constructor_pat (pat, tv_chosen, tv_possible, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@ Warning: The type the pattern of \"%a\" is ambiguous: Inferred type \
         is \"%a\" but could be of type \"%a\".@ Hint: You might want to add a type \
         annotation. @.@]"
        snippet_pp
        loc
        Ast_core.(Pattern.pp PP.type_expression_option)
        pat
        Type_var.pp
        tv_chosen
        Type_var.pp
        tv_possible
    | `Main_view_ignored loc ->
      Format.fprintf
        f
        "@[<hv>%a@ Warning: This view will be ignored, command line option override [@ \
         view] annotation@.@]"
        snippet_pp
        loc
    | `Main_entry_ignored loc ->
      Format.fprintf
        f
        "@[<hv>%a@ Warning: This entry will be ignored, command line option override [@ \
         entry] annotation@.@]"
        snippet_pp
        loc
    | `Self_ast_aggregated_warning_unused (loc, s) ->
      Format.fprintf
        f
        "@[<hv>%a:@.Warning: unused variable \"%s\".@.Hint: replace it by \"_%s\" to \
         prevent this warning.\n\
         @]"
        snippet_pp
        loc
        s
        s
    | `Self_ast_aggregated_warning_muchused (loc, _s) ->
      Format.fprintf
        f
        "@[<hv>%a:@.Warning: variable cannot be used more than once.\n@]"
        snippet_pp
        loc
    | `Self_ast_aggregated_warning_unused_rec (loc, s) ->
      Format.fprintf
        f
        "@[<hv>%a:@.Warning: unused recursion .@.Hint: remove recursion from the \
         function \"%s\" to prevent this warning.\n\
         @]"
        snippet_pp
        loc
        s
    | `Self_ast_aggregated_metadata_invalid_type (loc, s) ->
      Format.fprintf
        f
        "@[<hv>%a:@.Warning: If the following metadata is meant to be TZIP-16 \
         compliant,@.then it should be a 'big_map' from 'string' to 'bytes'.@.Hint: The \
         corresponding type should be :@.@[  %s@]@.You can disable this warning with the \
         '--no-metadata-check' flag.@.@]"
        snippet_pp
        loc
        s
    | `Nanopasses_attribute_ignored loc ->
      Format.fprintf
        f
        "@[<hv>%a@ Warning: unsupported attribute, ignored.@.@]"
        snippet_pp
        loc
    | `Nanopasses_infinite_for_loop loc ->
      Format.fprintf
        f
        "@[<hv>%a@.Warning: A boolean conditional expression is expected.@.Otherwise \
         this leads to an infinte loop.@.@]"
        snippet_pp
        loc
    | `Self_ast_imperative_warning_deprecated_polymorphic_variable (loc, name) ->
      Format.fprintf
        f
        "@[<hv>%a@ Warning: %a is not recognize as a polymorphic variable anymore. If \
         you want to make a polymorphic function, please consult the online \
         documentation @.@]"
        snippet_pp
        loc
        Type_var.pp
        name
    | `Jsligo_deprecated_failwith_no_return loc ->
      Format.fprintf
        f
        "@[<hv>%a@.Deprecated `failwith` without `return`: `failwith` is just a \
         function.@.Please add an explicit `return` before `failwith` if you meant the \
         built-in `failwith`.@.For now, compilation proceeds adding such `return` \
         automatically.@.@]"
        snippet_pp
        loc
    | `Jsligo_deprecated_toplevel_let loc ->
      Format.fprintf
        f
        "@[<hv>%a@.Toplevel let declaration is silently changed to const declaration.@.@]"
        snippet_pp
        loc
    | `Jsligo_unreachable_code loc ->
      Format.fprintf f "@[<hv>%a@ Warning: Unreachable code. @]" snippet_pp loc
    | `Self_ast_aggregated_warning_bad_self_type (got, expected, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@ Warning: Tezos.self type annotation.@.Annotation \"%a\" was given, \
         but contract being compiled would expect \"%a\".@.Note that \"Tezos.self\" \
         refers to the current contract, so the parameters should be generally the same. \
         @]"
        snippet_pp
        loc
        Ast_aggregated.PP.type_expression
        got
        Ast_aggregated.PP.type_expression
        expected
    | `Metadata_cannot_parse loc ->
      Format.fprintf
        f
        "@[<hv>%a@.Warning: Cannot parse metadata big-map. @]"
        snippet_pp
        loc
    | `Metadata_no_empty_key loc ->
      Format.fprintf
        f
        "@[<hv>%a@.Warning: Empty key in metadata big-map is mandatory. @]"
        snippet_pp
        loc
    | `Metadata_tezos_storage_not_found (loc, key) ->
      Format.fprintf
        f
        "@[<hv>%a@.Warning: Could not find key %s in storage's metadata. @]"
        snippet_pp
        loc
        key
    | `Metadata_not_valid_URI (loc, uri) ->
      Format.fprintf
        f
        "@[<hv>%a@.Warning: Could not find a valid URI %s in storage's metadata empty \
         key. @]"
        snippet_pp
        loc
        uri
    | `Metadata_slash_not_valid_URI (loc, uri) ->
      Format.fprintf
        f
        "@[<hv>%a@.Warning: Slash ('/') not in a valid position in URI: \"%s\", use \
         instead \"%%2F\". @]"
        snippet_pp
        loc
        uri
    | `Metadata_invalid_JSON (loc, e) ->
      Format.fprintf
        f
        "@[<hv>%a@.Warning: Could not parse JSON in storage's metadata: \"%s\". @]"
        snippet_pp
        loc
        e
    | `Metadata_error_JSON_object (loc, e) ->
      Format.fprintf
        f
        "@[<hv>%a@.Warning: Error in JSON in storage's metadata: %s. @]"
        snippet_pp
        loc
        e
    | `Metadata_hash_fails (loc, computed, given) ->
      Format.fprintf
        f
        "@[<hv>%a@.Warning: Hash mismatch in metadata's JSON document: got %s, when \
         given %s. @]"
        snippet_pp
        loc
        computed
        given
    | `Metadata_json_download (loc, s) ->
      Format.fprintf
        f
        "@[<hv>%a@.Warning: Metadata in storage points to %s document.@ Hint: If you \
         want to allow download and check it, pass `--allow-json-download`. To prevent \
         this message from appearing, pass `--disallow-json-download`.@.@]"
        snippet_pp
        loc
        s
    | `Metadata_error_download (loc, s) ->
      Format.fprintf
        f
        "@[<hv>%a@.Warning: Could not download JSON in URL: %s.@.@]"
        snippet_pp
        loc
        s)


let to_warning : all -> Simple_utils.Warning.t =
 fun w ->
  let open Simple_utils.Warning in
  match w with
  | `Use_meta_ligo location ->
    let message =
      Format.asprintf
        "You are using Michelson failwith primitive (loaded from standard \
         library).@.Consider using `Test.failwith` for throwing a testing framework \
         failure.@."
    in
    let content = make_content ~message ~location () in
    make ~stage:"testing framework" ~content
  | `Michelson_typecheck_failed_with_different_protocol (user_proto, errs) ->
    let open Environment.Protocols in
    let message =
      Format.asprintf
        "@[<hv>Warning: Error(s) occurred while type checking the produced michelson \
         contract:@.%a@.Note: You compiled your contract with protocol %s although we \
         internally use protocol %s to typecheck the produced Michelson contract@.so you \
         might want to ignore this error if related to a breaking change in protocol \
         %s@.@]"
        (Memory_proto_alpha.Client.Michelson_v1_error_reporter.report_errors
           ~details:true
           ~show_source:true
           ?parsed:None)
        errs
        (variant_to_string user_proto)
        (variant_to_string in_use)
        (variant_to_string in_use)
    in
    let location = Location.dummy in
    let content = make_content ~message ~location () in
    make ~stage:"michelson typecheck" ~content
  | `Checking_ambiguous_constructor_expr (expr, tv_chosen, tv_possible, location) ->
    let message =
      Format.asprintf
        "Warning: The type of \"%a\" is ambiguous: Inferred type is \"%a\" but could be \
         of type \"%a\".@ Hint: You might want to add a type annotation. @."
        Ast_core.PP.expression
        expr
        Type_var.pp
        tv_chosen
        Type_var.pp
        tv_possible
    in
    let content = make_content ~message ~location () in
    make ~stage:"typer" ~content
  | `Checking_ambiguous_constructor_pat (pat, tv_chosen, tv_possible, location) ->
    let message =
      Format.asprintf
        "Warning: The type the pattern of \"%a\" is ambiguous: Inferred type is \"%a\" \
         but could be of type \"%a\".@ Hint: You might want to add a type annotation. @."
        Ast_core.(Pattern.pp PP.type_expression_option)
        pat
        Type_var.pp
        tv_chosen
        Type_var.pp
        tv_possible
    in
    let content = make_content ~message ~location () in
    make ~stage:"typer" ~content
  | `Main_view_ignored location ->
    let message =
      Format.sprintf
        "Warning: This view will be ignored, command line option override [@ view] \
         annotation@."
    in
    let content = make_content ~message ~location () in
    make ~stage:"view compilation" ~content
  | `Main_entry_ignored location ->
    let message =
      Format.sprintf
        "Warning: This entry will be ignored, command line option override [@ entry] \
         annotation@."
    in
    let content = make_content ~message ~location () in
    make ~stage:"view compilation" ~content
  | `Self_ast_aggregated_warning_unused (location, variable) ->
    let message =
      Format.sprintf
        "@.Warning: unused variable \"%s\".@.Hint: replace it by \"_%s\" to prevent this \
         warning.\n"
        variable
        variable
    in
    let content = make_content ~message ~location ~variable () in
    make ~stage:"parsing command line parameters" ~content
  | `Self_ast_aggregated_warning_muchused (location, _s) ->
    let message =
      Format.sprintf "@.Warning: variable cannot be used more than once.\n@]"
    in
    let content = make_content ~message ~location () in
    make ~stage:"typer" ~content
  | `Self_ast_aggregated_warning_unused_rec (location, s) ->
    let message =
      Format.sprintf
        "Warning: unused recursion .@.Hint: remove recursion from the function \"%s\" to \
         prevent this warning.\n"
        s
    in
    let content = make_content ~message ~location () in
    make ~stage:"parsing command line parameters" ~content
  | `Self_ast_aggregated_metadata_invalid_type (loc, s) ->
    let message =
      Format.sprintf
        "Warning: If the following metadata is meant to be TZIP-16 compliant,@.then it \
         should be a 'big_map' from 'string' to 'bytes'.@.Hint: The corresponding type \
         should be :@.@[  %s@]@.You can disable this warning with the \
         '--no-metadata-check' flag.\n"
        s
    in
    let content = make_content ~message ~location:loc () in
    make ~stage:"parsing command line parameters" ~content
  | `Nanopasses_attribute_ignored loc ->
    let message = "Warning: ignored attributes" in
    let content = make_content ~message ~location:loc () in
    make ~stage:"typer" ~content
  | `Nanopasses_infinite_for_loop loc ->
    let message =
      "Warning: A boolean conditional expression is expected.\n\
       Otherwise this leads to an infinte loop."
    in
    let content = make_content ~message ~location:loc () in
    make ~stage:"typer" ~content
  | `Self_ast_imperative_warning_deprecated_polymorphic_variable (location, variable) ->
    let variable = Format.asprintf "%a" Type_var.pp variable in
    let message =
      Format.sprintf
        "Warning: %s is not recognize as a polymorphic variable anymore. If you want to \
         make a polymorphic function, please consult the online documentation @."
        variable
    in
    let content = make_content ~message ~location ~variable () in
    make ~stage:"abstractor" ~content
  | `Jsligo_deprecated_failwith_no_return location ->
    let message =
      Format.sprintf
        "Deprecated `failwith` without `return`: `failwith` is just a function.@.Please \
         add an explicit `return` before `failwith` if you meant the built-in \
         `failwith`.@.For now, compilation proceeds adding such `return` automatically.@"
    in
    let content = make_content ~message ~location () in
    make ~stage:"abstractor" ~content
  | `Jsligo_deprecated_toplevel_let location ->
    let message =
      Format.sprintf "Toplevel let declaration is silently changed to const declaration."
    in
    let content = make_content ~message ~location () in
    make ~stage:"abstractor" ~content
  | `Jsligo_unreachable_code location ->
    let message = "Warning: Unreachable code." in
    let content = make_content ~message ~location () in
    make ~stage:"abstractor" ~content
  | `Self_ast_aggregated_warning_bad_self_type (got, expected, location) ->
    let message =
      Format.asprintf
        "Warning: Tezos.self type annotation.@.Annotation \"%a\" was given, but contract \
         being compiled would expect \"%a\".@.Note that \"Tezos.self\" refers to the \
         current contract, so the parameters should be generally the same."
        Ast_aggregated.PP.type_expression
        got
        Ast_aggregated.PP.type_expression
        expected
    in
    let content = make_content ~message ~location () in
    make ~stage:"aggregation" ~content
  | `Metadata_cannot_parse location ->
    let message = Format.sprintf "Cannot parse big-map metadata." in
    let content = make_content ~message ~location () in
    make ~stage:"metadata_check" ~content
  | `Metadata_no_empty_key location ->
    let message = Format.sprintf "Empty key in metadata big-map is mandatory." in
    let content = make_content ~message ~location () in
    make ~stage:"metadata_check" ~content
  | `Metadata_tezos_storage_not_found (location, key) ->
    let message = Format.sprintf "Could not find key %s in storage's metadata." key in
    let content = make_content ~message ~location () in
    make ~stage:"metadata_check" ~content
  | `Metadata_not_valid_URI (location, uri) ->
    let message =
      Format.sprintf "Could not find a valid URI %s in storage's metadata empty key." uri
    in
    let content = make_content ~message ~location () in
    make ~stage:"metadata_check" ~content
  | `Metadata_slash_not_valid_URI (location, uri) ->
    let message = Format.sprintf "Slash ('/') not in a valid position in URI: %s." uri in
    let content = make_content ~message ~location () in
    make ~stage:"metadata_check" ~content
  | `Metadata_invalid_JSON (location, e) ->
    let message = Format.sprintf "Could not parse JSON in storage's metadata: \"%s\"" e in
    let content = make_content ~message ~location () in
    make ~stage:"metadata_check" ~content
  | `Metadata_error_JSON_object (location, e) ->
    let message = Format.sprintf "Error in JSON in storage's metadata: %s" e in
    let content = make_content ~message ~location () in
    make ~stage:"metadata_check" ~content
  | `Metadata_hash_fails (location, computed, given) ->
    let message =
      Format.sprintf
        "Hash mismatch in metadata's JSON document: got %s, when given %s."
        computed
        given
    in
    let content = make_content ~message ~location () in
    make ~stage:"metadata_check" ~content
  | `Metadata_json_download (location, s) ->
    let message =
      Format.sprintf
        "Metadata in storage points to %s document. If you want to allow download and \
         check it, pass `--allow-json-download`. To prevent this message from appearing, \
         pass `--disallow-json-download`."
        s
    in
    let content = make_content ~message ~location () in
    make ~stage:"metadata_check" ~content
  | `Metadata_error_download (location, s) ->
    let message = Format.sprintf "Warning: Could not download JSON in URL: %s" s in
    let content = make_content ~message ~location () in
    make ~stage:"metadata_check" ~content


let to_json : all -> Yojson.Safe.t =
 fun w ->
  let warning = to_warning w in
  Simple_utils.Warning.to_yojson warning


let format = { pp; to_json }
