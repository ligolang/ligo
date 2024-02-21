module Location = Simple_utils.Location
open PPrint
open Ligo_prim
open Simple_utils.Function

let strip_empty = List.filter ~f:(Caml.( != ) empty)
let unwords : document list -> document = separate space <@ strip_empty
let unlines : document list -> document = separate hardline <@ strip_empty

(* Just a placeholder for value declarations rhs *)
let dots = !^"\"...\""

let decompile_type (ty_expr : Ast_typed.ty_expr) : document =
  let core_type = Checking.untype_type_expression ~use_orig_var:true ty_expr in
  let unified_type =
    match
      Simple_utils.Trace.to_stdlib_result
      @@ Nanopasses.decompile_ty_expr ~syntax:JsLIGO core_type
    with
    | Ok (unified_type, _) -> unified_type
    | Error _ ->
      let open Ast_unified in
      t_var
        ~loc:Location.generated
        (Ty_variable.of_input_var ~loc:Location.generated "unresolved")
  in
  let open Unification.Jsligo in
  let open Parsing.Jsligo in
  let cst = decompile_ty_expr unified_type in
  Pretty.print_type_expr Pretty.default_state cst


let attach_doc (doc_opt : document option) (value : document) : document =
  match doc_opt with
  | Some doc -> hardline ^^ doc ^^ hardline ^^ value
  | None -> value


let export (public : bool) = if public then !^"export" else empty

let binder_doc
    (public : bool)
    (doc_opt : document option)
    ({ var; ascr } : Ast_typed.ty_expr Binder.t)
    : document
  =
  if Value_var.is_generated var
  then empty
  else (
    let name = Value_var.to_name_exn var in
    let typ = decompile_type ascr in
    attach_doc doc_opt
    @@ unwords [ export public; !^"const"; !^name; colon; typ; equals; dots ])


let type_expr_doc
    (public : bool)
    (doc_opt : document option)
    (name : Type_var.t)
    (ty_expr : Ast_typed.ty_expr)
    : document
  =
  if Type_var.is_generated name
  then empty
  else (
    (* If type has parameters then we want to show it like [t<a, b>] *)
    let rec get_params : Ast_typed.type_content -> Ast_typed.type_variable list = function
      | T_abstraction { ty_binder; type_; _ } ->
        ty_binder :: get_params type_.type_content
      | _ -> []
    in
    let params =
      match get_params ty_expr.type_content with
      | [] -> empty
      | params ->
        angles (separate comma @@ List.map params ~f:(string <@ Type_var.to_name_exn))
    in
    let name = Type_var.to_name_exn name in
    (* We want to avoid cases like "type t = t", so, lets remove its original name *)
    let typ = decompile_type { ty_expr with orig_var = None } in
    attach_doc doc_opt @@ unwords [ export public; !^"type"; !^name; params; equals; typ ])


let unmangle (str : string) : string option =
  let open Option.Let_syntax in
  let%bind str = String.chop_prefix str ~prefix:"Mangled_module_" in
  let open Str in
  str
  |> global_replace (regexp_string "_p_") "."
  |> global_replace (regexp_string "_c_") ":"
  |> global_replace (regexp_string "_b_") "\\"
  |> global_replace (regexp_string "_s_") "/"
  |> global_replace (regexp_string "_a_") "@"
  |> global_replace (regexp_string "_d_") "-"
  |> global_replace (regexp_string "_l_") "("
  |> global_replace (regexp_string "_r_") ")"
  |> global_replace (regexp_string "_u_") "_"
  |> return


let to_typescript_path (path : string) : string option =
  let open Option.Let_syntax in
  let absolute_path = FilePath.make_absolute (Caml.Sys.getcwd ()) path in
  let%bind () = Option.some_if (Caml.Sys.file_exists absolute_path) () in
  let base_name, ext_opt = Filename.split_extension absolute_path in
  match%bind Syntax.of_ext_opt ext_opt with
  | JsLIGO -> return base_name
  | CameLIGO -> None


let comments_to_doc : string list -> document option = function
  | [] -> None
  | comments ->
    Option.some @@ unlines @@ List.map comments ~f:(enclose !^"/*" !^"*/" <@ string)


let rec decl_to_typescript (decl : Ast_typed.decl) : document =
  match decl.wrap_content with
  | D_value { binder; attr = { public; hidden; leading_comments; _ }; _ } when not hidden
    -> binder_doc public (comments_to_doc leading_comments) binder
  | D_irrefutable_match { pattern; attr = { public; hidden; leading_comments; _ }; _ }
    when not hidden ->
    (* Let's extract all binders and create a declaration for each of them. *)
    let binders = Linear_pattern.binders pattern in
    let doc = comments_to_doc leading_comments in
    unlines @@ List.map binders ~f:(binder_doc public doc)
  | D_type
      { type_binder
      ; type_expr
      ; type_attr = { public; hidden; leading_comments; deprecated }
      }
    when not hidden ->
    type_expr_doc public (comments_to_doc leading_comments) type_binder type_expr
  | D_module
      { module_binder
      ; module_ = { module_content; _ }
      ; module_attr = { public; hidden; leading_comments; deprecated }
      ; _
      }
    when (not hidden) && not (Module_var.is_generated module_binder) ->
    let name = Module_var.to_name_exn module_binder in
    let doc = comments_to_doc leading_comments in
    let import (var, vars) =
      let import_line mod_path =
        unwords [ export public; !^"import"; !^name; equals; mod_path ]
      in
      match unmangle @@ Module_var.to_name_exn var, vars with
      | Some rel_path, [] ->
        Option.value
          ~default:empty
          (let open Option.Let_syntax in
          let%bind path = to_typescript_path rel_path in
          let import_from =
            unwords [ !^"import * as"; !^name; !^"from"; squotes !^path ]
          in
          return @@ unlines [ import_from; import_line !^name ])
      | _ ->
        let mod_path =
          separate dot @@ List.map (var :: vars) ~f:(string <@ Module_var.to_name_exn)
        in
        attach_doc doc @@ import_line mod_path
    in
    (match module_content with
    | M_struct decls ->
      let content = unlines @@ List.map decls ~f:decl_to_typescript in
      attach_doc doc @@ unwords [ export public; !^"namespace"; !^name; braces content ]
    | M_variable var -> import (var, [])
    | M_module_path vars -> import vars)
  | D_module_include _ -> (* This is impossible in JsLIGO *) empty
  | D_signature
      { signature_binder; signature; signature_attr = { public; leading_comments } } ->
    (* Signatures are translated into namespaces because
      interfaces in TypeScript are declaring only object fields.
      We can't declare types there. *)
    let rec process_signature
        ?(public = true)
        ?(doc : document option)
        (binder : Module_var.t)
        (sig_ : Ast_typed.signature)
      =
      if Module_var.is_generated binder
      then empty
      else (
        (* $ prefix here is needed for distinguishing JsLIGO's interfaces and namespaces *)
        let name = "$" ^ Module_var.to_name_exn binder in
        let sig_item (sig_item : Ast_typed.sig_item) : document =
          match Location.unwrap sig_item with
          | S_value (var, ascr, Sig_item_attr.{ leading_comments; _ }) ->
            binder_doc true (comments_to_doc leading_comments) @@ Binder.make var ascr
          | S_type (name, ty_expr, { leading_comments }) ->
            type_expr_doc true (comments_to_doc leading_comments) name ty_expr
          | S_type_var (name, { leading_comments }) ->
            (* Rhs part of type should be always present in TypeScript's types declaration *)
            let rhs_type =
              let open Ast_typed in
              t_variable
                ~loc:Location.generated
                (Type_var.of_input_var ~loc:Location.generated "$type_var")
                ()
            in
            type_expr_doc true (comments_to_doc leading_comments) name rhs_type
          | S_module (binder, sig_) | S_module_type (binder, sig_) ->
            process_signature binder sig_
        in
        let items = unlines @@ List.map sig_.sig_items ~f:sig_item in
        attach_doc doc @@ unwords [ export public; !^"namespace"; !^name; braces items ])
    in
    process_signature
      ~public
      ?doc:(comments_to_doc leading_comments)
      signature_binder
      signature
  | _ -> empty


(* Let's add "//@ts-nocheck" pragma at the beginning of file
  to suppress the TypeScript's typer. Thus will allow us to generate docs
  even if the program is malformed from types perspective. *)
let to_typescript (prg : Ast_typed.program) : PPrint.document =
  unlines @@ (!^"//@ts-nocheck" :: List.map prg.pr_module ~f:decl_to_typescript)


let to_typescript ~display_format ~no_colour f prg =
  (* CLI formatters require these 2 arguments. Let's ignore them. *)
  ignore (display_format, no_colour);
  let pp = to_typescript prg in
  ToFormatter.compact f pp
