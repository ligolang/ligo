(* Creates files like gitlab-pages/website/versioned_docs/version-1.2.0/reference/toplevel.md
   from LIGO source files *)

open PPrint
open Ligo_prim
open Simple_utils
open Simple_utils.Function

(** FIXME see
    https://tezos-dev.slack.com/archives/GQ635HR0U/p1708798904488879?thread_ts=1708795057.966149&cid=GQ635HR0U
    *)
(* let link ~text ~target = string @@ Format.sprintf {|<a href="%s">%s</a>|} target text *)

let link ~text ~target = !^"[" ^^ text ^^ !^"](" ^^ target ^^ !^")"

let unlines : document list -> document =
  separate (hardline ^^ hardline) <@ List.filter ~f:(Caml.( != ) empty)


(**  file_name is either "toplevel.md" or "A.B.md" where "A.B" is some module / module type *)
type 'a returned_file =
  { file_name : string
  ; contents : 'a
  }

type 'a returned_files' = 'a returned_file list
type returned_files = string returned_files'

type config =
  { source_syntax : Syntax_types.t
  ; source_file : string
  ; current_mod_path : string list (** Are we traversing some module now? *)
  ; additional_files : document returned_files' ref
        (** We collect files for modules / module types here *)
  }

let add_additional_file : config -> document returned_file -> unit =
 fun config file -> config.additional_files := !(config.additional_files) @ [ file ]


let with_mod_name : string -> config -> config =
 fun name config -> { config with current_mod_path = config.current_mod_path @ [ name ] }


let map_returned_files : f:('a -> 'b) -> 'a returned_file list -> 'b returned_file list =
 fun ~f ->
  List.map ~f:(fun { file_name; contents } -> { file_name; contents = f contents })


let syntax_title_ : Syntax_types.t -> document -> document =
 fun syntax x ->
  !^"<SyntaxTitle syntax=\""
  ^^ !^(Syntax.to_string syntax)
  ^^ !^"\">"
  ^^ hardline
  ^^ x
  ^^ hardline
  ^^ !^"</SyntaxTitle>"


let syntax_title (f : Syntax_types.t -> document) : document =
  syntax_title_ CameLIGO (f CameLIGO) ^^ hardline ^^ syntax_title_ JsLIGO (f JsLIGO)


let syntax_ : Syntax_types.t -> document -> document =
 fun syntax x ->
  !^"<Syntax syntax=\""
  ^^ !^(Syntax.to_string syntax)
  ^^ !^"\">"
  ^^ hardline
  ^^ hardline
  ^^ x
  ^^ hardline
  ^^ hardline
  ^^ !^"</Syntax>"


let comments_to_doc ~source_syntax ?deprecated (comments : string list) : document option =
  let deprecated_attr_to_comment d =
    match d with
    | None -> []
    | Some s -> [ string @@ "**Deprecated:** " ^ s ]
  in
  let comments =
    deprecated_attr_to_comment deprecated
    @ List.filter_map comments ~f:(fun comment ->
          let display_for_cameligo =
            Comments.is_doc_comment ~output_syntax:CameLIGO comment
          in
          let display_for_jsligo =
            Comments.is_doc_comment ~output_syntax:JsLIGO comment
          in
          match display_for_cameligo, display_for_jsligo with
          | true, true ->
            Option.some @@ string @@ Comments.format_doc_comment ~source_syntax comment
          | true, false ->
            Option.some
            @@ syntax_ CameLIGO
            @@ string
            @@ Comments.format_doc_comment ~source_syntax comment
          | false, true ->
            Option.some
            @@ syntax_ JsLIGO
            @@ string
            @@ Comments.format_doc_comment ~source_syntax comment
          | false, false -> None)
  in
  match comments with
  | [] -> None
  | comments -> Option.some @@ unlines @@ comments


let add_comments (doc_opt : document option) (value : document) : document =
  match doc_opt with
  | Some doc -> value ^^ hardline ^^ doc ^^ hardline
  | None -> value


let vdef_doc (comments : document option) (name : string) (t : Scopes.Types.type_case)
    : document
  =
  let p (syntax : Syntax_types.t) =
    let prefix =
      match syntax with
      | JsLIGO -> !^"let" ^//^ !^name ^^ colon
      | CameLIGO -> !^"val" ^//^ !^name ^//^ colon
    in
    Docs_utils.decompile_type_case ~escape_html_characters:true ~syntax ~prefix t
  in
  add_comments comments @@ syntax_title p


let value_binder_doc
    (comments : document option)
    (var : Value_var.t)
    (t : Ast_typed.type_expression)
    : document
  =
  if Value_var.is_generated var
  then empty
  else vdef_doc comments (Value_var.to_name_exn var) (Resolved t)


(* If type has parameters then we want to show it like [t<a, b>] *)
let rec get_params : Ast_core.type_content -> Type_var.t list = function
  | T_abstraction { ty_binder; type_; _ } -> ty_binder :: get_params type_.type_content
  | _ -> []


(* A bit hacky, handles cases like [type list_synonym = list] transforming [gen#xxx => gen#xxx list]
     abstraction to just [list] *)
let rec reduce_generated_params : Ast_core.type_expression -> Ast_core.type_expression
  = function
  | { type_content = T_abstraction { ty_binder; type_; _ }; _ }
    when Type_var.is_generated ty_binder -> reduce_generated_params type_
  | { type_content = T_app { type_operator; arguments }; _ } as t ->
    let is_generated_var : Ast_core.type_content -> bool = function
      | T_variable v -> Type_var.is_generated v
      | _ -> false
    in
    let good_arguments =
      List.filter
        ~f:(not <@ is_generated_var <@ Ast_core.(fun x -> x.type_content))
        arguments
    in
    if List.is_empty good_arguments
    then
      { t with
        type_content =
          (if List.is_empty type_operator.module_path
          then T_variable type_operator.element
          else T_module_accessor type_operator)
      }
    else { t with type_content = T_app { type_operator; arguments = good_arguments } }
  | t -> t


let type_expr_doc
    (comments : document option)
    (name : Type_var.t)
    (ty_expr : Ast_typed.ty_expr option)
      (* None for things like "type t" in module types*)
    : document
  =
  let mk_tvar : Type_var.t -> Ast_core.type_expression =
   fun v -> Ast_core.Combinators.t_variable v ~loc:Simple_utils.Location.dummy ()
  in
  if Type_var.is_generated name
  then empty
  else (
    let core_typ =
      (* We want to avoid cases like "type t = t", so, lets remove its original name, but
              still use orig_var for type experessions that are inside our type *)
      Option.bind
        ~f:(fun typ ->
          Option.map ~f:reduce_generated_params
          @@ Trace.to_option
          @@ Checking.untype_type_expression ~use_orig_var:true { typ with abbrev = None })
        ty_expr
    in
    let bindee =
      match core_typ with
      | Some typ ->
        let params = get_params typ.type_content in
        if List.is_empty params
        then mk_tvar name
        else
          Ast_core.Combinators.t_app
            ~loc:Simple_utils.Location.dummy
            { type_operator = Ligo_prim.Module_access.make_el @@ name
            ; arguments = List.map ~f:(fun var -> mk_tvar var) params
            }
            ()
      | None -> mk_tvar name
    in
    let doc_for syntax =
      let rec drop_params : Ast_core.type_expression -> Ast_core.type_expression
        = function
        | { type_content = T_abstraction { type_; _ }; _ } -> drop_params type_
        | t -> t
      in
      let print_core_type : Ast_core.type_expression -> document =
        Docs_utils.decompile_core_type ~escape_html_characters:true ~syntax ?prefix:None
      in
      let typ =
        match core_typ with
        | None -> None
        | Some core_typ ->
          if Ast_core.equal_type_content core_typ.type_content bindee.type_content
             || Option.value_map
                  ~default:false
                  ~f:(fun { type_operator = { module_path; element }; arguments = _ } ->
                    List.is_empty module_path && Type_var.equal element name)
                  (Ast_core.get_t_app @@ drop_params core_typ)
          then
            (* Happens with built-in types, we don't want to print e.g. [type int = int] *)
            None
          else Some (print_core_type core_typ)
      in
      let bindee = print_core_type bindee in
      match typ with
      | Some typ -> !^"type" ^//^ bindee ^//^ equals ^//^ typ
      | _ -> !^"type" ^/^ bindee
    in
    add_comments comments @@ syntax_title doc_for)


let rec sig_item_doc ~config (item : Ast_typed.sig_item) : document =
  let source_syntax = config.source_syntax in
  match Location.unwrap item with
  | S_value (var, typ, { leading_comments; _ }) ->
    (* XXX creates "let" kwd, is it ok for interfaces?  *)
    value_binder_doc (comments_to_doc ~source_syntax leading_comments) var typ
  | S_type (var, typ, { leading_comments; _ }) ->
    type_expr_doc (comments_to_doc ~source_syntax leading_comments) var (Some typ)
  | S_type_var (var, { leading_comments; _ }) ->
    type_expr_doc (comments_to_doc ~source_syntax leading_comments) var None
  | S_module _ | S_module_type _ ->
    empty (* FIXME I was unable to create this in a LIGO file*)


and module_doc
    ~config
    (comments : document option)
    (name : Module_var.t)
    (m : Ast_typed.module_content)
    : document
  =
  let from_unmangled_abs_path path =
    Docs_utils.reach_path_from_dir ~dir:(Filename.dirname config.source_file) ~path
  in
  let mod_path_to_string = String.concat ~sep:"." in
  let add_extension_md rel_path = FilePath.add_extension rel_path "md" in
  let md_file_for_module mod_path = add_extension_md @@ mod_path_to_string mod_path in
  (* Finds full module path for a module with given "relative path".
     E.g. we're inside module [A.B] and we have [module X = C].
     Then this [C] can refer either to [A.B.C] or to [A.C] (defined before [A.B])
     or to some [C] defined at toplevel (before [A]) *)
  (* TODO check what happens if [C] in example was [#import]ed from other file *)
  let try_find_module (m : string list) : string list option =
    let created rel_path =
      List.exists
        ~f:(fun { file_name; _ } -> String.equal rel_path file_name)
        !(config.additional_files)
    in
    let rec aux path_prefix =
      let candidate = path_prefix @ m in
      if created (md_file_for_module candidate)
      then Some candidate
      else (
        match List.drop_last path_prefix with
        | None -> None
        | Some smaller_prefix -> aux smaller_prefix)
    in
    aux config.current_mod_path
  in
  let unmangle_if_needed m =
    match Docs_utils.unmangle m with
    | None -> m
    | Some unmangled -> from_unmangled_abs_path unmangled
  in
  let mk_link_when_possible m =
    match try_find_module (String.split ~on:'.' m) with
    | None -> string m
    | Some mod_path ->
      link
        ~text:(string @@ mod_path_to_string mod_path)
        ~target:(string @@ String.lowercase (md_file_for_module mod_path))
  in
  match m with
  | M_struct m ->
    let name = String.lowercase (Module_var.to_name_exn name) in
    let full_path = mod_path_to_string @@ config.current_mod_path @ [ name ] in
    let file_name = add_extension_md full_path in
    let link =
      (* That's what we print to current file. FIXME: should be syntax dependant,
       but <SyntaxTitle> can't hold links for now *)
      link
        ~text:(string @@ "module " ^ name)
        ~target:(string (String.lowercase file_name))
      ^^ hardline
    and contents =
      (* Contents of file we create *)
      let heading = string @@ "# " ^ full_path in
      unlines
      @@ (add_comments comments heading
         :: (List.map ~f:(decl_doc ~config:(with_mod_name name config))
            @@ List.stable_sort ~compare:Docs_utils.compare_declarations m))
    in
    add_additional_file config { file_name; contents };
    if List.is_empty
         config.current_mod_path (* We're asked not to create links in the toplevel.md *)
    then empty
    else add_comments comments link
  | M_variable v ->
    (* can be generated by #import in CameLIGO *)
    let module_expr =
      string (Module_var.to_name_exn name)
      ^/^ !^"="
      ^/^ mk_link_when_possible
      @@ unmangle_if_needed
      @@ Module_var.to_name_exn v
    in
    (* FIXME: should be syntax dependant, but <SyntaxTitle> can't hold links for now *)
    !^"module" ^/^ module_expr
  | M_module_path v ->
    (* can be generated by #import in JsLIGO *)
    let module_expr =
      string (Module_var.to_name_exn name)
      ^/^ !^"="
      ^/^ mk_link_when_possible
      @@ unmangle_if_needed
      @@ String.concat ~sep:"."
      @@ List.map ~f:Module_var.to_name_exn
      @@ Utils.nseq_to_list v
    in
    (* FIXME: should be syntax dependant,
       but <SyntaxTitle> can't hold links for now *)
    !^"module" ^/^ module_expr


and module_type_doc
    ~(config : config)
    (comments : document option) (* TODO add comments to file with contents?? *)
    (name : Module_var.t)
    (m : Ast_typed.signature)
    : document
  =
  let name = Module_var.to_name_exn name in
  let full_path = String.concat ~sep:"." @@ config.current_mod_path @ [ name ] in
  let file_name = full_path ^ ".md" in
  let link =
    (* That's what we print to current file. FIXME: should be syntax dependant,
       but <SyntaxTitle> can't hold links for now *)
    link
      ~text:(string @@ "module type " ^ name)
      ~target:(string (String.lowercase file_name))
    ^^ hardline
  and contents =
    (* Contents of file we create *)
    let heading = string @@ "# " ^ full_path in
    unlines
    @@ (heading
       :: List.map ~f:(sig_item_doc ~config:(with_mod_name name config)) m.sig_items)
  in
  add_additional_file config { file_name; contents };
  add_comments comments link


and decl_doc ~config (decl : Ast_typed.decl) : document =
  let source_syntax = config.source_syntax in
  match decl.wrap_content with
  | D_value { binder; attr = { public; hidden; leading_comments; deprecated; _ }; _ }
    when public && not hidden ->
    value_binder_doc
      (comments_to_doc ~source_syntax ?deprecated leading_comments)
      binder.var
      binder.ascr
  | D_irrefutable_match
      { pattern; attr = { public; hidden; leading_comments; deprecated; _ }; _ }
    when public && not hidden ->
    (* Let's extract all binders and create a declaration for each of them. *)
    let binders = Linear_pattern.binders pattern in
    let doc = comments_to_doc ~source_syntax ?deprecated leading_comments in
    unlines @@ List.map binders ~f:(fun { var; ascr } -> value_binder_doc doc var ascr)
  | D_type
      { type_binder
      ; type_expr
      ; type_attr = { public; hidden; leading_comments; deprecated; _ }
      }
    when public && not hidden ->
    type_expr_doc
      (comments_to_doc ~source_syntax ?deprecated leading_comments)
      type_binder
      (Some type_expr)
  | D_module
      { module_binder
      ; module_
      ; module_attr = { public; hidden; leading_comments; deprecated; _ }
      ; _
      }
    when public && (not hidden) && not (Module_var.is_generated module_binder) ->
    module_doc
      ~config
      (comments_to_doc ~source_syntax ?deprecated leading_comments)
      module_binder
      module_.module_content
  | D_signature
      { signature_binder; signature; signature_attr = { leading_comments; public; _ } }
    when public ->
    module_type_doc
      ~config
      (comments_to_doc ~source_syntax leading_comments)
      signature_binder
      signature
  | D_module_include _ -> (* TODO *) empty
  | D_module _ | D_value _ | D_type _ | D_irrefutable_match _ | D_signature _ ->
    empty (* decl is hidden *)


let to_mdx ~(config : config) ?file_name (prg : Ast_typed.program)
    : PPrint.document returned_files'
  =
  let heading = string @@ "# " ^ Option.value ~default:"unnamed" file_name in
  let toplevel =
    unlines
    @@ (heading
       :: List.map
            (List.stable_sort ~compare:Docs_utils.compare_declarations prg.pr_module)
            ~f:(decl_doc ~config))
  in
  { file_name = "toplevel.md"; contents = toplevel } :: !(config.additional_files)


let to_mdx ~source_syntax ~source_file ?file_name prg : returned_files =
  let doc =
    to_mdx
      ?file_name
      ~config:
        { source_syntax; source_file; additional_files = ref []; current_mod_path = [] }
      prg
  in
  map_returned_files ~f:Docs_utils.doc_to_string_compact doc
