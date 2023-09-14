[@@@warning "-27"]

type 'a command =
  { group_name : string
  ; value : 'a
  ; contained : bool
  ; contained_in : string list
  ; contains : string list
  ; next_groups : string list
  }

type keyword = string list command
type match_ = string command

type match_group =
  { match_group_name : string
  ; start : string option
  ; end_ : string option
  }

type region_inside =
  { start : string option
  ; end_ : string option
  ; match_groups : match_group list
  }

type region = region_inside command

type syntax =
  | Match of match_
  | Region of region

type link =
  { group_name : string
  ; highlight : Core.highlight_name
  }

type highlight = Link of link

type item =
  | Syntax of syntax
  | Highlight of highlight
  | VIMComment of string

type t = item list

module Print = struct
  open Format

  let print_list fmt prefix = function
    | [] -> ()
    | _ as l ->
      fprintf fmt prefix;
      pp_print_list
        ~pp_sep:(fun fmt a -> fprintf fmt ",")
        (fun fmt a -> fprintf fmt "%s" a)
        fmt
        l;
      fprintf fmt " "

  let rec print_syntax fmt = function
    | Match { group_name; value = regexp; contained; contained_in; next_groups; contains }
      ->
      fprintf fmt "syntax match %s \"%s\" " group_name regexp;
      if contained then fprintf fmt "contained ";
      print_list fmt "containedin=" contained_in;
      print_list fmt "contains=" contains;
      print_list fmt "nextgroup=" next_groups;
      if next_groups <> [] then fprintf fmt "skipempty skipwhite";
      fprintf fmt "\n"
    | Region
        { group_name
        ; value = { start; end_; match_groups }
        ; next_groups
        ; contained
        ; contained_in
        ; contains
        } ->
      fprintf fmt "syntax region %s " group_name;
      print_match_groups fmt match_groups;
      (match start with
      | Some start -> fprintf fmt "start=\"%s\" " start
      | None -> ());
      (match end_ with
      | Some end_ -> fprintf fmt "end=\"%s\" " end_
      | None -> ());
      if contained then fprintf fmt "contained ";
      print_list fmt "containedin=" contained_in;
      print_list fmt "contains=" contains;
      print_list fmt "nextgroup=" next_groups;
      if next_groups <> [] then fprintf fmt "skipempty skipwhite";
      fprintf fmt "\n"

  and print_match_groups fmt = function
    | { match_group_name; start; end_ } :: rest ->
      fprintf fmt "matchgroup=%s " match_group_name;
      (match start with
      | Some s -> fprintf fmt "start=\"%s\" " s
      | None -> ());
      (match end_ with
      | Some s -> fprintf fmt "end=\"%s\" " s
      | None -> ());
      print_match_groups fmt rest
    | [] -> ()

  let highlight_to_string = function
    | Core.Comment -> "Comment"
    | Constant -> "Constant"
    | String -> "String"
    | Character -> "Character"
    | Number -> "Number"
    | Boolean -> "Boolean"
    | Float -> "Float"
    | FunctionName -> "Statement"
    | Identifier -> "Identifier"
    | Builtin_function | Function -> "Function"
    | Statement -> "Statement"
    | Conditional -> "Conditional"
    | Repeat -> "Repeat"
    | Label -> "Label"
    | Operator -> "Operator"
    | Keyword -> "Keyword"
    | Exception -> "Exception"
    | PreProc -> "PreProc"
    | Builtin_type | Type -> "Type"
    | Type_var -> "Type"
    | StorageClass -> "StorageClass"
    | Builtin_module | Structure -> "Structure"
    | Typedef -> "Typedef"
    | SpecialChar -> "SpecialChar"
    | SpecialComment -> "SpecialComment"
    | Underlined -> "Underlined"
    | Error -> "Error"
    | Todo -> "Todo"
    | Attribute -> "PreProc"

  let print_highlight fmt = function
    | Link { group_name; highlight } ->
      fprintf fmt "highlight link %s %s \n" group_name (highlight_to_string highlight)

  let print_comment fmt comment = fprintf fmt "\n\" %s\n" comment

  let print fmt v =
    List.iter
      (fun i ->
        match i with
        | VIMComment c -> print_comment fmt c
        | Syntax s -> print_syntax fmt s
        | Highlight h -> print_highlight fmt h)
      v
end

module Convert = struct
  let make_references (t : Core.t) =
    List.concat_map (function
        | Core.Self_ref -> [ "@top" ]
        | Core.Name_ref p -> [ p ]
        | Core.String_ref ->
          List.mapi
            (fun i _ -> "string" ^ string_of_int i)
            t.language_features.string_delimiters)

  let pattern_to_vim : Core.t -> Core.pattern -> item list =
   fun t ->
    let toplevel = t.syntax_patterns in
    function
    | { name; kind = Begin_end { meta_name = highlight; begin_; end_; patterns } } ->
      let rec aux
          name
          result
          (begin_ : (Core.regexp * Core.highlight_name option) list)
          end_
        =
        match begin_, end_ with
        | [ (start, highlight_start) ], (end_, highlight_end) :: rest ->
          aux
            (match rest with
            | [] -> name
            | _ -> name ^ "___")
            ((match highlight_end with
             | Some highlight ->
               [ Highlight (Link { group_name = name ^ "__"; highlight }) ]
             | None -> [])
            @ (match highlight_start with
              | Some highlight ->
                [ Highlight (Link { group_name = name ^ "_"; highlight }) ]
              | None -> [])
            @ (match highlight with
              | Some highlight -> [ Highlight (Link { group_name = name; highlight }) ]
              | None -> [])
            @ (Syntax
                 (Region
                    { group_name = name
                    ; value =
                        ({ start =
                             (match highlight_start with
                             | Some _ -> None
                             | None -> Some start.Core.vim)
                         ; end_ =
                             (match highlight_end with
                             | Some _ -> None
                             | None -> Some end_.Core.vim)
                         ; match_groups =
                             ((match highlight_start with
                              | Some _ ->
                                [ { match_group_name = name ^ "_"
                                  ; start = Some start.Core.vim
                                  ; end_ = None
                                  }
                                ]
                              | None -> [])
                             @
                             match highlight_end with
                             | Some _ ->
                               [ { match_group_name = name ^ "__"
                                 ; start = None
                                 ; end_ = Some end_.Core.vim
                                 }
                               ]
                             | None -> [])
                         } : region_inside)
                    ; next_groups =
                        (match rest with
                        | [] -> []
                        | _ -> [ name ^ "___" ])
                    ; contained = not (List.mem (Core.Name_ref name) toplevel)
                    ; contained_in = []
                    ; contains = make_references t patterns
                    })
              :: result))
            []
            rest
        | (match_, highlight) :: rest, end_ ->
          aux
            (name ^ "___")
            ((match highlight with
             | Some highlight -> [ Highlight (Link { group_name = name; highlight }) ]
             | None -> [])
            @ (Syntax
                 (Match
                    { group_name = name
                    ; value = match_.Core.vim
                    ; next_groups = [ name ^ "___" ]
                    ; contained = not (List.mem (Core.Name_ref name) toplevel)
                    ; contained_in = []
                    ; contains = []
                    })
              :: result))
            rest
            end_
        | [], (match_, highlight) :: rest_e ->
          aux
            (name ^ "___")
            ((match highlight with
             | Some highlight -> [ Highlight (Link { group_name = name; highlight }) ]
             | None -> [])
            @ (Syntax
                 (Match
                    { group_name = name
                    ; value = match_.Core.vim
                    ; next_groups =
                        (match rest_e with
                        | [] -> []
                        | _ -> [ name ^ "___" ])
                    ; contained = not (List.mem (Core.Name_ref name) toplevel)
                    ; contained_in = []
                    ; contains = []
                    })
              :: result))
            []
            rest_e
        | [], [] -> List.rev result
      in
      [ VIMComment name ] @ aux name [] begin_ end_
    | { name; kind = Match { match_; match_name } } ->
      let rec aux result name = function
        | (regexp, highlight) :: rest ->
          aux
            ([ Syntax
                 (Match
                    { group_name = name
                    ; value = regexp.Core.vim
                    ; contained = not (List.mem (Core.Name_ref name) toplevel)
                    ; contained_in = []
                    ; contains = []
                    ; next_groups =
                        (match rest with
                        | _ :: _ -> [ name ^ "_" ]
                        | _ -> [])
                    })
             ]
            @ (match highlight with
              | Some highlight -> [ Highlight (Link { group_name = name; highlight }) ]
              | None -> [])
            @ result)
            (name ^ "_")
            rest
        | _ :: rest -> aux result name rest
        | [] -> result
      in
      let matches = aux [] name match_ in
      [ VIMComment name ]
      @ matches
      @
      (match match_name with
      | Some highlight -> [ Highlight (Link { group_name = name; highlight }) ]
      | None -> [])

  let to_vim : Core.t -> t =
   fun t ->
    let language_features = t.Core.language_features in
    let comments = language_features.comments in
    let extra_patterns = language_features.extra_patterns in
    let strings =
      List.mapi
        (fun i d ->
          { group_name = "string" ^ string_of_int i
          ; value = { start = Some d.Core.vim; end_ = Some d.Core.vim; match_groups = [] }
          ; contained = false
          ; contained_in = []
          ; contains = "@Spell" :: make_references t extra_patterns.in_strings
          ; next_groups = []
          })
        language_features.string_delimiters
    in
    List.fold_left (fun a c -> pattern_to_vim t c @ a) [] t.repository
    @ [ VIMComment "string" ]
    @ List.concat_map
        (fun s ->
          [ Syntax (Region s)
          ; Highlight (Link { group_name = s.group_name; highlight = Core.String })
          ])
        strings
    @ [ VIMComment "linecomment" ]
    @ [ Syntax
          (Region
             { group_name = "linecomment"
             ; value =
                 { start = Some comments.line_comment.Core.vim
                 ; end_ = Some "$"
                 ; match_groups = []
                 }
             ; contained = false
             ; contained_in =
                 "ALLBUT" :: "blockcomment" :: make_references t [ String_ref ]
             ; contains = "@Spell" :: make_references t extra_patterns.in_line_comments
             ; next_groups = []
             })
      ]
    @ [ Highlight (Link { group_name = "linecomment"; highlight = Core.Comment }) ]
    @ [ VIMComment "blockcomment" ]
    @ [ Syntax
          (Region
             { group_name = "blockcomment"
             ; value =
                 { start = Some (fst comments.block_comment).Core.vim
                 ; end_ = Some (snd comments.block_comment).Core.vim
                 ; match_groups = []
                 }
             ; contained = false
             ; contained_in =
                 "ALLBUT" :: "blockcomment" :: make_references t [ String_ref ]
             ; contains = "@Spell" :: make_references t extra_patterns.in_block_comments
             ; next_groups = []
             })
      ]
    @ [ Highlight (Link { group_name = "blockcomment"; highlight = Core.Comment }) ]
end

let vim_ftdetect (syntaxes : (string * Core.t) list) : string =
  let buffer = Buffer.create 512 in
  let open Format in
  let fmt = formatter_of_buffer buffer in
  fprintf
    fmt
    "\" THIS FILE WAS AUTOMATICALLY GENERATED. DO NOT MODIFY MANUALLY OR YOUR CHANGES \
     WILL BE LOST.\n";
  Fun.flip List.iter syntaxes (fun (name, t) ->
      Fun.flip List.iter t.file_types (fun file_type ->
          fprintf fmt "au BufNewFile,BufRead *.%s setfiletype %s\n" file_type name));
  Buffer.contents buffer

let vim_plugin (syntaxes : (string * Core.t) list) : string =
  let buffer = Buffer.create 512 in
  let open Format in
  let fmt = formatter_of_buffer buffer in
  let allow_list =
    String.concat ", " (List.map (fun (name, _) -> "'" ^ name ^ "'") syntaxes)
  in
  fprintf
    fmt
    "\" THIS FILE WAS AUTOMATICALLY GENERATED. DO NOT MODIFY MANUALLY OR YOUR CHANGES \
     WILL BE LOST.\n";
  fprintf fmt "if executable('ligo')\n";
  fprintf fmt "  if !exists(\"autocommands_loaded\")\n";
  fprintf fmt "    let autocommands_loaded=1\n";
  fprintf fmt "    augroup ligoRegisterLanguageServer\n";
  fprintf fmt "      autocmd User lsp_setup\n";
  fprintf fmt "          \\ call lsp#register_server({\n";
  fprintf fmt "          \\   'name': 'ligo_lsp',\n";
  fprintf fmt "          \\   'cmd': {server_info->['ligo', 'lsp']},\n";
  fprintf fmt "          \\   'allowlist': [%s],\n" allow_list;
  fprintf fmt "          \\ })\n";
  fprintf fmt "    augroup END\n";
  fprintf fmt "  endif\n";
  fprintf fmt "endif\n";
  Buffer.contents buffer

let vim_syntax ((name, t) : string * Core.t) : string =
  let v = Convert.to_vim t in
  let buffer = Buffer.create 8192 in
  let open Format in
  let fmt = formatter_of_buffer buffer in
  fprintf
    fmt
    "\" THIS FILE WAS AUTOMATICALLY GENERATED. DO NOT MODIFY MANUALLY OR YOUR CHANGES \
     WILL BE LOST.\n";
  fprintf fmt "if exists(\"b:current_syntax\")\n";
  fprintf fmt "    finish\n";
  fprintf fmt "endif\n";
  fprintf fmt "\n";
  fprintf fmt "syntax cluster top contains=TOP\n";
  Print.print fmt v;
  fprintf fmt "\nlet b:current_syntax = \"%s\"\n" name;
  Buffer.contents buffer
