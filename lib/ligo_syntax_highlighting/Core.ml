type reference =
  | Name_ref of string
  | Self_ref
  | String_ref

type t =
  { syntax_name : string
  ; alt_name : string
  ; file_types : string list
  ; scope_name : string
  ; folding_start_marker : string option
  ; folding_stop_marker : string option
  ; language_features : language_features
  ; syntax_patterns : reference list
  ; repository : pattern list
  }

(*
  a temporary solution, as we currently don't have a way to target these 
  regexp engines via one regexp.
*)
and regexp =
  { emacs : string
  ; textmate : string
  ; vim : string
  }

(* FIXME: doesn't support Emacs for now *)
and extra_patterns =
  { (* Match extra things inside line comments. *)
    in_line_comments : reference list
  ; (* Match extra things inside block comments. *)
    in_block_comments : reference list
  ; (* Match extra things inside strings. *)
    in_strings : reference list
  }

and language_features =
  { operators : string list
        (* currently only used by EMacs, but ideally also for all the other editors *)
  ; string_delimiters : regexp list (* used by all editors *)
  ; comments : regexp language_features_comments (* used by all editors *)
  ; comments_insertion : string language_features_comments
        (* used by VS Code for now, but Emacs may use it as well *)
  ; extra_patterns : extra_patterns
        (* used to match things inside comments or strings, such as JsLIGO attributes *)
  ; brackets : (string * string) list (* currently not used *)
  ; auto_closing_pairs : (string * string) list (* used by VS Code *)
  ; surrounding_pairs : (string * string) list (* used by VS Code *)
  ; syntax_table : (string * string) list (* for Emacs *)
  }

and 'gen_type language_features_comments =
  { line_comment : 'gen_type
  ; block_comment : 'gen_type * 'gen_type
  }

(* Shared syntax highlighting names for all editors. *)
and highlight_name =
  | Comment
  | Attribute
  | Constant
  | String
  | Character
  | Number
  | Boolean
  | Float
  | Identifier
  | Function
  | Statement
  | Conditional
  | Repeat
  | Label
  | Operator
  | Keyword
  | Exception
  | PreProc
  | Type
  | Type_var
  | StorageClass
  | Structure
  | Typedef
  | SpecialChar
  | SpecialComment
  | Underlined
  | Error
  | Todo
  | Builtin_type
  | Builtin_module
  | Builtin_function
  | FunctionName

and error =
  | Referenced_rule_does_not_exist of string
  | Meta_name_some_but_empty of string
  | Begin_cant_be_empty of string
  | End_cant_be_empty of string

and pattern =
  { name : string
  ; kind : pattern_kind
  }

and pattern_kind =
  | Begin_end of begin_end_pattern (* currently doesn't work well in EMacs *)
  | Match of match_pattern (* preferred for now due to EMacs *)

and begin_end_pattern =
  { meta_name : highlight_name option
  ; (* If multiple parts need to be highlighted in a regexp, the regexp should 
     be split. This is because of VIM. *)
    begin_ : (regexp * highlight_name option) list
  ; end_ : (regexp * highlight_name option) list
  ; patterns : reference list
  }

and match_pattern =
  { (* If multiple parts need to be highlighted in a regexp, the regexp should 
     be split. This is because of VIM. *)
    match_ : (regexp * highlight_name option) list
  ; match_name : highlight_name option
  }
