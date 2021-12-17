type t = {
  syntax_name:                string;
  alt_name:                   string;
  file_types:                 string list;
  scope_name:                 string;
  folding_start_marker:       string option;
  folding_stop_marker:        string option;
  language_features:          language_features;
  syntax_patterns:            string list;
  repository:                 pattern list
}

(*
  a temporary solution, as we currently don't have a way to target these 
  regexp enginges via one regexp.
*)
and regexp = {
  emacs:    string;
  textmate: string;
  vim:      string;
}

and language_features = {
  operators: string list; (* currently only used by EMacs, but ideally also for all the other editors *)
  string_delimiters: regexp list; (* used by all editors *)
  comments: language_features_comments; (* used by all editors *)
  brackets: (string * string) list; (* currently not used *)
  auto_closing_pairs: (string * string) list; (* used by VS Code *)
  surrounding_pairs: (string * string) list; (* used by VS Code *)
  syntax_table: (string * string) list; (* for Emacs *)
}

and language_features_comments = {
  line_comment:  regexp;
  block_comment: (regexp * regexp)
}

(* Shared syntax highlighting names for all editors. *)
and highlight_name = 
  Comment
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
  Referenced_rule_does_not_exist of string
| Meta_name_some_but_empty of string
| Begin_cant_be_empty of string
| End_cant_be_empty of string

and pattern = {
  name: string;
  kind: pattern_kind;
}

and pattern_kind = 
  Begin_end of begin_end_pattern (* currently doesn't work well in EMacs *)
| Match     of match_pattern (* preferred for now due to EMacs *)

and begin_end_pattern = {
  meta_name:      highlight_name option;
  (* If multiple parts need to be highlighted in a regexp, the regexp should 
     be split. This is because of VIM. *)
  begin_:         (regexp * highlight_name option) list;
  end_:           (regexp * highlight_name option) list;
  patterns:       string list;
}

and match_pattern = {
  (* If multiple parts need to be highlighted in a regexp, the regexp should 
     be split. This is because of VIM. *)
  match_:   (regexp * highlight_name option) list;
  match_name: highlight_name option
}