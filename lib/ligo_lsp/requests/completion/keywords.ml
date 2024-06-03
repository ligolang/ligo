open Core
open Common
open Lsp_helpers

(** Helper module type to deal with CameLIGO or JsLIGO tokens. *)
module type Built_in = sig
  (** A list with keywords for a given dialect. *)
  val keywords_list : string list

  (** A list with operators for a given dialect. *)
  val symbols_list : string list
end

(** Gets completions for keywords and operators based on whether we're dealing with
    CameLIGO or JsLIGO. See [cameligo_keyword_completions] and
    [jsligo_keyword_completions]. *)
let dialect_keyword_completions (module Built_in : Built_in) : CompletionItem.t list =
  List.map
    Built_in.(keywords_list @ symbols_list)
    ~f:(fun keyword ->
      CompletionItem.create
        ~label:keyword
        ~kind:CompletionItemKind.Keyword
        ~sortText:(completion_context_priority Keyword)
        ())

(** Gets completions for CameLIGO keywords and operators. *)
let cameligo_keyword_completions : CompletionItem.t list =
  dialect_keyword_completions (module Lx_ml_self_tokens.Token)

(** Gets completions for JsLIGO keywords and operators. *)
let jsligo_keyword_completions : CompletionItem.t list =
  dialect_keyword_completions (module Lx_js_self_tokens.Token)

(** Gets completions for keywords and operators based on whether we're dealing with
    CameLIGO or JsLIGO. *)
let get_keyword_completions : Syntax_types.t -> CompletionItem.t list = function
  | CameLIGO -> cameligo_keyword_completions
  | JsLIGO -> jsligo_keyword_completions
