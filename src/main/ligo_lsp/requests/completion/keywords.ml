open Common
open Lsp_helpers
module SMap = Map.Make (String)

module type Built_in = sig
  type t

  val keywords : (Simple_utils.Region.t -> t) SMap.t
  val symbols : (Simple_utils.Region.t -> t) SMap.t
end

let dialect_keyword_completions (module Built_in : Built_in) : CompletionItem.t list =
  List.map
    Built_in.(Map.keys keywords @ Map.keys symbols)
    ~f:(fun keyword ->
      CompletionItem.create
        ~label:keyword
        ~kind:CompletionItemKind.Keyword
        ~sortText:(completion_context_priority Keyword)
        ())


let cameligo_keyword_completions : CompletionItem.t list =
  dialect_keyword_completions (module Lx_ml_self_tokens.Token)


let jsligo_keyword_completions : CompletionItem.t list =
  dialect_keyword_completions (module Lx_js_self_tokens.Token)


let get_keyword_completions : Syntax_types.t -> CompletionItem.t list = function
  | CameLIGO -> cameligo_keyword_completions
  | JsLIGO -> jsligo_keyword_completions
