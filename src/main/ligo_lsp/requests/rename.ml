open Lsp.Types

let rename_reference : string -> Range.t -> TextEdit.t =
 fun newText range -> TextEdit.create ~range ~newText
