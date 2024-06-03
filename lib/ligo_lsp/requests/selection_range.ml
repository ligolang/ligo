open Core
open Lsp_helpers
open Handler
module Loc = Simple_utils.Location
module Region = Simple_utils.Region

(** [selection_range cst pos] returns a selection range for a given position.

    A selection range is a range around a given position [pos] that a user may
    be interested in selecting, consisting of the nearest range that includes
    the position & all parent ranges of that range. *)
let selection_range cst (position : Position.t) : SelectionRange.t =
  let open Cst_shared.Fold in
  let select_range parent_range range =
    Some { SelectionRange.parent = parent_range; range }
  in
  let process (region : Region.t) =
    let range = Range.of_region region in
    (* If the position is within the range, then we add the range
       to our top-down tree of ranges (using [select_range]) and
       continue to the node's children. *)
    if Range.contains_position position range then Continue range else Stop
  in
  Dialect_cst.from_dialect
    { cameligo =
        (let open Cst_cameligo.Fold in
        fold_cst None select_range (fun (Some_node (content, tag)) ->
            match tag with
            | S_reg _ -> process content.region
            | S_wrap _ -> process content#region
            | _ -> Skip))
    ; jsligo =
        (let open Cst_jsligo.Fold in
        fold_cst None select_range (fun (Some_node (content, tag)) ->
            match tag with
            | S_reg _ -> process content.region
            | S_wrap _ -> process content#region
            | _ -> Skip))
    }
    cst
  |> Option.value
       ~default:
         (* Every position [position] in the selection range request must have an
            associated selection range result. For results where some positions
            do not have selection ranges, the LSP permits empty ranges starting
            at [position].

            See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_selectionRange *)
         { SelectionRange.parent = None; range = Range.Construct.empty ~position () }

(** Runs the handler of a selection range request. This will return a selection
    range for each position in the list of positions [positions] in the file at
    [path].

    @see {{: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_selectionRange}LSP spec} *)
let on_req_selection_range : Path.t -> Position.t list -> SelectionRange.t list Handler.t =
 fun path positions ->
  with_cst path ~default:[] ~on_error:(fun _err ->
      send_message ~type_:Error
      @@ "Can not select ranges in a file with preprocessor or lexer errors")
  @@ fun cst -> positions |> List.map ~f:(selection_range cst) |> Handler.return
