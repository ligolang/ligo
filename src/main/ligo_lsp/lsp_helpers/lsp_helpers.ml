(* Helpers provided by this lib *)
module Ligo_interface = Ligo_interface
module Helpers_file = Helpers_file
module Helpers_pretty = Helpers_pretty
module Path = Path

(* Wrappers / extended versions for modules from LIGO and Lsp.Types *)
module Def = Def
module Dialect_cst = Dialect_cst
module DocumentUri = Document_uri
module Position = Position
module Range = Range
module Location = Location

(* LIGO (Simple_utils) reexports *)
include Imports
module Trace = Simple_utils.Trace

(* Lsp.Types reexports *)
module Diagnostic = struct
  include Lsp.Types.Diagnostic

  let pp = Helpers_pretty.pp_with_yojson yojson_of_t
  let eq = Caml.( = )
  let testable = Alcotest.testable pp eq
end

module Locations = struct
  include Lsp.Types.Locations

  let pp = Helpers_pretty.pp_with_yojson yojson_of_t
  let eq x y = Yojson.Safe.equal (yojson_of_t x) (yojson_of_t y)

  (* Because Caml.(=) didn't work *)
  let testable = Alcotest.testable pp eq
end

module DocumentLink = struct
  include Lsp.Types.DocumentLink

  let pp = Helpers_pretty.pp_with_yojson yojson_of_t
  let eq = Caml.( = )
  let testable = Alcotest.testable pp eq
end

module FoldingRange = struct
  include Lsp.Types.FoldingRange

  let pp = Helpers_pretty.pp_with_yojson yojson_of_t
  let eq = Caml.( = )
  let testable = Alcotest.testable pp eq
end

module ClientCapabilities = Lsp.Types.ClientCapabilities
module ConfigurationItem = Lsp.Types.ConfigurationItem
module DiagnosticSeverity = Lsp.Types.DiagnosticSeverity
module DocumentLinkOptions = Lsp.Types.DocumentLinkOptions
module FoldingRangeKind = Lsp.Types.FoldingRangeKind
module Hover = Lsp.Types.Hover
module InitializeParams = Lsp.Types.InitializeParams
module InitializeResult = Lsp.Types.InitializeResult
module MarkedString = Lsp.Types.MarkedString
module MessageType = Lsp.Types.MessageType
module Registration = Lsp.Types.Registration
module RenameOptions = Lsp.Types.RenameOptions
module ServerCapabilities = Lsp.Types.ServerCapabilities
module ShowMessageParams = Lsp.Types.ShowMessageParams
module TextEdit = Lsp.Types.TextEdit
module WorkspaceEdit = Lsp.Types.WorkspaceEdit

(* Lsp reexports *)
module Client_notification = Lsp.Client_notification
module Client_request = Lsp.Client_request
module Server_notification = Lsp.Server_notification
module Server_request = Lsp.Server_request

(* Linol_lwt reexports *)
module IO = Linol_lwt.Jsonrpc2.IO
module Req_id = Linol_lwt.Jsonrpc2.Req_id
