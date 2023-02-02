(* Main code
  This is the code that creates an instance of the lsp server class
  and runs it as a task. *)

module Requests = Ligo_lsp.Server.Requests

module Server = Ligo_lsp.Server.Make (struct
  module Info = Info
  module Print = Print
end)

let run () =
  let s = new Server.lsp_server in
  let server = Linol_lwt.Jsonrpc2.create_stdio (s :> Linol_lwt.Jsonrpc2.server) in
  let task = Linol_lwt.Jsonrpc2.run server in
  match Linol_lwt.run task with
  | () -> Ok ("", "")
  | exception e ->
    let e = Caml.Printexc.to_string e in
    Error ("", e)
