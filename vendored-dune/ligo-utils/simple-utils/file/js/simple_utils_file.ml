let exists ?dir file_path =
  let uri_s =
  match dir with
  | Some dir when String.(dir = "." || dir = "") -> file_path
  | Some dir -> dir ^ "/" ^ file_path 
  | None -> file_path in
  let uri = Uri.of_string uri_s in
  let open Js_of_ocaml in
  let open XmlHttpRequest in
  let req = create () in
  let method_ = "GET" in
  req##_open (Js.string method_) (Js.string uri_s) Js._false;
  req##send Js.null;
  let code = req##.status in
  match code = 200 with
  | true -> Some uri_s
  | false -> None

let read uri_s =
  let uri = Uri.of_string uri_s in
  let open Js_of_ocaml in
  let open XmlHttpRequest in
  let req = create () in
  let method_ = "GET" in
  req##_open (Js.string method_) (Js.string uri_s) Js._false;
  req##send Js.null;
  let code = req##.status in
  let responseText = req##.responseText |> Js.Opt.to_option  in
  match code = 200, responseText with
  | true, Some responseText -> responseText |> Js.to_string |> Option.some
  | _ -> None
