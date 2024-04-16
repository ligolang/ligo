(* TODO: Implement Http_uri for unix 
   For context, the following is the implementation for jsoo:  
   {[
      let fetch uri =
        let open Js_of_ocaml in
        let open XmlHttpRequest in
        let req = create () in
        let method_ = "GET" in
        req##_open (Js.string method_) (Js.string (Uri.to_string uri)) Js._false;
        req##send Js.null;
        let code = req##.status in
        let responseText = req##.responseText |> Js.Opt.to_option in
        match code = 200, responseText with
        | true, Some responseText -> responseText |> Js.to_string
        | _ -> failwith "failed"

      let get_filename uri = uri |> Uri.path |> Filename.basename 
   ]} 
*)

let fetch _uri = failwith "TODO: Http_uri.fetch for Unix"
let get_filename _uri = failwith "TODO: Http_uri.get_filename for Unix"
