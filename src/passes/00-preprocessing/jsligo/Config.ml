(* Static configuration for JsLIGO *)

type block_comment_delimiters = <opening : string; closing : string>
type line_comment_delimiter   = string (* Opening of a line comment *)
type string_delimiter         = string
type verbatim_delimiters      = <opening : string; closing : string>

let block =
  object
    method opening = "/*"
    method closing = "*/"
  end

let block    = Some block
let line     = Some "//"
let string   = Some "\""
let file_ext = Some ".jsligo"

let verbatim =
  object
    method opening = "`"
    method closing = "`"
  end

let verbatim = Some verbatim

type file_name   = string
type module_name = string

let mk_module file_name module_name =
  Printf.sprintf "import %s = %s;" module_name file_name
