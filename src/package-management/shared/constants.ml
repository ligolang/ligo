type command = string * string array

let ligo_compile_storage ?(ligo = "ligo") ~main ~expression () =
  "", [| ligo; "compile"; "storage"; main; expression |]
