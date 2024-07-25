open Ctypes
open Tree_sitter.Api_types

module Functions (S: FOREIGN) = struct 
  open S 
  let tree_sitter_typescript = foreign "tree_sitter_typescript" (void @-> returning (ptr ts_language))
end 