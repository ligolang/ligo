open Ctypes

let ts_language = Tree_sitter.Api_types.ts_language

module Functions (F : FOREIGN) = struct
  open F

  let tree_sitter_typescript =
    foreign "tree_sitter_typescript" (void @-> returning (ptr ts_language))
end
