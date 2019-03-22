module A = struct
  open Rope_top_level_open
  open Rope

  let x = d
  let x = ~%d
  let x = (~%d) #% 42
  let x = (~%d%d)
  let x = (~%d%d) #% 42 43
  let x = (~%d%s) #% 42 "foo"
  let x = (~%(S"foo")%s) #% ""
  let x = (~%d%S"tralala"%d%s) #% 42 43 "foo"
end

module B = struct
  open Rope_top_level_open

  type foo = S | NotCaptured
  let d = NotCaptured
  let s = NotCaptured

  let x = Rope.(~%d) #% 42
  let x = Rope.(~%d%d)
  let x = Rope.(~%d%d) #% 42 43
  let x = Rope.(~%d%s) #% 42 "foo"
  let x = Rope.(~%(S"foo")%s) #% ""
  let x = Rope.(~%d%S"tralala"%d%s) #% 42 43 "foo"
end
