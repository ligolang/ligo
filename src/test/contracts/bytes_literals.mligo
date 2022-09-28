
let tests =
  [ ("666f6f" : bytes) = 0x666f6f
  ; [%bytes "foo"]     = 0x666f6f
  ; [%bytes "666f6f"]  = 0x363636663666
  ]
