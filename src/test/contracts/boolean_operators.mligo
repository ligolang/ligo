// Test CameLIGO boolean operators

let or_true (b : bool) : bool =
   b or True

let or_false (b : bool) : bool =
   b or False

let and_true (b : bool) : bool =
   b and True

let and_false (b : bool) : bool =
   b and False

let not_bool (b: bool) : bool =
   not b
