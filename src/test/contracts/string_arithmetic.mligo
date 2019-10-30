(* Test that the string concatenation syntax in CameLIGO works *)

let size_op (s : string) : nat =
  String.size s

let slice_op (s : string) : string =
  String.slice 1p 2p s

let concat_syntax (s: string) =
  s ^ "test_literal"
