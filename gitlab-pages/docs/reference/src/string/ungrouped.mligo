let size_op (s : string) : nat = String.length s
let slice_op (s: string) : string = String.sub 1n 2n s
let concat_syntax (s : string) = String.concat s "test_literal"
let concat_syntax_alt (s: string) = s ^ "test_literal"