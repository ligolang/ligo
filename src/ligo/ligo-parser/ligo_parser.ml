module Parser = Parser
module Lexer = Lexer.Make(LexToken)
module AST = AST
module AST2 = AST2
module Typed = Typecheck2

let ast_to_typed_ast ast = ast |> AST2.s_ast |> Typed.annotate
