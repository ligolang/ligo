type error =
  | WrongFunctionArguments of AST.expr
  | InvalidWild of AST.expr

exception Error of error
