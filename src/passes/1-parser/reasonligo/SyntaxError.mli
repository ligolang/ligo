type error = 
  | WrongFunctionArguments of AST.expr

exception Error of error