module CST = Cst.Cameligo

type error =
  | WrongFunctionArguments of CST.expr
  | InvalidWild of CST.expr

exception Error of error
