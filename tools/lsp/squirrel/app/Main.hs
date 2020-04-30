
import ParseTree
import Parser
import AST

import System.Environment

main :: IO ()
main = do
  [fin] <- getArgs
  toParseTree fin >>= print
  runParser contract fin >>= print