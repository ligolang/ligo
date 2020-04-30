
import Data.Foldable (for_)

import ParseTree
import Parser
import AST
import PrettyPrint

import System.Environment

main :: IO ()
main = do
  [fin] <- getArgs
  toParseTree fin >>= print
  (res, errs) <- runParser contract fin
  print (pp res)
  putStrLn ""
  putStrLn "Errors:"
  for_ errs (print . nest 2 . pp)