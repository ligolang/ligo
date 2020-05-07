
import Data.Foldable (for_)

import Control.Monad (unless)

import ParseTree
import Parser
import AST
import Pretty

import System.Environment

main :: IO ()
main = do
  [fin] <- getArgs
  toParseTree fin >>= print
  (res, errs) <- runParser contract fin
  putStrLn "----------------------"
  print (pp res)
  unless (null errs) do
    putStrLn ""
    putStrLn "Errors:"
    for_ errs (print . nest 2 . pp)