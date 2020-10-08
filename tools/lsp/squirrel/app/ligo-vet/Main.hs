import Control.Monad (unless)
import Data.Foldable (for_)
import Duplo.Pretty (pp, render)
import Main.Utf8 (withUtf8)
import Options.Applicative
  (Parser, ParserInfo, command, execParser, help, helper, hsubparser, info, long, metavar,
  progDesc, strOption)

import AST (parse)
import ParseTree (Source (Path))

data Command = PrintSexp FilePath

commandParser :: Parser Command
commandParser = hsubparser $ mconcat
  [ printSubCommand ]
  where
    printSubCommand = command "print-sexp"
      (info (PrintSexp <$> contractFileOption)
        (progDesc "Parse a given contract and print a s-exp representing it."))

-- | Parser of a path to a contract.
contractFileOption :: Parser FilePath
contractFileOption = strOption $
  long "contract" <>
  metavar "FILEPATH" <>
  help "Path to contract file"

programInfo :: ParserInfo Command
programInfo = info (helper <*> commandParser) mempty

main :: IO ()
main = withUtf8 $ do
  execParser programInfo >>= \case
    PrintSexp path -> do
      (tree, messages) <- parse (Path path)
      putStrLn (render (pp tree))
      unless (null messages) $ do
        putStrLn "The following errors have been encountered: "
        for_ messages $ \(range, err) -> do
          putStrLn (render (pp range <> ": " <> pp err))
