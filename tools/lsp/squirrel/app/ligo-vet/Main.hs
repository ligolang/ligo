module Main (main) where

import Duplo.Pretty (Pretty, pp, render)
import Main.Utf8 (withUtf8)
import Options.Applicative
  (Parser, ParserInfo, command, execParser, help, helper, hsubparser, info, long, metavar, progDesc,
  short, strOption, switch)
import System.Environment (setEnv)

import AST (Fallback, FindFilepath, ParsedContract (..), _getContract, parse, parseWithScopes)
import Diagnostic (Message (..))
import Log (runNoLoggingT)
import ParseTree (pathToSrc)

newtype Command = PrintSexp PrintSexpOptions

data PrintSexpOptions = PrintSexpOptions
  { psoContract :: FilePath
  , psoWithScopes :: Bool
  }

commandParser :: Parser Command
commandParser = hsubparser $ mconcat
  [ printSubCommand ]
  where
    printSubCommand = command "print-sexp"
      (info printSexp
        (progDesc "Parse a given contract and print a s-exp representing it."))

printSexp :: Parser Command
printSexp = do
  psoContract <- contractFileOption
  psoWithScopes <- withScopesOption
  pure (PrintSexp PrintSexpOptions{ .. })
  where
    withScopesOption = switch $
      long "with-scopes" <>
      short 's' <>
      help "Whether to add fallback scopes to the tree"

-- | Parser of a path to a contract.
contractFileOption :: Parser FilePath
contractFileOption = strOption $
  long "contract" <>
  metavar "FILEPATH" <>
  help "Path to contract file"

programInfo :: ParserInfo Command
programInfo = info (helper <*> commandParser) mempty

data SomePretty where
  SomePretty :: Pretty a => a -> SomePretty

instance Pretty SomePretty where
  pp (SomePretty a) = pp a

main :: IO ()
main = withUtf8 do
  PrintSexp PrintSexpOptions{ .. } <- liftIO $ execParser programInfo
  -- Don't depend on LIGO.
  liftIO $ setEnv "LIGO_BINARY_PATH" "/dev/null"

  let
    treeMsgs (ParsedContract _ tree msgs) = (tree, msgs)
    toPretty :: (Functor f, Pretty info) => f (FindFilepath info) -> f (SomePretty, [Message])
    toPretty = fmap (first SomePretty . treeMsgs . _getContract)
    parser
      | psoWithScopes = toPretty . parseWithScopes @Fallback
      | otherwise     = toPretty . runNoLoggingT . parse <=< runNoLoggingT . pathToSrc
  (tree, messages) <- parser psoContract
  liftIO do
    putStrLn (render (pp tree))
    unless (null messages) do
      putTextLn "The following errors have been encountered: "
      for_ messages \(Message err _ range) ->
        putStrLn (render (pp range <> ": " <> pp err))
