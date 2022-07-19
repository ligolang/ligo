module Main (main) where

import Control.Monad (join)
import Lib
import Options.Applicative

main :: IO ()
main =
  join . customExecParser (prefs showHelpOnError) $
    info
      (helper <*> parser)
      ( fullDesc
          <> header "LIGO WebIDE backend"
          <> progDesc "compile LIGO contracts to Tezos"
      )
  where
    parser :: Parser (IO ())
    parser = fmap startApp $
      Config
        <$> strOption
          ( long "ligo-path"
            <> short 'l'
            <> metavar "STRING"
            <> help "path to LIGO binary"
          )
        <*> option auto
          ( long "port"
            <> short 'p'
            <> metavar "INT"
            <> showDefault
            <> value 8080
            <> help "port the server should use"
          )
        <*> switch
          ( long "verbose"
            <> short 'v'
            <> help "print received requests and the responses"
          )
