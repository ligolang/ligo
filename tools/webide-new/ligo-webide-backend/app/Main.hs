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
          <> progDesc "provide a server interface to the LIGO compiler"
      )
  where
    parser :: Parser (IO ())
    parser = fmap startApp $
      Config
        <$> optional (strOption
          ( long "ligo-path"
            <> short 'l'
            <> metavar "STRING"
            <> help "path to LIGO binary"
          ))
        <*> optional (strOption
          ( long "tezos-client-path"
            <> short 't'
            <> metavar "STRING"
            <> help "path to tezos-client binary"
          ))
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
        <*> optional (strOption
          ( long "dockerized-ligo-version"
          <> short 'd'
          <> metavar "LIGO_VERSION"
          <> help "use a LIGO from Docker instead of a \
                  \LIGO binary. If this is specified, 'ligo-path' \
                  \will be ignored."
          ))
