module Config
  ( ServerConfig (..)
  , ConnectionConfig (..)
  , ConnectionId
  , readConfig
  ) where

import Control.Arrow ((>>>))
import Data.Text qualified as Text
import Options.Applicative

data ServerConfig = ServerConfig
  { scLigoPath :: FilePath
  , scOctezClientPath :: Maybe FilePath
  , scPort :: Int
  , scVerbosity :: Int
  , scDockerizedLigoVersion :: Maybe String
  , scGistToken :: String
  , scLSPWorkspacePrefix :: Text
  , scLSPClientCounter :: IORef Int
  }

type ConnectionId = Int

data ConnectionConfig = ConnectionConfig
  { ccId :: ConnectionId
  , ccServerConfig :: ServerConfig
  }

readConfig :: IO ServerConfig
readConfig = do
  clientCounter <- newIORef 0
  customExecParser (prefs showHelpOnError) (mkParserInfo clientCounter)

mkParserInfo :: IORef Int -> ParserInfo ServerConfig
mkParserInfo clientCounter =
  info
    (helper <*> parser)
    (fullDesc
     <> header "LIGO WebIDE Backend"
     <> progDesc "Provide an interface to the LIGO LSP for the WebIDE frontend"
    )
   where
    parser = ServerConfig
      <$> strOption
          ( long "ligo-path"
            <> short 'l'
            <> metavar "STRING"
            <> help "path to ligo binary"
          )
      <*> optional (strOption
          ( long "octez-client-path"
            <> short 't'
            <> metavar "STRING"
            <> help "path to octez-client binary"
          ))
      <*> option auto
          ( long "port"
            <> short 'p'
            <> metavar "INT"
            <> showDefault
            <> value 8080
            <> help "port the server should use"
          )
      <*> option auto
          ( long "verbosity"
            <> short 'v'
            <> showDefault
            <> value 0
            <> help "verbosity level (max 2)"
          )
      <*> optional (strOption
          ( long "dockerized-ligo-version"
            <> short 'd'
            <> metavar "LIGO_VERSION"
            <> help "use a LIGO from Docker instead of a \
                    \LIGO binary. If this is specified, 'ligo-path' \
                    \will be ignored."
          ))
      <*> option str
          ( long "gist-token"
            <> short 'g'
            <> metavar "STRING"
            <> help "print received requests and the responses"
          )
      <*> (normaliseWorkspacePrefix <$> strOption
          ( long "workspace-prefix"
            <> short 'w'
            <> metavar "STRING"
            <> help "folder to store files from the client"
          ))
      <*> pure clientCounter

-- Ensure there is exactly one slash at the end of the workspacePrefix
normaliseWorkspacePrefix :: Text -> Text
normaliseWorkspacePrefix =
  Text.reverse
  >>> Text.dropWhile (== '/')
  >>> Text.cons '/'
  >>> Text.reverse
