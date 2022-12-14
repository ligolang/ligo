module Config
  ( ServerConfig (..)
  , ConnectionId
  , ConnectionConfig (..)
  , readConfig
  ) where

import Options.Applicative

data ServerConfig = ServerConfig
  { scSquirrelPath :: FilePath
  , scPort :: Int
  , scHostname :: Text
  , scVerbose :: Bool
  , scWorkspacePrefix :: Text
  , scClientCounter :: IORef Int
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
     <> header "LIGO WebIDE LSP Shim"
     <> progDesc "Provide an interface to ligo-squirrel for the WebIDE frontend"
    )
  where
    parser = ServerConfig
      <$> strOption
          ( long "squirrel-path"
            <> short 's'
            <> metavar "STRING"
            <> help "path to ligo-squirrel binary"
          )
      <*> option auto
          ( long "port"
            <> short 'p'
            <> metavar "INT"
            <> showDefault
            <> value 3000
            <> help "port the server should use"
          )
      <*> strOption
          ( long "hostname"
            <> short 'h'
            <> metavar "STRING"
            <> showDefault
            <> value "localhost"
            <> help "server hostname"
          )
      <*> switch
          ( long "verbose"
            <> short 'v'
            <> help "print received requests and the responses"
          )
      <*> strOption
          ( long "workspace-prefix"
            <> short 'w'
            <> metavar "STRING"
            <> help "folder to store files from the client"
          )
      <*> pure clientCounter
