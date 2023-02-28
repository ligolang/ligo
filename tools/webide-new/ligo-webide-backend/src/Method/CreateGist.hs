module Method.CreateGist (createGist) where

import Data.Text qualified as Text

import Data.Aeson (ToJSON(..), Value(..), encode, object, (.=))
import Data.ByteString qualified as BS
import GitHub
  (Auth(..), CommandMethod(..), Gist(..), NewGist(..), NewGistFile(..), RW(..),
  Request, command, createGistR, github, untagName)
import GitHub.Internal.Prelude (Binary, genericRnf)

import Common (WebIDEM)
import Config (scGistToken)
import Error (GistError(..))
import Schema.GistCreateUpdateRequest (GistCreateUpdateRequest(..))
import Source (Source(..), SourceFile(..))

data UpdateGist = UpdateGist
    { ugGistDescription :: !(Maybe Text)
    , ugGistFiles       :: !(HashMap Text NewGistFile)
    , ugGistId          :: !Text
    } deriving stock (Show, Eq, Generic)

instance NFData UpdateGist where rnf = genericRnf
instance Binary UpdateGist

instance ToJSON UpdateGist where
  toJSON UpdateGist
    { ugGistDescription = description
    , ugGistFiles       = files
    , ugGistId      = gistId
    } = object $ filter notNull
          [ "description" .= description
          , "files"       .= files
          , "gist_id"     .= gistId
          ]
    where
      notNull (_, Null) = False
      notNull (_, _)    = True

updateGistR :: UpdateGist -> Request 'RW Gist
updateGistR ugist = command Post ["gists", ugGistId ugist] (encode ugist)

createGist :: GistCreateUpdateRequest -> WebIDEM Text
createGist request = do
  dockerizedLigo <- lift $ asks scGistToken
  let files = fromList $ flip map (gcuSourceFiles request) $
        \s -> (Text.pack $ sfFilePath s, NewGistFile $ unSource $ sfSource s)
  result <- liftIO $ github (OAuth (encodeUtf8 dockerizedLigo :: BS.ByteString)) $ case gcuGistId request of
    Just id' -> updateGistR $ UpdateGist
      { ugGistDescription = gcuDescription request
      , ugGistFiles = files
      , ugGistId = id'
      }
    Nothing -> createGistR $ NewGist
      { newGistDescription = gcuDescription request
      , newGistFiles = files
      , newGistPublic = Just False
      }
  case result of
    Right gist -> pure $ untagName $ gistId gist
    Left err -> throwM $ GistError $ Text.pack $ show err

