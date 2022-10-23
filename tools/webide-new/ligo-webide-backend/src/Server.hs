module Server
  ( startApp
  , mkApp
  )
where

import Katip (Environment(..), initLogEnv, runKatipT)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (Application, Handler(..), Server, hoistServer, serve, (:<|>)((:<|>)))
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (swaggerSchemaUIServer)

import Api (API, SwaggeredAPI)
import Common (WebIDEM)
import Config (Config(..))
import Method.Compile (compile)
import Method.CompileExpression (compileExpression)
import Method.DryRun (dryRun)
import Method.GenerateDeployScript (generateDeployScript)
import Method.ListDeclarations (listDeclarations)

startApp :: Config -> IO ()
startApp config = run (cPort config) (mkApp config)

mkApp :: Config -> Application
mkApp config =
  maybeLogRequests . corsWithContentType $ serve (Proxy @SwaggeredAPI) server
  where
    maybeLogRequests :: Middleware
    maybeLogRequests =
      if cVerbose config
      then logStdoutDev
      else id

    -- Allow Content-Type header with values other then allowed by simpleCors.
    corsWithContentType :: Middleware
    corsWithContentType = cors (const $ Just policy)
      where
        policy = simpleCorsResourcePolicy
          {corsRequestHeaders = ["Content-Type"]}

    server :: Server SwaggeredAPI
    server =
      swaggerSchemaUIServer (toSwagger (Proxy @API))
        :<|> hoistServer (Proxy @API) hoist (compile :<|> generateDeployScript :<|> compileExpression :<|> dryRun :<|> listDeclarations)

    hoist :: WebIDEM a -> Handler a
    hoist x = Handler $ do
      logEnv <- liftIO $ initLogEnv "ligo-webide" (Environment "devel")
      runReaderT (runKatipT logEnv x) config
