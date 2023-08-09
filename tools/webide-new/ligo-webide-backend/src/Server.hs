{-# LANGUAGE PolyKinds #-}

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
import Servant
  (Application, Context(..), Handler(..), Server, hoistServer, serveWithContext, (:<|>)((:<|>)))
import Servant.Swagger.UI (swaggerSchemaUIServer)

import Api (API, SwaggeredAPI)
import Common (WebIDEM)
import Config (ServerConfig(..))
import Error (LigoCompilerError, MorleyError, convertToServerError, customFormatters)
import Method.Compile (compile)
import Method.RunTest (runTest)
import Method.CompileExpression (compileExpression)
import Method.CreateGist (createGist)
import Method.DryRun (dryRun)
import Method.GenerateDeployScript (generateDeployScript)
import Method.LigoVersion (ligoVersion)
import Method.ListDeclarations (listDeclarations)
import Method.ListTemplates (listTemplates)
import SwaggerSchema (webIdeOpenApi)

startApp :: ServerConfig -> IO ()
startApp config = run (scPort config) (mkApp config)

mkApp :: ServerConfig -> Application
mkApp config =
  maybeLogRequests . corsWithContentType $ serveWithContext (Proxy @SwaggeredAPI) (customFormatters :. EmptyContext) server
  where
    maybeLogRequests :: Middleware
    maybeLogRequests =
      if scVerbosity config >= 1
      then logStdoutDev
      else id

    -- Allow Content-Type header with values other then allowed by simpleCors.
    corsWithContentType :: Middleware
    corsWithContentType = cors (const $ Just policy)
      where
        policy = simpleCorsResourcePolicy
          {corsRequestHeaders = ["Content-Type"]}

    server :: Server SwaggeredAPI
    server = swaggerSchemaUIServer webIdeOpenApi :<|> hoistServer (Proxy @API) hoist
      (    compile
      :<|> runTest
      :<|> generateDeployScript
      :<|> compileExpression
      :<|> dryRun
      :<|> listDeclarations
      :<|> createGist
      :<|> listTemplates
      :<|> ligoVersion
      )

    hoist :: WebIDEM a -> Handler a
    hoist x = convertToServerError @'[LigoCompilerError, MorleyError, SomeException] $ Handler $ do
      logEnv <- liftIO $ initLogEnv "ligo-webide" (Environment "devel")
      runReaderT (runKatipT logEnv x) config
