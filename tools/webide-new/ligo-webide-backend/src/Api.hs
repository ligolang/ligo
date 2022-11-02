module Api (API, SwaggeredAPI) where

import Servant (JSON, Post, ReqBody, (:<|>), (:>))
import Servant.Swagger.UI (SwaggerSchemaUI)

import Schema.CompileExpressionRequest (CompileExpressionRequest)
import Schema.CompileRequest (CompileRequest)
import Schema.CompilerResponse (CompilerResponse)
import Schema.DeployScript (DeployScript)
import Schema.DryRunRequest (DryRunRequest)
import Schema.GenerateDeployScriptRequest (GenerateDeployScriptRequest)
import Schema.ListDeclarationsRequest (ListDeclarationsRequest)
import Schema.ListDeclarationsResponse (ListDeclarationsResponse)

type API =
       "compile" :> ReqBody '[JSON] CompileRequest :> Post '[JSON] CompilerResponse
  :<|> "generate-deploy-script" :> ReqBody '[JSON] GenerateDeployScriptRequest :> Post '[JSON] DeployScript
  :<|> "compile-expression" :> ReqBody '[JSON] CompileExpressionRequest :> Post '[JSON] CompilerResponse
  :<|> "dry-run" :> ReqBody '[JSON] DryRunRequest :> Post '[JSON] CompilerResponse
  :<|> "list-declarations" :> ReqBody '[JSON] ListDeclarationsRequest :> Post '[JSON] ListDeclarationsResponse

type SwaggeredAPI =
  SwaggerSchemaUI "swagger-ui" "openapi.json"
    :<|> API
