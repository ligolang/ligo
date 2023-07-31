module Main
  ( main
  ) where

import Morley.Util.Main (wrapMain)

import Protocol.DAP (compileHandlers)
import Protocol.DAP.Serve.IO
  (ServingRequestsOptions (..), defaultRequestsServingOptions, servingRequestsIO)

import Language.LIGO.Debugger.Handlers.Impl (ligoHandlers)
import Morley.Debugger.DAP.RIO (logMessage)
import Morley.Debugger.DAP.Types (newRioContext)

main :: IO ()
main = wrapMain $ do
  ctx <- newRioContext
  usingReaderT ctx $ servingRequestsIO defaultRequestsServingOptions
    { log = usingReaderT ctx . logMessage
    }
    \stopAdapter -> compileHandlers (ligoHandlers stopAdapter)
