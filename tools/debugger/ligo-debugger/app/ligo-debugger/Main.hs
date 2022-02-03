module Main
  ( main
  ) where

import Morley.Debugger.DAP.IO (withDebuggerMain)
import Morley.Util.Main (wrapMain)

import Language.LIGO.Debugger.Handlers (LIGO)

main :: IO ()
main = wrapMain $ withDebuggerMain @LIGO
