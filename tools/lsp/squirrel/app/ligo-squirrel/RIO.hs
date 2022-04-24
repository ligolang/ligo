module RIO
  ( module RIO.Types
  , newRioEnv
  , initializeRio
  , run
  ) where

import Algebra.Graph.Class qualified as G (empty)
import Control.Monad.Reader (runReaderT)
import Data.HashSet qualified as HashSet
import Language.LSP.Server qualified as S
import StmContainers.Map (newIO)
import UnliftIO.MVar (newEmptyMVar, newMVar)

import AST (Fallback)
import ASTMap qualified
import Config (Config (..))
import Log (LogT)
import RIO.Document qualified (load)
import RIO.Registration qualified
import RIO.Types (Contract (..), RIO (..), RioEnv (..))

newRioEnv :: IO RioEnv
newRioEnv = do
  reCache <- ASTMap.empty $ RIO.Document.load @Fallback
  reOpenDocs <- newMVar HashSet.empty
  reIncludes <- newMVar G.empty
  reTempFiles <- newIO
  reIndexOpts <- newEmptyMVar
  reBuildGraph <- newMVar G.empty
  pure RioEnv {..}

initializeRio :: RIO ()
initializeRio = do
  RIO.Registration.registerDidChangeConfiguration
  RIO.Registration.registerFileWatcher

run :: (S.LanguageContextEnv Config, RioEnv) -> RIO a -> LogT IO a
run (lcEnv, env) (RIO action) = S.runLspT lcEnv $ runReaderT action env
