-- Deriving `ToGraph` creates some reduntant constraints warnings which we
-- unforunately have no control over. Disable this warning for now.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.LIGO.Debugger.Util.AST.Common
  ( ParsedContract (..)
  , FindFilepath (..)

  , ContractInfo
  , ParsedContractInfo

  , pattern FindContract

  , contractFile
  , contractTree
  , contractMsgs

  , addLigoErrsToMsg

  , cFile
  , cTree
  , cMsgs
  , getContract
  ) where

import Prelude hiding (Element, Product)

import Control.Lens (makeLenses)

import Duplo.Pretty

import Language.LIGO.Debugger.Util.AST.Skeleton
import Language.LIGO.Debugger.Util.Diagnostic (Message)
import Language.LIGO.Debugger.Util.ParseTree
import Language.LIGO.Debugger.Util.Parser (Info, ParsedInfo)

-- TODO: Many of these datatypes don't make sense to be defined here. Consider
-- moving into different or new modules.
data ParsedContract info = ParsedContract
  { _cFile :: Source -- ^ The path to the contract.
  , _cTree :: info -- ^ The payload of the contract.
  , _cMsgs :: [Message] -- ^ Messages produced by this contract.
  } deriving stock (Show)
    deriving Pretty via ShowPP (ParsedContract info)

-- | Wraps a 'ParsedContract', allowing it to be stored in a container where its
-- comparison will always be on its source file.
newtype FindFilepath info
  = FindFilepath { _getContract :: ParsedContract info }
  deriving stock (Show)
  deriving newtype (Pretty)

instance Eq (FindFilepath info) where
  (==) = (==) `on` contractFile

instance Ord (FindFilepath info) where
  compare = compare `on` contractFile

contractFile :: FindFilepath info -> FilePath
contractFile (FindFilepath pc) = srcPath $ _cFile pc

contractTree :: FindFilepath info -> info
contractTree (FindFilepath pc) = _cTree pc

contractMsgs :: FindFilepath info -> [Message]
contractMsgs (FindFilepath pc) = _cMsgs pc

makeLenses ''ParsedContract
makeLenses ''FindFilepath

addLigoErrsToMsg :: [Message] -> FindFilepath info -> FindFilepath info
addLigoErrsToMsg errs = getContract . cMsgs %~ (errs <>)

pattern FindContract :: Source -> info -> [Message] -> FindFilepath info
pattern FindContract f t m = FindFilepath (ParsedContract f t m)
{-# COMPLETE FindContract #-}

type ContractInfo       = FindFilepath (SomeLIGO Info)
type ParsedContractInfo = FindFilepath (SomeLIGO ParsedInfo)
