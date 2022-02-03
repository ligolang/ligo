module Language.LIGO.Debugger.Cli
  ( LigoMapper (..)
  , LigoPosition (..)
  , LigoVariable (..)
  , LigoRange (..)
  , LigoType (..)
  , LigoMichelson (..)
  , LigoIndexedLocations (..)
  , LigoIndexedEnvironment (..)
  , LigoIndexedVariable (..)
  ) where

import Data.Vector.Unboxed qualified as UV
import Morley.Micheline.Expression (Expression)

data LigoMapper = LigoMapper
  { lmVariables :: [LigoVariable]
  , lmTypes :: [LigoType]
  , lmMichelson :: LigoMichelson
  }

data LigoPosition = LigoPosition
  { lpFile :: FilePath
  , lpLine :: Word
  , lpCol :: Word
  }

data LigoVariable = LigoVariable
  { lvName :: Text
  , lvRange :: Maybe LigoRange
  , lvType :: Word
  }

data LigoRange = LigoRange
  { lrStart :: LigoPosition
  , lrStop :: LigoPosition
  }

newtype LigoType = LigoType
  { ltType :: Void  -- TODO
  }

data LigoMichelson = LigoMichelson
  { lmExpression :: Expression
  , lmLocations :: LigoIndexedLocations
  }

newtype LigoIndexedLocations = LigoIndexedLocations
  { lilEnvironment :: Vector LigoIndexedEnvironment
  }

newtype LigoIndexedEnvironment = LigoIndexedEnvironment
  { lieLocation :: UV.Vector LigoIndexedVariable
  }

newtype LigoIndexedVariable = LigoIndexedVariable
  { livVariablePointer :: Word
  }
