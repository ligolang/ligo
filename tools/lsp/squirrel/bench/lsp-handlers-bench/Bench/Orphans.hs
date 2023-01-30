{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Bench.Orphans () where

import Language.LSP.Types

deriving stock instance Generic Hover
instance NFData Hover

deriving stock instance Generic HoverContents
instance NFData HoverContents

deriving stock instance Generic MarkedString
instance NFData MarkedString

deriving stock instance Generic MarkupContent
instance NFData MarkupContent

deriving stock instance Generic LanguageString
instance NFData LanguageString

deriving stock instance Generic MarkupKind
instance NFData MarkupKind

type instance PrettyShow (UInt, UInt) = ()
