-- | Utilities for working with LSP.
module Language.LSP.Util
  ( MessageDescription (..)
  , describeFromClientMessage
  ) where

import Control.Lens ((^.))
import Data.Text (Text, pack)
import qualified GHC.Generics as G
import qualified Language.LSP.Messages as J
import qualified Language.LSP.Types  as J
import qualified Language.LSP.Types.Lens as J


-- | A short description of a message.
data MessageDescription
  = MessageRequest Text J.LspId
  | MessageResponse Text J.LspIdRsp
  | MessageNotification Text
  deriving (Eq, Show)

-- | Describe an LSP @FromClientMessage@ using 'MessageDescription'.
describeFromClientMessage :: J.FromClientMessage -> MessageDescription
describeFromClientMessage = describeMessage . G.from

class DescribeMessage t where
  describeMessage :: t x -> MessageDescription

class DescribeMessageWithMethodName t where
  describeMessageWithMethodName :: String -> t x -> MessageDescription

instance DescribeMessage f => DescribeMessage (G.M1 G.D c f) where
  describeMessage (G.M1 x) = describeMessage x

instance (DescribeMessage f, DescribeMessage g) => DescribeMessage ((G.:+:) f g) where
  describeMessage (G.L1 x) = describeMessage x
  describeMessage (G.R1 x) = describeMessage x

instance (G.Constructor c, DescribeMessageWithMethodName f) => DescribeMessage (G.M1 G.C c f) where
  describeMessage c@(G.M1 x) = describeMessageWithMethodName (G.conName c) x

instance DescribeMessageWithMethodName f => DescribeMessageWithMethodName (G.M1 G.S c f) where
  describeMessageWithMethodName method (G.M1 x) = describeMessageWithMethodName method x

instance DescribeMessageWithMethodName (G.K1 G.R (J.RequestMessage J.ClientMethod req resp)) where
  describeMessageWithMethodName method (G.K1 req) = MessageRequest (pack method) (req^.J.id)

instance DescribeMessageWithMethodName (G.K1 G.R (J.ResponseMessage a)) where
  describeMessageWithMethodName method (G.K1 req) = MessageResponse (pack method) (req^.J.id)

instance DescribeMessageWithMethodName (G.K1 G.R (J.NotificationMessage J.ClientMethod a)) where
  describeMessageWithMethodName method _ = MessageNotification (pack method)
