{-# OPTIONS_GHC -Wno-orphans #-}

-- | Tests on navigation through the snapshots and
-- responding to stepping commands.
module Test.Navigation
  ( module Test.Navigation
  ) where

import Hedgehog (Gen, discard, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Util
import Text.Interpolation.Nyan

import Morley.Debugger.Core
  (Direction (..), MovementResult (..), NavigableSnapshot (getExecutedPosition), frozen,
  getCurMethodBlockLevel, getFutureSnapshotsNum, move)
import Morley.Debugger.DAP.Types (StepCommand (..))

import Language.LIGO.Debugger.Navigate

-- TODO: move these to Morley

deriving stock instance Show Direction

genDirection :: Gen Direction
genDirection = Gen.frequency [(7, pure Forward), (3, pure Backward)]

reverseDirection :: Direction -> Direction
reverseDirection = \case
  Forward -> Backward
  Backward -> Forward

test_StepBackReversed :: IO TestTree
test_StepBackReversed = fmap (testGroup "Step back is the opposite to Next") $
  [ ContractRunData
    { crdProgram = contractsDir </> "simple-ops.mligo"
    , crdEntrypoint = Nothing
    , crdParam = ()
    , crdStorage = 0 :: Integer
    }
  , ContractRunData
    { crdProgram = contractsDir </> "functions-assignments.mligo"
    , crdEntrypoint = Nothing
    , crdParam = ()
    , crdStorage = 0 :: Integer
    }
  , ContractRunData
    { crdProgram = contractsDir </> "module_contracts" </> "importer.mligo"
    , crdEntrypoint = Nothing
    , crdParam = ()
    , crdStorage = 0 :: Integer
    }

  ] `forM` \runData -> do
    locsAndHis <- liftIO $ mkSnapshotsForImpl dummyLoggingFunction runData
    return $ testProperty [int||On example of "#{crdProgram runData}"|] $
      property $ withSnapshots locsAndHis do
        granularity <- pure ()  -- will be changed soon

        let liftProp = lift . lift

        -- Jump to some position in the middle of execution
        do
          futureSnapshotsNum <- frozen getFutureSnapshotsNum
          stepsNum <- liftProp . forAll $
            Gen.integral (Range.constant 0 futureSnapshotsNum)
          replicateM_ stepsNum $ move Forward

        startPos <- frozen getExecutedPosition
        startMethodLevel <- frozen getCurMethodBlockLevel

        dir <- liftProp $ forAll genDirection

        -- Do a step over...
        do
          moveRes <- processLigoStep (CNext dir granularity)
          unless (moveRes == MovedSuccessfully) $
            -- We started at the end of the tape, this case is not interesting
            liftProp discard

        endMethodLevel <- frozen getCurMethodBlockLevel
        unless (startMethodLevel == endMethodLevel) $
          -- Exited current method, step back won't return to the old position
          liftProp discard

        -- ...and then a step in the opposite direction
        processLigoStep (CNext (reverseDirection dir) granularity)

        -- Check this all resulted in no-op
        endPos <- frozen getExecutedPosition
        liftProp $ startPos === endPos
