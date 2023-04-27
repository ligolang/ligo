module Test.DelayedValues
  ( module Test.DelayedValues
  ) where

import Test.Tasty (TestTree)
import Test.Util
import UnliftIO.Exception (StringException (..), throwString)

import Control.DelayedValues

test_Test :: [TestTree]
test_Test =
  [ testCase "Simple case" do
      manager <- newManager @Int (pure . map (* 2))
      compute manager 1
        @@?= Nothing
      compute manager 3
        @@?= Nothing
      runPendingComputations manager
        @@?= True
      compute manager 1
        @@?= Just 2
      compute manager 3
        @@?= Just 6
      runPendingComputations manager
        @@?= False

  , testCase "Requests only for non-cached values" do
      lastComputed <- newIORef []
      manager <- newManager @Int \inps -> do
        writeIORef lastComputed inps
        return $ map (* 2) inps

      compute manager 1
      compute manager 2
      compute manager 3
      runPendingComputations manager
        @@?= True
      readIORef lastComputed >>= (@~=? [1, 2, 3])

      compute manager 4
      compute manager 2
      runPendingComputations manager
        @@?= True
      readIORef lastComputed
        @@?= [4]

      writeIORef lastComputed []
      compute manager 3
      compute manager 4
      runPendingComputations manager
        @@?= False
      readIORef lastComputed
        @@?= []

  , testCase "In case of exceptions" do
      manager <- newManager @Int $ mapM \inp ->
        if even inp
        then return $ inp `div` 2
        else throwString "odd"

      compute manager 2
      compute manager 3
      compute manager 4
      void (runPendingComputations manager)
        `catch` \(StringException msg _) -> msg @?= "odd"

      -- For now, after exception all the pending values are thrown away.
      -- This is not ideal, but better then leaving them in place and
      -- causing all the future computations fail too.
      runPendingComputations manager
        @@?= False

  ]
