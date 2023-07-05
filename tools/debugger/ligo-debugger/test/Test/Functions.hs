-- | Unit tests on functionality related to LIGO lambdas.
module Test.Functions
  ( module Test.Functions
  ) where

import Test.Tasty (TestTree, testGroup)

import Language.LIGO.Debugger.CLI.Helpers
import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.Functions

import Test.Util

test_LambdaMeta :: TestTree
test_LambdaMeta = testGroup "functionality around LambdaMeta"
  [ testGroup "lmGroupByName"
    let mkLambdaNamedInfo name =
          LambdaNamedInfo @'Concise name (LigoTypeResolved $ intType' ~> intType')
        mkLambdaArg (i :: Integer) =
          LambdaArg (SomeLorentzValue i) (LigoTypeResolved intType')
    in
    [ testCase "Simple partial application" $
        lmGroupByName LambdaMeta
          { lmEvents = reverse
              -- further listing events in chronological order
              [ LambdaNamed $ mkLambdaNamedInfo "a"
              , LambdaApplied $ mkLambdaArg 1
              ]
          }
        @?=
          [ (mkLambdaNamedInfo "a", one $ mkLambdaArg 1) ]

    , testCase "Arguments applied before the first naming event are ignored" $
        lmGroupByName LambdaMeta
          { lmEvents = reverse
              [ LambdaApplied $ mkLambdaArg 1
              , LambdaApplied $ mkLambdaArg 2
              , LambdaNamed $ mkLambdaNamedInfo "a"
              , LambdaApplied $ mkLambdaArg 3
              ]
          }
        @?=
          [ (mkLambdaNamedInfo "a", one $ mkLambdaArg 3) ]

    , testCase "No events" $
        lmGroupByName @'Concise LambdaMeta
          { lmEvents = []
          }
        @?=
          []

    , testCase "Only argument application event" $
        lmGroupByName @'Concise LambdaMeta
          { lmEvents = [ LambdaApplied $ mkLambdaArg 0 ]
          }
        @?=
          []

    , testCase "Trailing naming event" $
        lmGroupByName LambdaMeta
          { lmEvents = reverse
              [ LambdaNamed $ mkLambdaNamedInfo "a"
              , LambdaApplied $ mkLambdaArg 1
              , LambdaNamed $ mkLambdaNamedInfo "b"
              ]
          }
        @?= reverse
          -- also listed in chronological order
          [ (mkLambdaNamedInfo "a", [mkLambdaArg 1])
          , (mkLambdaNamedInfo "b", [])
          ]

    , testCase "Two adjacent namings are preserved" $
        lmGroupByName LambdaMeta
          { lmEvents = reverse
              [ LambdaNamed $ mkLambdaNamedInfo "a"
              , LambdaNamed $ mkLambdaNamedInfo "b"
              , LambdaApplied $ mkLambdaArg 1
              ]
          }
        @?= reverse
          [ (mkLambdaNamedInfo "a", [])
          , (mkLambdaNamedInfo "b", [mkLambdaArg 1])
          ]

    , testCase "Complex case" $
        lmGroupByName LambdaMeta
          { lmEvents = reverse
              [ LambdaApplied $ mkLambdaArg 1
              , LambdaNamed $ mkLambdaNamedInfo "a"
              , LambdaApplied $ mkLambdaArg 2
              , LambdaNamed $ mkLambdaNamedInfo "b"
              , LambdaApplied $ mkLambdaArg 3
              , LambdaApplied $ mkLambdaArg 4
              , LambdaNamed $ mkLambdaNamedInfo "c"
              , LambdaNamed $ mkLambdaNamedInfo "d"
              , LambdaApplied $ mkLambdaArg 5
              ]
          }
        @?= reverse
          [ (mkLambdaNamedInfo "a", [mkLambdaArg 2])
          , (mkLambdaNamedInfo "b", [mkLambdaArg 3, mkLambdaArg 4])
          , (mkLambdaNamedInfo "c", [])
          , (mkLambdaNamedInfo "d", [mkLambdaArg 5])
          ]

    ]

  ]
