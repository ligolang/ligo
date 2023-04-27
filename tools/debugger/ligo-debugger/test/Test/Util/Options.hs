module Test.Util.Options
  ( reinsuring
  , minor
  , ligoIngredient
  ) where

import Prelude hiding (Option)

import Data.Coerce (coerce)

import Test.Tasty (TestTree, askOption, includingOptions)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.Ingredients (Ingredient)
import Test.Tasty.Options (IsOption (..), OptionDescription (Option), flagCLParser, safeReadBool)
import Test.Tasty.Runners (TestTree (..))

newtype Reinsuring = Reinsuring Bool

instance IsOption Reinsuring where
  defaultValue = Reinsuring False
  parseValue = fmap Reinsuring . safeReadBool
  optionName = pure "reinsuring"
  optionHelp = pure "Run 'reinsuring' tests"
  optionCLParser = flagCLParser (Just 'r') (Reinsuring True)

newtype Minor = Minor Bool

instance IsOption Minor where
  defaultValue = Minor True
  parseValue = fmap Minor . safeReadBool
  optionName = pure "minor"
  optionHelp = pure "Do not run 'minor' tests"
  optionCLParser = flagCLParser (Just 'm') (Minor False)

-- | A helper function for @Bool@ newtypes options.
-- It runs the test if only the corresponding option is @True@.
conditionalRun :: forall a. (Coercible a Bool, IsOption a) => TestTree -> TestTree
conditionalRun tree = askOption @a \(coerce -> run) ->
  if run
  then tree
  else replaceIsTestWithDummy tree
  where
    -- We can't do this with @syb@ because there is not @Data@ instance
    -- for @TestTree@.
    replaceIsTestWithDummy :: TestTree -> TestTree
    replaceIsTestWithDummy = \case
      SingleTest name _ -> testCase (name <> " <ignored>") pass
      TestGroup name tests -> TestGroup name (replaceIsTestWithDummy <$> tests)
      PlusTestOptions f testTree -> PlusTestOptions f (replaceIsTestWithDummy testTree)
      WithResource rs f -> WithResource rs (replaceIsTestWithDummy . f)
      AskOptions f -> AskOptions (replaceIsTestWithDummy . f)
      After depType expr testTree -> After depType expr (replaceIsTestWithDummy testTree)

-- | Mark a test as @reinsuring@. We use it for tests
-- that are long to run but unlikely to fail.
-- For e.g. tests that effectively check solely @ligo@
-- executable's functionality can be marked as @reinsuring@
-- (see @Contracts are sensible@ test in @Snapshots@ module).
--
-- They are not running by default. Use @-r@ flag
-- to enable them.
reinsuring :: TestTree -> TestTree
reinsuring = conditionalRun @Reinsuring

-- | Mark a test as @minor@. We use it for tests
-- that can fail, assuming we provide partial support
-- of the given @ligo@ version.
--
-- They are running by default. Use @-m@ flag
-- to disable them.
minor :: TestTree -> TestTree
minor = conditionalRun @Minor

ligoIngredient :: Ingredient
ligoIngredient = includingOptions
  [ Option (Proxy @Reinsuring)
  , Option (Proxy @Minor)
  ]
