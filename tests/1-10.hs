import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import Utils

-- List of imports source: https://stackoverflow.com/questions/20331209/haskell-unit-testing

test1BaseCase = TestCase (assertEqual "" "")