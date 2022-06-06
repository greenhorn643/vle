import Test.Tasty
import Test.Tasty.Hspec
import qualified Test.VLE as VLE

main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs
    [ VLE.specs
    ]
  defaultMain (testGroup "All Tests" [
      testGroup "Specs" specs
    ])