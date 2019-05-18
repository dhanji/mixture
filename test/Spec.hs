import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.Hspec
import Mix.Data.Cell
import qualified Mix
import Mix.Data (rA)
import qualified Fixture

import GHC.IO (unsafePerformIO)


main :: IO ()
main = defaultMain $ testGroup "\nMix Test Suite" (properties : specs)


properties :: TestTree
properties = testGroup "QuickCheck: Data.Cell" [
    testProperty "Roundtrip: Int -> toCell -> toInt" $
      \n -> (toInt . toCell) n == (n :: Int)

  , testProperty "addCells result equals integer sum" $
      \(n, m) -> toInt (addCells (toCell n) (toCell m)) == (n :: Int) + (m :: Int)
  ]


specs :: [TestTree]
specs = map asSpec [
    Fixture.addFieldParts
  , Fixture.subFieldParts
  ]


asSpec :: Fixture.Specification -> TestTree
asSpec (specifies, assembly, expected) = specify $ it specifies (accumulator `shouldBe` expected)
  where
    specify     = unsafePerformIO . testSpec ""
    accumulator = toInt $ rA (Mix.executeProgram newMix assembly)
