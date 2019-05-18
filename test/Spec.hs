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


asSpec :: String -> Fixture.Specification -> TestTree
asSpec group (specifies, asm, expected) = specify $ it specifies (accumulator `shouldBe` expected)
  where
    specify     = unsafePerformIO . testSpec group
    accumulator = toInt $ rA (Mix.executeProgram newMix asm)


-- QuickCheck tests
properties :: TestTree
properties = testGroup "QuickCheck: Data.Cell" [
    testProperty "Roundtrip: Int -> toCell -> toInt" $
      \n -> (toInt . toCell) n == (n :: Int)

  , testProperty "addCells result equals integer sum" $
      \(n, m) -> toInt (addCells (toCell n) (toCell m)) == (n :: Int) + (m :: Int)
  ]


-- HSpec tests (detailed in Fixture)
specs :: [TestTree]
specs = map (asSpec "ADD") [
    Fixture.addSingleFieldPart
  , Fixture.addDoublesCell
  , Fixture.addDoublesCellNoFieldSpec
  ]
  ++ map (asSpec "SUB") [
    Fixture.subtractFieldParts
  ]
