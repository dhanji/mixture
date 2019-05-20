import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.Hspec
import Mix.Data.Cell
import Mix.Data (rA)

import qualified Mix
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

  , testProperty "computeWith (+) results in integer sum" $
      \(n, m) -> toInt (computeWith (+) (toCell n) (toCell m)) == (n :: Int) + (m :: Int)

  , testProperty "computeWith (-) results in integer difference" $
      \(n, m) -> toInt (computeWith (-) (toCell n) (toCell m)) == (n :: Int) - (m :: Int)

  , testProperty "computeWith (*) results in integer product" $
      \(n, m) -> toInt (computeWith (*) (toCell n) (toCell m)) == (n :: Int) * (m :: Int)

  , testProperty "computeWith (div) results in integer division" $
      \(n, m) -> m /= 0
              ==> toInt (computeWith div (toCell n) (toCell m)) == (n :: Int) `div` (m :: Int)
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
  ++ map (asSpec "MUL") [
    Fixture.mulDoublesCell
  ]
