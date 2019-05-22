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
asSpec group (specifies, asm, expected) =
    specify $ it specifies (accumulator `shouldBe` expected)
  where
    specify     = unsafePerformIO . testSpec group
    accumulator = toInt $ rA (Mix.executeProgram newMix asm)


-- QuickCheck tests
properties :: TestTree
properties = testGroup "QuickCheck: Data.Cell" [
    testProperty "Roundtrip: Int -> toCell -> toInt" $
      \n -> (toInt . toCell) n == n

  , testProperty "computeWith (+) results in integer sum" $
      \(n, m) -> assertComputesWith (+) n m

  , testProperty "computeWith (-) results in integer difference" $
      \(n, m) -> assertComputesWith (-) n m

  , testProperty "computeWith (*) results in integer product" $
      \(n, m) -> assertComputesWith (*) n m

  , testProperty "computeWith (div) results in integer division" $
      \(n, m) -> m /= 0
              ==> assertComputesWith div n m
  ]
  where
    assertComputesWith fn n m = toInt (computeWith fn (toCell n) (toCell m)) == n `fn` m


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
    Fixture.mulByOne
  , Fixture.mulByZeroes
  , Fixture.mulDoublesCell
  , Fixture.mulByNegativeOne
  ]
  ++ map (asSpec "DIV") [
    Fixture.divByTwo
  , Fixture.divBySelf
  , Fixture.divByNegativeOne
  ]
