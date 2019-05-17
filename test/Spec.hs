import Test.Tasty
import Test.Tasty.QuickCheck
import Mix.Data.Cell

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "\nMix Test Suite" [properties]

properties :: TestTree
properties = testGroup "QuickCheck: Data.Cell" [
    testProperty "Roundtrip: Int -> toCell -> toInt" $
      \n -> (toInt . toCell) n == (n :: Int)

  , testProperty "addCells result equals integer sum" $
      \(n, m) -> toInt (addCells (toCell n) (toCell m)) == (n :: Int) + (m :: Int)
  ]
