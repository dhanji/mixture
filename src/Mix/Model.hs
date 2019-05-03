module Mix.Model where

import System.IO
import Data.Array
import Data.Char
import Data.String.Interpolate
import qualified Data.Text as Text
import qualified Data.List.Split as Split


type Byte = Int


data Sign = Plus | Minus

instance Show Sign where
  show Plus   = "+"
  show Minus  = "-"


-- a MIX cell is basically a memory location (including a register)
data Cell = Cell {
  -- bits in MIX are not well-defined,
  -- in our case we treat it as Int <10
  -- The first bit is always the sign bit
  bytes  :: [Byte]
}

instance Show Cell where
  show cell = [i|#{sign (bytes cell !! 0)}#{toBitString "" $ bytes cell}|]
    where
      toBitString :: String -> [Byte] -> String
      toBitString string []       = string
      toBitString string (b:bs)   = toBitString (string ++ (show b)) bs
      sign :: Byte -> String
      sign 0  = "+"
      sign _  = "-"


-- A range specified over a Word.
data FieldSpec = FieldSpec Int Int

instance Read FieldSpec where
  readsPrec _ ('(':l:':':h:")") =
    let low = ord l - ord '0'
        hi  = ord h - ord '0' in
      [(FieldSpec low hi, "")]
  readsPrec _ _                 = [(FieldSpec 0 5, "")]

-- Purely for symmetry with the Read instance
instance Show FieldSpec where
  show (FieldSpec low hi)  = [i|(#{low}:#{hi})|]


data Mix = Mix {
  -- registers
    rA :: Cell
  , rX :: Cell

  , memory  :: Array Int Cell
  , source :: [String]
}

instance Show Mix where
  show mix = [i|
Final MIX Machine state:

register A: #{show $ rA mix}
register X: #{show $ rX mix}

program:
|] ++ (foldl (++) "" [line ++ "\n" | line <- source mix])


data Op = Load | Store | Zero | Increment | Set deriving (Eq)


data Instruction = Comment | Blank | Instruction {
    op      :: Op
  -- the target register (if any), i.e. A1-5 or X1-5
  , target  :: Maybe String
  , address :: Int

  -- The index and field specifications I and F as per MIX assembly.
  , iSpec   :: Int
  , fSpec   :: FieldSpec
}


-- Constructs a blank cell suitable for use in registers or main memory.
newCell :: Cell
newCell = Cell { bytes = replicate 6 0 }


-- Constructs a blank Mix computer.
newMix :: Mix
newMix = Mix {
    rA = newCell
  , rX = newCell
    -- Mix computers have 4000 words of memory.
  , memory = array (1, 4000) [(i, newCell)| i <- [1..4000]]
  , source = []
  }


-- Returns a slice of the given bitstring according to the FieldSpec
-- Contents of source are copied to a new cell such that
-- any bits of target that are not covered by the F-spec
-- are "preserved" intact in the result.
copyCell :: FieldSpec -> Cell -> Cell -> Cell
copyCell (FieldSpec from to) Cell{bytes=source} Cell{bytes=target} = Cell {
  bytes = pad ++ slice
}
  where
    slice = take (to + 1 - from) $ drop from source
    pad   = take (6 - length slice) target


-- Converts an Int to a 6-word Byte string suitable for use in a cell.
toBytes :: Int -> [Byte]
toBytes i | i == 0    = replicate 6 0
          | i > 0     = bytize i
          | otherwise = 1:(bytize $ abs i)
  where
    bytize i = [(i `mod` 10^n) `div` 10^(n - 1) | n <- [6,5..1]]
